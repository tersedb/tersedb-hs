{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
  , DerivingVia
  , DataKinds
  , DeriveGeneric
  , RankNTypes
  , TemplateHaskell
  , FlexibleContexts
  #-}

module Lib.Actions.Safe where

import Lib.Actions.Tabulation
  ( updateTabulationStartingAt
  )
import Lib.Actions.Unsafe
  ( unsafeEmptyStore
  , unsafeStoreGroup
  , unsafeStoreActor
  , unsafeStoreSpace
  , unsafeStoreEntity
  , StoreVersionError
  , unsafeStoreVersion
  , unsafeAdjustUniversePermission
  , unsafeAdjustOrganizationPermission
  , unsafeAdjustRecruiterPermission
  , unsafeAdjustGroupPermission
  , unsafeAdjustSpacePermission
  , unsafeAdjustEntityPermission
  , unsafeAdjustMemberPermission
  , unsafeAddMember
  )
import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption (..)
  , SinglePermission (..)
  , escalate
  , collectionPermission
  )
import Lib.Types.Store
  ( Store (..)
  , toActors
  , toEntities
  , toVersions
  , toTabulatedPermissions
  )
import Lib.Types.Store.Tabulation.Group
  ( TabulatedPermissionsForGroup (..)
  , forUniverse
  , forOrganization
  , forRecruiter
  , forSpaces
  , forEntities
  , forGroups
  , forMembers
  )
import Lib.Types.Store.Entity (space, versions)
import Lib.Types.Store.Version (genesisVersion, forkVersion, entity)

import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Control.Lens ((^.), at, non, _Left, (%~), (&))
import Control.Monad.State (MonadState (get), execState)
import Control.Monad.Extra (andM, orM, when)



emptyStore :: ActorId -> GroupId -> Store
emptyStore adminActor adminGroup = flip execState unsafeEmptyStore $ do
  unsafeStoreGroup adminGroup
  unsafeAdjustUniversePermission (const $ CollectionPermissionWithExemption Delete True) adminGroup
  unsafeAdjustOrganizationPermission (const $ CollectionPermissionWithExemption Delete True) adminGroup
  unsafeAdjustRecruiterPermission (const Delete) adminGroup
  unsafeStoreActor adminActor
  unsafeAddMember adminGroup adminActor
  updateTabulationStartingAt adminGroup

-- | Looks first for the groups the user is in, then sees if any of the groups
-- can do the action, depicted by the Lens
canDoWithTab
  :: ( MonadState Store m
     , Ord a
     ) => (TabulatedPermissionsForGroup -> a) -- ^ Specific permission being checked
       -> ActorId -- ^ Actor requesting permission
       -> (TabulatedPermissionsForGroup -> a) -- ^ Minimum permission actor needs
       -> m Bool
canDoWithTab proj creator getP = do
  s <- get
  pure $ case s ^. toActors . at creator of
    Just groups ->
      let perGroup gId =
            let tab = s ^. toTabulatedPermissions . at gId . non mempty
            in  proj tab >= getP tab
      in  any perGroup (HS.toList groups)
    _ -> False

canDo
  :: ( MonadState Store m
     , Ord a
     ) => (TabulatedPermissionsForGroup -> a) -- ^ Specific permission being checked
       -> ActorId -- ^ Actor requesting permission
       -> a -- ^ Minimum permission actor needs
       -> m Bool
canDo a b c = canDoWithTab a b (const c)

canUpdateGroup :: MonadState Store m => ActorId -> GroupId -> m Bool
canUpdateGroup creator gId = orM
  [ canDo (\t -> t ^. forGroups . at gId . non Blind) creator Update
  , canDo (\t -> t ^. forOrganization . collectionPermission) creator Update
  ]

conditionally :: Applicative m => m () -> Bool -> m Bool
conditionally f t = t <$ when t f

storeGroup
  :: MonadState Store m
  => ActorId -- ^ actor storing the group
  -> GroupId -- ^ group being stored
  -> m Bool
storeGroup creator gId = do
  canDo (\t -> t ^. forOrganization . collectionPermission) creator Create >>=
    conditionally (unsafeStoreGroup gId)

storeActor
  :: MonadState Store m
  => ActorId -- ^ actor storing the created actor
  -> ActorId -- ^ created actor being stored
  -> m Bool
storeActor creator aId =
  canDo (\t -> t ^. forRecruiter) creator Create >>=
    conditionally (unsafeStoreActor aId)

addMember
  :: MonadState Store m
  => ActorId -- ^ actor creating membership
  -> GroupId -- ^ group gaining a member
  -> ActorId -- ^ new member
  -> m Bool
addMember creator gId aId = do
  canDo (\t -> t ^. forMembers . at gId . non Blind) creator Create >>=
    conditionally (unsafeAddMember gId aId)

storeSpace
  :: MonadState Store m
  => ActorId -- ^ actor storing the space
  -> SpaceId -- ^ space being created
  -> m Bool
storeSpace creator sId =
  canDo (\t -> t ^. forUniverse . collectionPermission) creator Create >>=
    conditionally (unsafeStoreSpace sId)

storeEntity
  :: MonadState Store m
  => ActorId -- ^ actor storing the entity
  -> EntityId -- ^ entity being stored
  -> SpaceId -- ^ space in which entity is being stored
  -> VersionId -- ^ initial version
  -> m Bool
storeEntity creator eId sId vId = do
  canAdjust <- canDo (\t -> t ^. forEntities . at sId . non Blind) creator Create
  if not canAdjust then pure False else do
    eVErr <- unsafeStoreEntity eId sId vId genesisVersion
    case eVErr of
      Left e -> error $ "Impossible error - genesis version has undefined reference " <> show (e, eId, sId, vId)
      Right () -> pure True

data StoreForkedEntityError
  = PreviousVersionDoesntExist
  | PreviousEntityDoesntExist
  | ForkingSelf
  deriving (Eq, Show, Read)

storeForkedEntity
  :: MonadState Store m
  => ActorId -- ^ actor forking the entity
  -> EntityId -- ^ entity being forked
  -> SpaceId -- ^ space in which entity is being forked to
  -> VersionId -- ^ initial version of forked entitiy
  -> VersionId -- ^ previous version of entity (possibly in a different space, definitely in a different entity)
  -> m (Maybe (Either (Either StoreForkedEntityError StoreVersionError) ()))
storeForkedEntity creator eId sId vId prevVId
  | vId == prevVId = pure . Just . Left $ Left ForkingSelf
  | otherwise = do
  s <- get
  case s ^. toVersions . at prevVId of
    Nothing -> pure . Just . Left $ Left PreviousVersionDoesntExist
    Just prevV -> case s ^. toEntities . at (prevV ^. entity) of
      Nothing -> pure . Just . Left $ Left PreviousEntityDoesntExist
      Just prevE -> do
        let prevSId = prevE ^. space
        canAdjust <- andM
          [ canDo (\t -> t ^. forEntities . at sId . non Blind) creator Create
          , canDo (\t -> t ^. forEntities . at prevSId . non Blind) creator Read
          ]
        if not canAdjust then pure Nothing else do
          eVErr <- unsafeStoreEntity eId sId vId (flip forkVersion prevVId)
          pure . Just $ eVErr & _Left %~ Right

storeVersion
  :: MonadState Store m
  => ActorId -- ^ actor attempting to store a version
  -> EntityId -- ^ entity receiving a new version
  -> VersionId -- ^ version being stored
  -> m (Maybe (Either StoreVersionError ()))
storeVersion creator eId vId = do
  s <- get
  case s ^. toEntities . at eId of
    Nothing -> pure Nothing
    Just e -> do
      canAdjust <- canDo (\t -> t ^. forEntities . at (e ^. space) . non Blind) creator Create
      if not canAdjust then pure Nothing else do
        fmap Just . unsafeStoreVersion eId vId . flip forkVersion . NE.head $ e ^. versions

-- | Will only update the group if the actor has same or greater permission
setUniversePermission
  :: MonadState Store m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermissionWithExemption -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> m Bool
setUniversePermission creator p gId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , canDo (\t -> t ^. forUniverse) creator p
    ]
  conditionally
    (unsafeAdjustUniversePermission (const p) gId)
    canAdjust

setOrganizationPermission 
  :: MonadState Store m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermissionWithExemption -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> m Bool
setOrganizationPermission creator p gId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , canDo (\t -> t ^. forOrganization) creator p
    ]
  conditionally
    (unsafeAdjustOrganizationPermission (const p) gId)
    canAdjust

setRecruiterPermission 
  :: MonadState Store m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermission -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> m Bool
setRecruiterPermission creator p gId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , canDo (\t -> t ^. forRecruiter) creator p
    ]
  conditionally
    (unsafeAdjustRecruiterPermission (const p) gId)
    canAdjust

setSpacePermission 
  :: MonadState Store m
  => ActorId -- ^ actor attempting to set permission
  -> Maybe SinglePermission -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> SpaceId -- ^ relevant to this space
  -> m Bool
setSpacePermission creator p gId sId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , canDoWithTab -- use universe permission if spaces permission isn't set
        (\t -> fromMaybe (t ^. forUniverse . collectionPermission) (t ^. forSpaces . at sId))
        creator
        (\t -> maybe Read (escalate (t ^. forUniverse)) p)
    ]
  conditionally
    (unsafeAdjustSpacePermission (const p) gId sId)
    canAdjust

setEntityPermission 
  :: MonadState Store m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermission -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> SpaceId -- ^ relevant to this space
  -> m Bool
setEntityPermission creator p gId sId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , orM
      [ canDo (\t -> t ^. forEntities . at sId . non Blind) creator p -- can you already do stuff with entities?
      , canDo (\t -> t ^. forUniverse . collectionPermission) creator p -- if not, do you have universe permissions?
      ]
    ]
  conditionally
    (unsafeAdjustEntityPermission (const p) gId sId)
    canAdjust

setGroupPermission 
  :: MonadState Store m
  => ActorId -- ^ actor attempting to set permission
  -> Maybe SinglePermission -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> GroupId -- ^ relevant to this group (grants one group to NEAO other groups)
  -> m Bool
setGroupPermission creator p gId towardGId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , canDoWithTab
        (\t -> fromMaybe (t ^. forOrganization . collectionPermission)
           (t ^. forGroups . at towardGId)) -- I've already been granted permissions with that group I'm allowing the first permission over
        creator
        (\t -> maybe Read (escalate (t ^. forOrganization)) p)
    ]
  conditionally
    (unsafeAdjustGroupPermission (const p) gId towardGId)
    canAdjust

setMemberPermission 
  :: MonadState Store m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermission -- ^ the permission being granted
  -> GroupId -- ^ the group gaining the permission
  -> GroupId -- ^ the group that can have their members manipulated
  -> m Bool
setMemberPermission creator p manipulatorGId manipulatedGId = do
  canAdjust <- andM
    [ canUpdateGroup creator manipulatorGId
    , orM
      [ canDo (\t -> t ^. forMembers . at manipulatedGId . non Blind) creator p -- can you already do stuff to manipulated?
      , canDo (\t -> t ^. forOrganization . collectionPermission) creator p -- if not, do you have organization rights?
      ]
    ]
  conditionally
    (unsafeAdjustMemberPermission (const p) manipulatorGId manipulatedGId)
    canAdjust
