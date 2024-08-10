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
  , escalateWithoutExemption
  , collectionPermission
  )
import Lib.Types.Store
  ( Store (..)
  , toActors
  , toEntities
  , toVersions
  , toTabulatedPermissions
  , TabulatedPermissionsForGroup (..)
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
import Control.Lens ((^.), at, non)
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

-- canDo
--   :: MonadState Store m
--   => Lens' TabulatedPermissionsForGroup CollectionPermission
--   -> ActorId
--   -> CollectionPermission
--   -> m Bool
-- canDo a b c = canDo' a b . const $ pure c
--
-- canDoWithExemption
--   :: MonadState Store m
--   => Lens' TabulatedPermissionsForGroup CollectionPermission
--   -> ActorId
--   -> CollectionPermissionWithExemption
--   -> m Bool
-- canDoWithExemption a b c = canDo' a b . const . pure $ c ^. collectionPermission
--
-- canDoSingle
--   :: MonadState Store m
--   => Lens' TabulatedPermissionsForGroup CollectionPermission
--   -> ActorId
--   -> CollectionPermission
--   -> Lens' Store (Maybe SinglePermission)
--   -> Lens' Group CollectionPermissionWithExemption
--   -- -> (CollectionPermissionWithExemption -> Maybe SinglePermission -> m CollectionPermission)
--   -> m Bool
-- canDoSingle fromTab creator p toSingle toCollection = do
--   -- canDo fromTab creator p
--   canDo' fromTab creator p $ \g -> do
--     s <- get
--     let c = g ^. toCollection
--         q = case s ^. toSingle of
--           Nothing -> c ^. collectionPermission
--           Just s' -> escalate c s'

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
  canAdjust <- orM
    [ canDo (\t -> t ^. forMembers . at gId . non Blind) creator Create
    , canDo (\t -> t ^. forOrganization . collectionPermission) creator Update
    ]
  conditionally (unsafeAddMember gId aId) canAdjust

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
  canAdjust <- orM
    [ canDo (\t -> t ^. forEntities . at sId . non Blind) creator Create
    -- , canDo (\t -> t ^. forUniverse . collectionPermission) creator Update -- FIXME is this correct? Just because I can update spaces, does that mean I have a right to create entities in any space?
    ]
  conditionally (unsafeStoreEntity eId sId vId genesisVersion) canAdjust

data StoreForkedEntityError
  = PreviousVersionDoesntExist
  | PreviousEntityDoesntExist
  deriving (Eq, Show, Read)

storeForkedEntity
  :: MonadState Store m
  => ActorId -- ^ actor forking the entity
  -> EntityId -- ^ entity being forked
  -> SpaceId -- ^ space in which entity is being forked to
  -> VersionId -- ^ initial version of forked entitiy
  -> VersionId -- ^ previous version of entity (possibly in a different space, definitely in a different entity)
  -> m (Either StoreForkedEntityError Bool)
storeForkedEntity creator eId sId vId prevVId = do
  s <- get
  case s ^. toVersions . at prevVId of
    Nothing -> pure (Left PreviousVersionDoesntExist)
    Just prevV -> case s ^. toEntities . at (prevV ^. entity) of
      Nothing -> pure (Left PreviousEntityDoesntExist)
      Just prevE -> do
        let prevSId = prevE ^. space
        canAdjust <- andM
          [ orM
            [ canDo (\t -> t ^. forEntities . at sId . non Blind) creator Create
            -- , canDo forUniverse creator Update -- FIXME is this correct? Just because I can update spaces, does that mean I have a right to create entities in any space?
            ]
          , canDo (\t -> t ^. forEntities . at prevSId . non Blind) creator Read
          ]
        Right <$> conditionally (unsafeStoreEntity eId sId vId (flip forkVersion prevVId)) canAdjust

storeVersion
  :: MonadState Store m
  => ActorId -- ^ actor attempting to store a version
  -> EntityId -- ^ entity receiving a new version
  -> VersionId -- ^ version being stored
  -- -> VersionId -- ^ previous version of entity -- FIXME just pull the latest from entity???
  -> m Bool
storeVersion creator eId vId = do
  s <- get
  case s ^. toEntities . at eId of
    Nothing -> pure False
    Just e -> do
      let sId = e ^. space
      canAdjust <- orM
        [ canDo (\t -> t ^. forSpaces . at sId . non Blind) creator Create
        , canDo (\t -> t ^. forUniverse . collectionPermission) creator Update -- FIXME is this valid? Because I can update any space, does that mean I can create data in any space?
        ]
      conditionally (unsafeStoreVersion eId vId . flip forkVersion . NE.head $ e ^. versions) canAdjust

-- | Will only update the group if the actor has same or greater permission
setUniversePermission
  :: MonadState Store m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermissionWithExemption -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> m Bool
setUniversePermission creator p gId = do
  canAdjust <- andM
    [ orM
      [ canDo (\t -> t ^. forGroups . at gId . non Blind) creator Update
      -- , canDo (\t -> t ^. forOrganization) creator Update -- forGroups is already subject to forOrganization
      ]
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
    [ orM
      [ canDo (\t -> t ^. forGroups . at gId . non Blind) creator Update
      -- , canDo forOrganization creator Update
      ]
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
    [ orM
      [ canDo (\t -> t ^. forGroups . at gId . non Blind) creator Update
      -- , canDo forOrganization creator Update
      ]
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
    [ orM
      [ canDo (\t -> t ^. forGroups . at gId . non Blind) creator Update
      , canDo (\t -> t ^. forOrganization . collectionPermission) creator Update
      ]
    , orM
      [ canDoWithTab -- creator can already do `p` with space `sId`
          (\t -> t ^. forSpaces . at sId . non Blind)
          creator
          (\t -> maybe Read (escalate (t ^. forUniverse)) p)
      -- , canDo forUniverse creator p
      -- FIXME only apply this if entry in spaces is Nothing; Just Blind should be forced to use exemption
      , canDo
          (\t -> t ^. forUniverse . collectionPermission)
          creator
          (maybe Blind escalateWithoutExemption  p)
      ]
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
    [ orM
      [ canDo (\t -> t ^. forGroups . at gId . non Blind) creator Update -- FIXME if this is Just Blind, and I'm not exempt, then this should fail
      , canDo (\t -> t ^. forOrganization . collectionPermission) creator Update -- FIXME what if I'm not exempt to the group rights?
      ]
    , orM
      [ canDo (\t -> t ^. forEntities . at sId . non Blind) creator p -- can you already do stuff with entities?
      , canDo (\t -> t ^. forUniverse . collectionPermission) creator p
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
  canAdjust <- orM
    [ andM
      [ canDo (\t -> t ^. forGroups . at gId . non Blind) creator Update -- I can update the group I'm granting permissions over another
      , canDoWithTab
          (\t -> t ^. forGroups . at towardGId . non Blind) -- I've already been granted permissions with that group I'm allowing the first permission over
          creator
          (\t -> maybe Read (escalate (t ^. forOrganization)) p)
      ]
    , canDo (\t -> t ^. forOrganization . collectionPermission) creator Update
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
  canAdjust <- orM
    [ andM
      [ canDo (\t -> t ^. forGroups . at manipulatorGId . non Blind) creator Update
      , canDo (\t -> t ^. forMembers . at manipulatedGId . non Blind) creator p
      ]
    -- , canDo forOrganization creator Update -- unnecessary as single permission adjusts for it?
    ]
  conditionally
    (unsafeAdjustMemberPermission (const p) manipulatorGId manipulatedGId)
    canAdjust
