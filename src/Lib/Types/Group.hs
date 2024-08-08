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

module Lib.Types.Group where

import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Permission (Permission (..))
import Lib.Types.Monad (SheepdogM)
import Lib.Types.Store
  ( Store (..)
  , toGroups
  , toActors
  , toSpaces
  , toEntities
  , toVersions
  , toTabulatedPermissions
  , toSpacePermissions
  , toEntityPermissions
  , toGroupPermissions
  , TabulatedPermissionsForGroup (..)
  , forGroupUniverse
  , forGroupOrganization
  , forGroupRecruiter
  , forGroupSpaces
  , forGroupEntities
  , forGroupGroups
  )
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Entity (Entity, initEntity, addVersion, space)
import Lib.Types.Store.Version (Version, genesisVersion, forkVersion)
import Lib.Types.Store.Groups
  ( Group
  , emptyGroup
  , emptyGroups
  , hasCycle
  , nodes
  , universePermission
  , organizationPermission
  , recruiterPermission
  , roots
  , outs
  , edges
  , next
  , prev
  , members
  )

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromJust, fromMaybe)
import Data.Maybe.HT (toMaybe)
import Data.Foldable (traverse_)
import System.Random (StdGen)
import System.Random.Stateful (AtomicGenM, StatefulGen, globalStdGen, Uniform (uniformM))
import Control.Lens (Lens', (&), (^.), (.~), (%~), at, non, ix)
import Control.Monad.State (MonadState (get, put), modify, execState)
import Control.Monad.Extra (andM, orM, when)


-- Using this would break the system
unsafeEmptyStore :: Store
unsafeEmptyStore = Store
  { storeGroups = emptyGroups
  , storeActors = mempty
  , storeSpaces = mempty
  , storeEntities = mempty
  , storeVersions = mempty
  , storeSpacePermissions = mempty
  , storeEntityPermissions = mempty
  , storeGroupPermissions = mempty
  , storeTabulatedPermissions = mempty
  }

emptyStore :: ActorId -> GroupId -> Store
emptyStore adminActor adminGroup = flip execState unsafeEmptyStore $ do
  unsafeStoreGroup adminGroup
  unsafeAdjustUniversePermission (const Delete) adminGroup
  unsafeAdjustOrganizationPermission (const Delete) adminGroup
  unsafeAdjustRecruiterPermission (const Delete) adminGroup
  unsafeStoreActor adminActor
  unsafeAddMember adminGroup adminActor
  updateTabulationStartingAt adminGroup

-- | Looks first for the groups the user is in, then sees if any of the groups
-- can do the action, depicted by the Lens
canDo :: MonadState Store m
      => Lens' TabulatedPermissionsForGroup Permission
      -> ActorId
      -> Permission
      -> m Bool
canDo proj creator p = do
  s <- get
  pure $ case s ^. toActors . at creator of
    Just groups ->
      let perGroup gId = s ^. toTabulatedPermissions . at gId . non mempty . proj >= p
      in  any perGroup (HS.toList groups)
    _ -> False

conditionally :: Applicative m => m () -> Bool -> m Bool
conditionally f t = t <$ when t f

generateWithAuthority
  :: ( StatefulGen (AtomicGenM StdGen) m
     , Uniform a
     ) => (a -> m Bool)
       -> m (Maybe a)
generateWithAuthority perform = do
  ident <- uniformM globalStdGen
  worked <- perform ident
  pure (toMaybe worked ident)

newGroup :: ActorId -> SheepdogM (Maybe GroupId)
newGroup = generateWithAuthority . storeGroup

storeGroup :: MonadState Store m => ActorId -> GroupId -> m Bool
storeGroup creator gId = do
  didSucceed <- canDo forGroupOrganization creator Create
  if not didSucceed then error "didn't succeed" else pure ()
  conditionally (unsafeStoreGroup gId) didSucceed

-- | Sets the group to empty
unsafeStoreGroup :: MonadState Store m => GroupId -> m ()
unsafeStoreGroup gId = do
  modify $ toGroups . nodes . at gId %~ Just . fromMaybe emptyGroup
  s <- get
  when (null (s ^. toGroups . nodes . at gId . non emptyGroup . prev)) $
    modify $ toGroups . roots . at gId .~ Just ()

newActor :: ActorId -> SheepdogM (Maybe ActorId)
newActor = generateWithAuthority . storeActor

storeActor :: MonadState Store m => ActorId -> ActorId -> m Bool
storeActor creator aId =
  canDo forGroupRecruiter creator Create >>=
    conditionally (unsafeStoreActor aId)

unsafeStoreActor :: MonadState Store m => ActorId -> m ()
unsafeStoreActor aId = do
  modify $ toActors . at aId .~ Just mempty

addMember :: MonadState Store m => ActorId -> GroupId -> ActorId -> m Bool
addMember creator gId aId = do
  canAdjust <- orM
    [ canDo (forGroupGroups . at gId . non Blind) creator Create
    , canDo forGroupOrganization creator Update
    ]
  conditionally (unsafeAddMember gId aId) canAdjust

unsafeAddMember :: MonadState Store m => GroupId -> ActorId -> m ()
unsafeAddMember gId aId = do
  modify $ toActors . at aId . non mempty . at gId .~ Just ()
  modify $ toGroups . nodes . at gId . non emptyGroup . members . at aId .~ Just ()

newSpace :: ActorId -> SheepdogM (Maybe SpaceId)
newSpace = generateWithAuthority . storeSpace

storeSpace :: MonadState Store m => ActorId -> SpaceId -> m Bool
storeSpace creator sId =
  canDo forGroupUniverse creator Create >>=
    conditionally (unsafeStoreSpace sId)

-- | Sets the space to empty
unsafeStoreSpace :: MonadState Store m => SpaceId -> m ()
unsafeStoreSpace sId = do
  modify $ toSpaces . at sId .~ Just mempty

newEntity :: ActorId -> SpaceId -> SheepdogM (Maybe (EntityId, VersionId))
newEntity creator sId =
  generateWithAuthority (\(eId, vId) -> storeEntity creator eId sId vId)

storeEntity
  :: MonadState Store m
  => ActorId
  -> EntityId
  -> SpaceId
  -> VersionId
  -> m Bool
storeEntity creator eId sId vId = do
  canAdjust <- orM
    [ canDo (forGroupSpaces . at sId . non Blind) creator Create
    , canDo forGroupUniverse creator Update -- FIXME is this correct? Just because I can update spaces, does that mean I have a right to create entities in any space?
    ]
  conditionally (unsafeStoreEntity eId sId vId genesisVersion) canAdjust

unsafeStoreEntity
  :: MonadState Store m
  => EntityId
  -> SpaceId
  -> VersionId
  -> (EntityId -> Version)
  -> m ()
unsafeStoreEntity eId sId vId buildVersion = do
  modify $ toEntities . at eId .~ Just (initEntity sId vId)
  modify $ toSpaces . ix sId . entities . at eId .~ Just ()
  modify $ toVersions . at vId .~ Just (buildVersion eId)

-- Adds a version to an existing entity -- FIXME make variant for appending on an existing entity
newVersion
  :: ActorId
  -> EntityId
  -> VersionId
  -> SheepdogM (Maybe VersionId)
newVersion creator eId prevVId =
  generateWithAuthority (\vId -> storeVersion creator eId vId prevVId)

storeVersion
  :: MonadState Store m
  => ActorId
  -> EntityId
  -> VersionId
  -> VersionId
  -> m Bool
storeVersion creator eId vId prevVId = do
  s <- get
  case s ^. toEntities . at eId of
    Nothing -> pure False
    Just e -> do
      let sId = e ^. space
      canAdjust <- orM
        [ canDo (forGroupSpaces . at sId . non Blind) creator Create
        , canDo forGroupUniverse creator Update -- FIXME is this valid? Because I can update any space, does that mean I can create data in any space?
        ]
      conditionally (unsafeStoreVersion eId vId (flip forkVersion prevVId)) canAdjust

unsafeStoreVersion
  :: MonadState Store m
  => EntityId
  -> VersionId
  -> (EntityId -> Version)
  -> m ()
unsafeStoreVersion eId vId buildVersion = do
  modify $ toEntities . ix eId %~ flip addVersion vId
  modify $ toVersions . at vId .~ Just (buildVersion eId)

data LinkGroupError
  = CycleDetected [GroupId]
  | DuplicateEdge GroupId GroupId
  | MultiParent GroupId
  deriving (Eq, Show, Read)

-- | Gets an initial tabulation for a specific group; assumes the group is a root
-- node, and isn't inheriting any other groups.
initTabulatedPermissionsForGroup :: MonadState Store m => GroupId -> m TabulatedPermissionsForGroup
initTabulatedPermissionsForGroup gId = do
  s <- get
  case s ^. toGroups . nodes . at gId of
    Nothing -> error $ "Group " <> show gId <> " doesn't exist in groups store"
    Just group -> do
      -- TODO fetch all relevant spaces & entities as well 
      pure TabulatedPermissionsForGroup
        { tabulatedPermissionsForGroupUniverse = group ^. universePermission
        , tabulatedPermissionsForGroupOrganization = group ^. organizationPermission
        , tabulatedPermissionsForGroupRecruiter = group ^. recruiterPermission
        , tabulatedPermissionsForGroupSpaces = fromMaybe mempty (s ^. toSpacePermissions . at gId)
        , tabulatedPermissionsForGroupEntities = fromMaybe mempty (s ^. toEntityPermissions . at gId)
        , tabulatedPermissionsForGroupGroups = fromMaybe mempty (s ^. toGroupPermissions . at gId)
        }

-- | updates the tab with a possibly erroneous cache for the parent if its missing
-- - would only work if the parent happened to be the root node.
updateTabulationStartingAt :: MonadState Store m => GroupId -> m ()
updateTabulationStartingAt gId = do
  s <- get
  initTab <- initTabulatedPermissionsForGroup gId
  let group = fromJust (s ^. toGroups . nodes . at gId)
  parentTab <- case group ^. prev of
    Nothing -> pure mempty -- root node
    Just parent -> case s ^. toTabulatedPermissions . at parent of
      Nothing -> do
        t <- initTabulatedPermissionsForGroup parent
        modify $ toTabulatedPermissions . at parent .~ Just t
        pure t
      Just t -> pure t
  let newTab = parentTab <> initTab
  case s ^. toTabulatedPermissions . at gId of
    Just oldTab | newTab == oldTab -> pure ()
    _ -> do
      -- doing fromJust because initTabulatedPermissionsForGroup already checks
      modify (toTabulatedPermissions . at gId .~ Just newTab)
      traverse_ updateTabulationStartingAt . HS.toList $ group ^. next


resetTabulation :: MonadState Store m => m ()
resetTabulation = do
  s <- get
  traverse_ updateTabulationStartingAt . HS.toList $ s ^. toGroups . roots

-- | Loads the parent's untabulated permissions if it's not already tabulated!
linkGroups :: MonadState Store m => GroupId -> GroupId -> m (Either LinkGroupError ())
linkGroups from to = do
  s <- get
  let groups = s ^. toGroups
  if HS.member (from, to) (groups ^. edges)
  then pure (Left (DuplicateEdge from to))
  else if HS.member to (groups ^. outs)
  then pure (Left (MultiParent to))
  else
    let newGroups = groups
          & edges . at (from,to) .~ Just ()
          & outs . at to .~ Just ()
          & outs . at from .~ Nothing
          & roots . at to .~ Nothing
          & nodes . ix from . next . at to .~ Just ()
          & nodes . ix to . prev .~ Just from
    in case hasCycle newGroups of
        Just cycle -> pure . Left $ CycleDetected cycle
        Nothing -> do
          modify $ toGroups .~ newGroups
          updateTabulationStartingAt to
          pure (Right ())

unlinkGroups :: MonadState Store m => GroupId -> GroupId -> m ()
unlinkGroups from to = do
  s <- get
  let groups = s ^. toGroups
      newGroups = groups
        & edges . at (from,to) .~ Nothing
        & outs . at to .~ Nothing
        & outs . at from .~ Just ()
        & roots . at to .~ Just ()
        & nodes . ix from . next . at to .~ Nothing
        & nodes . ix to . prev .~ Nothing
  put $ s
      & toGroups .~ newGroups
  updateTabulationStartingAt to


-- Permissions that affect the group alone

unsafeAdjustPermissionForGroup
  :: MonadState Store m
  => Lens' Group Permission
  -> (Permission -> Permission)
  -> GroupId
  -> m ()
unsafeAdjustPermissionForGroup project f gId = do
  modify $ toGroups . nodes . ix gId . project %~ f
  updateTabulationStartingAt gId

-- | Will only update the group if the actor has same or greater permission
setUniversePermission
  :: MonadState Store m
  => ActorId
  -> Permission
  -> GroupId
  -> m Bool
setUniversePermission creator p gId = do
  canAdjust <- andM
    [ orM
      [ canDo (forGroupGroups . at gId . non Blind) creator Update
      , canDo forGroupOrganization creator Update
      ]
    , canDo forGroupUniverse creator p
    ]
  conditionally
    (unsafeAdjustUniversePermission (const p) gId)
    canAdjust

unsafeAdjustUniversePermission
  :: MonadState Store m
  => (Permission -> Permission)
  -> GroupId
  -> m ()
unsafeAdjustUniversePermission =
  unsafeAdjustPermissionForGroup universePermission

setOrganizationPermission 
  :: MonadState Store m
  => ActorId
  -> Permission
  -> GroupId
  -> m Bool
setOrganizationPermission creator p gId = do
  canAdjust <- andM
    [ orM
      [ canDo (forGroupGroups . at gId . non Blind) creator Update
      , canDo forGroupOrganization creator Update
      ]
    , canDo forGroupOrganization creator p
    ]
  conditionally
    (unsafeAdjustOrganizationPermission (const p) gId)
    canAdjust

unsafeAdjustOrganizationPermission
  :: MonadState Store m
  => (Permission -> Permission)
  -> GroupId
  -> m ()
unsafeAdjustOrganizationPermission =
  unsafeAdjustPermissionForGroup organizationPermission

setRecruiterPermission 
  :: MonadState Store m
  => ActorId
  -> Permission
  -> GroupId
  -> m Bool
setRecruiterPermission creator p gId = do
  canAdjust <- andM
    [ orM
      [ canDo (forGroupGroups . at gId . non Blind) creator Update
      , canDo forGroupOrganization creator Update
      ]
    , canDo forGroupRecruiter creator p
    ]
  conditionally
    (unsafeAdjustRecruiterPermission (const p) gId)
    canAdjust

unsafeAdjustRecruiterPermission
  :: MonadState Store m
  => (Permission -> Permission)
  -> GroupId
  -> m ()
unsafeAdjustRecruiterPermission =
  unsafeAdjustPermissionForGroup recruiterPermission

unsafeAdjustPermission
  :: MonadState Store m
  => Lens' Store Permission
  -> (Permission -> Permission)
  -> GroupId
  -> m ()
unsafeAdjustPermission project f gId = do
  modify $ project %~ f
  updateTabulationStartingAt gId

setSpacePermission 
  :: MonadState Store m
  => ActorId
  -> Permission
  -> GroupId
  -> SpaceId
  -> m Bool
setSpacePermission creator p gId sId = do
  canAdjust <- andM
    [ orM
      [ canDo (forGroupGroups . at gId . non Blind) creator Update
      , canDo forGroupOrganization creator Update
      ]
    , orM
      [ canDo (forGroupSpaces . at sId . non Blind) creator p
      , canDo forGroupUniverse creator p
      ]
    ]
  conditionally
    (unsafeAdjustSpacePermission (const p) gId sId)
    canAdjust

unsafeAdjustSpacePermission
  :: MonadState Store m
  => (Permission -> Permission)
  -> GroupId
  -> SpaceId
  -> m ()
unsafeAdjustSpacePermission f gId sId =
  unsafeAdjustPermission
    (toSpacePermissions . at gId . non mempty . at sId . non Blind)
    f
    gId

setEntityPermission 
  :: MonadState Store m
  => ActorId
  -> Permission
  -> GroupId
  -> SpaceId
  -> m Bool
setEntityPermission creator p gId sId = do
  canAdjust <- andM
    [ orM
      [ canDo (forGroupGroups . at gId . non Blind) creator Update
      , canDo forGroupOrganization creator Update
      ]
    , orM
      [ canDo (forGroupEntities . at sId . non Blind) creator p -- can you already do stuff with entities?
      , canDo (forGroupSpaces . at sId . non Blind) creator p -- okay what about the space itself?
      , canDo forGroupUniverse creator p
      ]
    ]
  conditionally
    (unsafeAdjustEntityPermission (const p) gId sId)
    canAdjust

unsafeAdjustEntityPermission
  :: MonadState Store m
  => (Permission -> Permission)
  -> GroupId
  -> SpaceId
  -> m ()
unsafeAdjustEntityPermission f gId sId =
  unsafeAdjustPermission
    (toEntityPermissions . at gId . non mempty . at sId . non Blind)
    f
    gId

setGroupPermission 
  :: MonadState Store m
  => ActorId
  -> Permission
  -> GroupId
  -> GroupId
  -> m Bool
setGroupPermission creator p manipulatorGId manipulatedGId = do
  canAdjust <- orM
    [ orM
      [ canDo (forGroupGroups . at manipulatorGId . non Blind) creator Update
      , canDo (forGroupGroups . at manipulatedGId . non Blind) creator p
      ]
    , canDo forGroupOrganization creator Update
    ]
  conditionally
    (unsafeAdjustGroupPermission (const p) manipulatorGId manipulatedGId)
    canAdjust

unsafeAdjustGroupPermission
  :: MonadState Store m
  => (Permission -> Permission)
  -> GroupId
  -> GroupId
  -> m ()
unsafeAdjustGroupPermission f gId gId' =
  unsafeAdjustPermission
    (toGroupPermissions . at gId . non mempty . at gId' . non Blind)
    f
    gId
