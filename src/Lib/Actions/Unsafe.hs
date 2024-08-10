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

module Lib.Actions.Unsafe
  ( unsafeEmptyStore
  , unsafeStoreGroup
  , unsafeStoreActor
  , unsafeAddMember
  , unsafeStoreSpace
  , unsafeStoreEntity
  , unsafeStoreVersion
  , LinkGroupError (..)
  , unsafeLinkGroups
  , unsafeUnlinkGroups
  , unsafeAdjustUniversePermission
  , unsafeAdjustOrganizationPermission
  , unsafeAdjustRecruiterPermission
  , unsafeAdjustGroupPermission
  , unsafeAdjustSpacePermission
  , unsafeAdjustEntityPermission
  , unsafeAdjustMemberPermission
  ) where

import Lib.Actions.Tabulation
  ( updateTabulationStartingAt
  )
import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption
  , SinglePermission
  )
import Lib.Types.Store
  ( Store (..)
  , toGroups
  , toActors
  , toSpaces
  , toEntities
  , toVersions
  , toSpacePermissions
  , toEntityPermissions
  , toGroupPermissions
  , toMemberPermissions
  )
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Entity (initEntity, addVersion)
import Lib.Types.Store.Version (Version)
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

import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import Control.Lens (Lens', (&), (^.), (.~), (%~), at, non, ix)
import Control.Monad.State (MonadState (get, put), modify)
import Control.Monad.Extra (when)



-- Note that this doesn't grant any initial "admin" actor
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
  , storeMemberPermissions = mempty
  , storeTabulatedPermissions = mempty
  }

-- | Sets the group to empty
unsafeStoreGroup :: MonadState Store m => GroupId -> m ()
unsafeStoreGroup gId = do
  modify $ toGroups . nodes . at gId %~ Just . fromMaybe emptyGroup
  s <- get
  when (null (s ^. toGroups . nodes . at gId . non emptyGroup . prev)) $
    modify $ toGroups . roots . at gId .~ Just ()

unsafeStoreActor :: MonadState Store m => ActorId -> m ()
unsafeStoreActor aId = do
  modify $ toActors . at aId .~ Just mempty

unsafeAddMember :: MonadState Store m => GroupId -> ActorId -> m ()
unsafeAddMember gId aId = do
  modify $ toActors . at aId . non mempty . at gId .~ Just ()
  modify $ toGroups . nodes . at gId . non emptyGroup . members . at aId .~ Just ()

-- | Sets the space to empty
unsafeStoreSpace :: MonadState Store m => SpaceId -> m ()
unsafeStoreSpace sId = do
  modify $ toSpaces . at sId .~ Just mempty

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

-- | Loads the parent's untabulated permissions if it's not already tabulated!
unsafeLinkGroups :: MonadState Store m => GroupId -> GroupId -> m (Either LinkGroupError ())
unsafeLinkGroups from to = do
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

unsafeUnlinkGroups :: MonadState Store m => GroupId -> GroupId -> m ()
unsafeUnlinkGroups from to = do
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

unsafeAdjustPermissionForGroup
  :: MonadState Store m
  => Lens' Group a
  -> (a -> a)
  -> GroupId
  -> m ()
unsafeAdjustPermissionForGroup project f gId = do
  modify $ toGroups . nodes . ix gId . project %~ f
  updateTabulationStartingAt gId

unsafeAdjustUniversePermission
  :: MonadState Store m
  => (CollectionPermissionWithExemption -> CollectionPermissionWithExemption)
  -> GroupId
  -> m ()
unsafeAdjustUniversePermission =
  unsafeAdjustPermissionForGroup universePermission

unsafeAdjustOrganizationPermission
  :: MonadState Store m
  => (CollectionPermissionWithExemption -> CollectionPermissionWithExemption)
  -> GroupId
  -> m ()
unsafeAdjustOrganizationPermission =
  unsafeAdjustPermissionForGroup organizationPermission

unsafeAdjustRecruiterPermission
  :: MonadState Store m
  => (CollectionPermission -> CollectionPermission)
  -> GroupId
  -> m ()
unsafeAdjustRecruiterPermission =
  unsafeAdjustPermissionForGroup recruiterPermission

unsafeAdjustPermission
  :: MonadState Store m
  => Lens' Store a
  -> (a -> a)
  -> GroupId
  -> m ()
unsafeAdjustPermission project f gId = do
  modify $ project %~ f
  updateTabulationStartingAt gId

unsafeAdjustSpacePermission
  :: MonadState Store m
  => (Maybe SinglePermission -> Maybe SinglePermission)
  -> GroupId
  -> SpaceId
  -> m ()
unsafeAdjustSpacePermission f gId sId =
  unsafeAdjustPermission
    (toSpacePermissions . at gId . non mempty . at sId)
    f
    gId

unsafeAdjustEntityPermission
  :: MonadState Store m
  => (CollectionPermission -> CollectionPermission)
  -> GroupId
  -> SpaceId
  -> m ()
unsafeAdjustEntityPermission f gId sId =
  unsafeAdjustPermission
    (toEntityPermissions . at gId . non mempty . at sId . non Blind)
    f
    gId

unsafeAdjustGroupPermission
  :: MonadState Store m
  => (Maybe SinglePermission -> Maybe SinglePermission) -- ^ get new permission
  -> GroupId -- ^ group gaining new permission
  -> GroupId -- ^ group being subject to manipulation
  -> m ()
unsafeAdjustGroupPermission f gId gId' =
  unsafeAdjustPermission
    (toGroupPermissions . at gId . non mempty . at gId')
    f
    gId

unsafeAdjustMemberPermission
  :: MonadState Store m
  => (CollectionPermission -> CollectionPermission)
  -> GroupId
  -> GroupId
  -> m ()
unsafeAdjustMemberPermission f gId gId' =
  unsafeAdjustPermission
    (toMemberPermissions . at gId . non mempty . at gId' . non Blind)
    f
    gId
