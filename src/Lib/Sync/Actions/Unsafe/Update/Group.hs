{-
TerseDB - Entity Management System
Copyright (C) 2024  Athan Clark

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

You can reach me at athan.clark@gmail.com.
-}

module Lib.Sync.Actions.Unsafe.Update.Group (
  LinkGroupError (..),
  unsafeLinkGroups,
  unsafeUnlinkGroups,
  unsafeUpdateGroupParent,
  unsafeUpdateGroupChildren,
  unsafeAdjustUniversePermission,
  unsafeAdjustOrganizationPermission,
  unsafeAdjustRecruiterPermission,
  unsafeAdjustGroupPermission,
  unsafeAdjustSpacePermission,
  unsafeAdjustEntityPermission,
  unsafeAdjustMemberPermission,
) where

import Lib.Sync.Actions.Tabulation (updateTabulationStartingAt)
import Lib.Sync.Types.Id (GroupId, SpaceId)
import Lib.Sync.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption,
  SinglePermission,
 )
import Lib.Sync.Types.Store (
  Shared (..),
  store,
  toEntityPermissions,
  toGroupPermissions,
  toGroups,
  toMemberPermissions,
  toSpacePermissions,
 )
import Lib.Sync.Types.Store.Groups (
  Group,
  edges,
  hasCycle,
  next,
  nodes,
  organizationPermission,
  outs,
  prev,
  recruiterPermission,
  roots,
  universePermission,
 )

import Control.Lens (Lens', at, ix, non, (%~), (&), (.~), (^.), (^?), _Just)
import Control.Monad.State (MonadState (get, put), modify, runState)
import Data.Foldable (foldlM, for_)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)

data LinkGroupError
  = CycleDetected [GroupId]
  | MultiParent GroupId
  deriving (Eq, Show, Read)

-- | Loads the parent's untabulated permissions if it's not already tabulated!
unsafeLinkGroups
  :: (MonadState Shared m) => GroupId -> GroupId -> m (Either LinkGroupError ())
unsafeLinkGroups from to = do
  s <- get
  let groups = s ^. store . toGroups
  if HS.member (from, to) (groups ^. edges)
    then pure (Right ())
    else
      if HS.member to (groups ^. outs)
        then pure (Left (MultiParent to))
        else
          let newGroups =
                groups
                  & edges . at (from, to) .~ Just ()
                  & outs . at to .~ Just ()
                  & outs . at from .~ Nothing
                  & roots . at to .~ Nothing
                  & nodes . ix from . next . at to .~ Just ()
                  & nodes . ix to . prev .~ Just from
           in case hasCycle newGroups of
                Just cycle -> pure . Left $ CycleDetected cycle
                Nothing -> do
                  modify $ store . toGroups .~ newGroups
                  updateTabulationStartingAt to
                  pure (Right ())

unsafeUnlinkGroups :: (MonadState Shared m) => GroupId -> GroupId -> m ()
unsafeUnlinkGroups from to = do
  s <- get
  let groups = s ^. store . toGroups
      newGroups =
        groups
          & edges . at (from, to) .~ Nothing
          & outs . at to .~ Nothing
          & outs . at from .~ Just ()
          & roots . at to .~ Just ()
          & nodes . ix from . next . at to .~ Nothing
          & nodes . ix to . prev .~ Nothing
  put $
    s
      & store . toGroups .~ newGroups
  updateTabulationStartingAt to

unsafeUpdateGroupParent
  :: (MonadState Shared m)
  => GroupId
  -> Maybe GroupId
  -> m (Either LinkGroupError ())
unsafeUpdateGroupParent gId mParent = do
  s <- get
  case s ^? store . toGroups . nodes . ix gId . prev . _Just of
    mOldParent | mOldParent == mParent -> pure (Right ())
    Nothing -> unsafeLinkGroups (fromJust mParent) gId
    Just oldParent -> do
      unsafeUnlinkGroups oldParent gId
      case mParent of
        Nothing -> pure (Right ())
        Just newParent -> unsafeLinkGroups newParent gId

unsafeUpdateGroupChildren
  :: (MonadState Shared m)
  => GroupId
  -> HashSet GroupId
  -> m (Either LinkGroupError ())
unsafeUpdateGroupChildren gId children = do
  let newChildren = HS.delete gId children
  s <- get
  case s ^? store . toGroups . nodes . ix gId . next of
    Nothing -> pure (Right ()) -- FIXME gId not found - should be error?
    Just oldChildren -> do
      let toAdd = newChildren `HS.difference` oldChildren
          toRemove = oldChildren `HS.difference` newChildren
          addChild :: Shared -> GroupId -> Either LinkGroupError Shared
          addChild s toAdd =
            let (mX :: Either LinkGroupError (), s' :: Shared) =
                  runState (unsafeLinkGroups gId toAdd) s
             in fmap (const s') mX
      case foldlM addChild s toAdd of
        Left e -> pure (Left e)
        Right s' -> do
          put s'
          for_ toRemove $ \toRemove -> unsafeUnlinkGroups gId toRemove
          pure (Right ())

unsafeAdjustPermissionForGroup
  :: (MonadState Shared m)
  => Lens' Group a
  -> (a -> a)
  -> GroupId
  -> m ()
unsafeAdjustPermissionForGroup project f gId = do
  modify $ store . toGroups . nodes . ix gId . project %~ f
  updateTabulationStartingAt gId

unsafeAdjustUniversePermission
  :: (MonadState Shared m)
  => (CollectionPermissionWithExemption -> CollectionPermissionWithExemption)
  -> GroupId
  -> m ()
unsafeAdjustUniversePermission =
  unsafeAdjustPermissionForGroup universePermission

unsafeAdjustOrganizationPermission
  :: (MonadState Shared m)
  => (CollectionPermissionWithExemption -> CollectionPermissionWithExemption)
  -> GroupId
  -> m ()
unsafeAdjustOrganizationPermission =
  unsafeAdjustPermissionForGroup organizationPermission

unsafeAdjustRecruiterPermission
  :: (MonadState Shared m)
  => (CollectionPermission -> CollectionPermission)
  -> GroupId
  -> m ()
unsafeAdjustRecruiterPermission =
  unsafeAdjustPermissionForGroup recruiterPermission

unsafeAdjustPermission
  :: (MonadState Shared m)
  => Lens' Shared a
  -> (a -> a)
  -> GroupId
  -> m ()
unsafeAdjustPermission project f gId = do
  modify $ project %~ f
  updateTabulationStartingAt gId

unsafeAdjustSpacePermission
  :: (MonadState Shared m)
  => (Maybe SinglePermission -> Maybe SinglePermission)
  -> GroupId
  -> SpaceId
  -> m ()
unsafeAdjustSpacePermission f gId sId =
  unsafeAdjustPermission
    (store . toSpacePermissions . at gId . non mempty . at sId)
    f
    gId

unsafeAdjustEntityPermission
  :: (MonadState Shared m)
  => (CollectionPermission -> CollectionPermission)
  -> GroupId
  -> SpaceId
  -> m ()
unsafeAdjustEntityPermission f gId sId =
  unsafeAdjustPermission
    (store . toEntityPermissions . at gId . non mempty . at sId . non Blind)
    f
    gId

unsafeAdjustGroupPermission
  :: (MonadState Shared m)
  => (Maybe SinglePermission -> Maybe SinglePermission)
  -- ^ get new permission
  -> GroupId
  -- ^ group gaining new permission
  -> GroupId
  -- ^ group being subject to manipulation
  -> m ()
unsafeAdjustGroupPermission f gId gId' =
  unsafeAdjustPermission
    (store . toGroupPermissions . at gId . non mempty . at gId')
    f
    gId

unsafeAdjustMemberPermission
  :: (MonadState Shared m)
  => (CollectionPermission -> CollectionPermission)
  -> GroupId
  -> GroupId
  -> m ()
unsafeAdjustMemberPermission f gId gId' =
  unsafeAdjustPermission
    (store . toMemberPermissions . at gId . non mempty . at gId' . non Blind)
    f
    gId
