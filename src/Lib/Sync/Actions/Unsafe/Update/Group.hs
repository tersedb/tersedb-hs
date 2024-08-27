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

import Control.Lens (
  Lens',
  at,
  ix,
  non,
  (%~),
  (&),
  (.~),
  (?~),
  (^.),
  (^?),
  _Just,
 )
import Control.Monad (unless)
import Control.Monad.State (MonadState (get, put), execState, modify, runState)
import Data.Foldable (foldlM, for_)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)
import Lib.Sync.Actions.Tabulation (updateTabulationStartingAt)
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
import Lib.Types.Id (GroupId, SpaceId)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption,
  SinglePermission,
 )

-- | Loads the parent's untabulated permissions if it's not already tabulated!
unsafeLinkGroups
  :: (MonadState Shared m) => GroupId -> GroupId -> m ()
unsafeLinkGroups from to = do
  s <- get
  let groups = s ^. store . toGroups
  unless (HS.member (from, to) (groups ^. edges)) $ -- edge already exists
    unless (HS.member to (groups ^. outs)) $ -- would cause a cycle
      let newGroups =
            groups
              & edges . at (from, to) ?~ ()
              & outs . at to ?~ ()
              & outs . at from .~ Nothing
              & roots . at to .~ Nothing
              & nodes . ix from . next . at to ?~ ()
              & nodes . ix to . prev ?~ from
       in case hasCycle newGroups of
            Just cycle -> pure ()
            Nothing -> do
              modify $ store . toGroups .~ newGroups
              updateTabulationStartingAt to

unsafeUnlinkGroups :: (MonadState Shared m) => GroupId -> GroupId -> m ()
unsafeUnlinkGroups from to = do
  s <- get
  let groups = s ^. store . toGroups
      newGroups =
        groups
          & edges . at (from, to) .~ Nothing
          & outs . at to .~ Nothing
          & outs . at from ?~ ()
          & roots . at to ?~ ()
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
  -> m ()
unsafeUpdateGroupParent gId mParent = do
  s <- get
  case s ^? store . toGroups . nodes . ix gId . prev . _Just of
    mOldParent | mOldParent == mParent -> pure ()
    Nothing -> unsafeLinkGroups (fromJust mParent) gId
    Just oldParent -> do
      unsafeUnlinkGroups oldParent gId
      case mParent of
        Nothing -> pure ()
        Just newParent -> unsafeLinkGroups newParent gId

unsafeUpdateGroupChildren
  :: (MonadState Shared m)
  => GroupId
  -> HashSet GroupId
  -> m ()
unsafeUpdateGroupChildren gId children = do
  let newChildren = HS.delete gId children
  s <- get
  case s ^? store . toGroups . nodes . ix gId . next of
    Nothing -> pure () -- FIXME gId not found - should be error?
    Just oldChildren -> do
      let toAdd = newChildren `HS.difference` oldChildren
          toRemove = oldChildren `HS.difference` newChildren
          addChild :: GroupId -> Shared -> Shared
          addChild toAdd = execState (unsafeLinkGroups gId toAdd)
          s' = foldr addChild s toAdd
      put s'
      for_ toRemove $ \toRemove -> unsafeUnlinkGroups gId toRemove

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
