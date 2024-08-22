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


module Lib.Actions.Safe.Update.Group where

import Control.Lens (ix, (^.), (^?), _Just)
import Control.Monad.Extra (andM, anyM)
import Control.Monad.State (MonadState, get)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Lib.Actions.Safe.Verify (anyCanUpdateGroup, conditionally)
import Lib.Actions.Safe.Verify.Group (hasGroupPermission)
import Lib.Actions.Safe.Verify.Member (hasMemberPermission)
import Lib.Actions.Safe.Verify.SpaceAndEntity (
  hasEntityPermission,
  hasSpacePermission,
 )
import Lib.Actions.Safe.Verify.Utils (canDo)
import Lib.Actions.Unsafe.Update.Group (
  LinkGroupError,
  unsafeAdjustEntityPermission,
  unsafeAdjustGroupPermission,
  unsafeAdjustMemberPermission,
  unsafeAdjustOrganizationPermission,
  unsafeAdjustRecruiterPermission,
  unsafeAdjustSpacePermission,
  unsafeAdjustUniversePermission,
  unsafeLinkGroups,
  unsafeUnlinkGroups,
  unsafeUpdateGroupChildren,
  unsafeUpdateGroupParent,
 )
import Lib.Types.Id (ActorId, GroupId, SpaceId)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption (..),
  SinglePermission (..),
 )
import Lib.Types.Store (Shared, store, toGroups)
import Lib.Types.Store.Groups (next, nodes, prev)
import Lib.Types.Store.Tabulation.Group (
  forOrganization,
  forRecruiter,
  forUniverse,
 )

linkGroups
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> GroupId
  -> GroupId
  -> m (Maybe (Either LinkGroupError ()))
linkGroups updater gId childId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup updater gId
      , anyCanUpdateGroup updater childId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeLinkGroups gId childId

unlinkGroups
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> GroupId
  -> GroupId
  -> m Bool
unlinkGroups updater gId childId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup updater gId
      , anyCanUpdateGroup updater childId
      ]
  conditionally (unsafeUnlinkGroups gId childId) canAdjust

updateGroupParent
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> GroupId
  -> Maybe GroupId
  -> m (Maybe (Either LinkGroupError ()))
updateGroupParent updater gId mParent = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup updater gId
      , do
          s <- get
          case s ^? store . toGroups . nodes . ix gId . prev . _Just of
            Nothing -> pure True
            Just oldParent -> anyCanUpdateGroup updater oldParent
      , case mParent of
          Nothing -> pure True
          Just newParent -> anyCanUpdateGroup updater newParent
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeUpdateGroupParent gId mParent

updateGroupChildren
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> GroupId
  -> HashSet GroupId
  -> m (Maybe (Either LinkGroupError ()))
updateGroupChildren updater gId newChildren = do
  s <- get
  case s ^? store . toGroups . nodes . ix gId . next of
    Nothing -> pure Nothing
    Just oldChildren -> do
      canAdjust <-
        andM $
          anyCanUpdateGroup updater gId
            : map (anyCanUpdateGroup updater) (HS.toList (newChildren <> oldChildren))
      if not canAdjust
        then pure Nothing
        else Just <$> unsafeUpdateGroupChildren gId newChildren

-- | Will only update the group if the actor has same or greater permission
setUniversePermission
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> CollectionPermissionWithExemption
  -- ^ permission being set
  -> GroupId
  -- ^ group subject to new permission
  -> m Bool
setUniversePermission creator p gId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator gId
      , anyM (\c -> canDo (^. forUniverse) c p) (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustUniversePermission (const p) gId)
    canAdjust

setOrganizationPermission
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> CollectionPermissionWithExemption
  -- ^ permission being set
  -> GroupId
  -- ^ group subject to new permission
  -> m Bool
setOrganizationPermission creator p gId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator gId
      , anyM (\c -> canDo (^. forOrganization) c p) (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustOrganizationPermission (const p) gId)
    canAdjust

setRecruiterPermission
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> CollectionPermission
  -- ^ permission being set
  -> GroupId
  -- ^ group subject to new permission
  -> m Bool
setRecruiterPermission creator p gId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator gId
      , anyM (\c -> canDo (^. forRecruiter) c p) (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustRecruiterPermission (const p) gId)
    canAdjust

setSpacePermission
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> Maybe SinglePermission
  -- ^ permission being set
  -> GroupId
  -- ^ group subject to new permission
  -> SpaceId
  -- ^ relevant to this space
  -> m Bool
setSpacePermission creator p gId sId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator gId
      , anyM (\c -> hasSpacePermission c sId (fromMaybe Exists p)) (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustSpacePermission (const p) gId sId)
    canAdjust

setEntityPermission
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> CollectionPermission
  -- ^ permission being set
  -> GroupId
  -- ^ group subject to new permission
  -> SpaceId
  -- ^ relevant to this space
  -> m Bool
setEntityPermission creator p gId sId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator gId
      , anyM (\c -> hasEntityPermission c sId p) (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustEntityPermission (const p) gId sId)
    canAdjust

setGroupPermission
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> Maybe SinglePermission
  -- ^ permission being set
  -> GroupId
  -- ^ group subject to new permission
  -> GroupId
  -- ^ relevant to this group (grants one group to NEAO other groups)
  -> m Bool
setGroupPermission creator p gId towardGId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator gId
      , anyM
          (\c -> hasGroupPermission c towardGId (fromMaybe Exists p))
          (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustGroupPermission (const p) gId towardGId)
    canAdjust

setMemberPermission
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> CollectionPermission
  -- ^ the permission being granted
  -> GroupId
  -- ^ the group gaining the permission
  -> GroupId
  -- ^ the group that can have their members manipulated
  -> m Bool
setMemberPermission creator p manipulatorGId manipulatedGId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator manipulatorGId
      , anyM (\c -> hasMemberPermission c manipulatedGId p) (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustMemberPermission (const p) manipulatorGId manipulatedGId)
    canAdjust
