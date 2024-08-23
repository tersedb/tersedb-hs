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

module Lib.Sync.Actions.Safe.Store where

import Control.Monad.Extra (andM)
import Control.Monad.State (MonadState)
import Data.List.NonEmpty (NonEmpty)
import Lib.Sync.Actions.Safe.Verify (
  anyCanCreateActor,
  anyCanCreateEntity,
  anyCanCreateGroup,
  anyCanCreateMember,
  anyCanCreateSpace,
  anyCanReadVersion,
  anyCanUpdateEntity,
  conditionally,
 )
import Lib.Sync.Actions.Unsafe.Store (
  unsafeAddMember,
  unsafeStoreActor,
  unsafeStoreEntity,
  unsafeStoreGroup,
  unsafeStoreSpace,
  unsafeStoreVersion,
 )
import Lib.Sync.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Sync.Types.Store (
  Shared,
 )

storeGroup
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -- ^ actor storing the group
  -> GroupId
  -- ^ group being stored
  -> m Bool
storeGroup creator gId =
  anyCanCreateGroup creator
    >>= conditionally (unsafeStoreGroup gId)

storeActor
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -- ^ actor storing the created actor
  -> ActorId
  -- ^ created actor being stored
  -> m Bool
storeActor creator aId =
  anyCanCreateActor creator
    >>= conditionally (unsafeStoreActor aId)

addMember
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -- ^ actor creating membership
  -> GroupId
  -- ^ group gaining a member
  -> ActorId
  -- ^ new member
  -> m Bool
addMember creator gId aId = do
  anyCanCreateMember creator gId
    >>= conditionally (unsafeAddMember gId aId)

storeSpace
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -- ^ actor storing the space
  -> SpaceId
  -- ^ space being created
  -> m Bool
storeSpace creator sId =
  anyCanCreateSpace creator
    >>= conditionally (unsafeStoreSpace sId)

storeEntity
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -- ^ actor storing the entity
  -> EntityId
  -- ^ entity being stored
  -> SpaceId
  -- ^ space in which entity is being stored
  -> VersionId
  -- ^ initial version
  -> Maybe VersionId
  -- ^ forked version
  -> m (Maybe (Either VersionId ()))
storeEntity creator eId sId vId mFork = do
  canAdjust <-
    andM
      [ anyCanCreateEntity creator sId
      , maybe (pure True) (anyCanReadVersion creator) mFork
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeStoreEntity eId sId vId mFork

storeNextVersion
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -- ^ actor attempting to store a version
  -> EntityId
  -- ^ entity receiving a new version
  -> VersionId
  -- ^ version being stored
  -> m (Maybe (Either VersionId ()))
storeNextVersion creator eId vId = do
  canAdjust <- anyCanUpdateEntity creator eId
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeStoreVersion eId vId
