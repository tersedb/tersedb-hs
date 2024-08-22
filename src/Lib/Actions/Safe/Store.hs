module Lib.Actions.Safe.Store where

import Lib.Actions.Safe.Verify (
  anyCanCreateActor,
  anyCanCreateEntity,
  anyCanCreateGroup,
  anyCanCreateMember,
  anyCanCreateSpace,
  anyCanReadVersion,
  anyCanUpdateEntity,
  conditionally,
 )
import Lib.Actions.Unsafe.Store (
  unsafeAddMember,
  unsafeStoreActor,
  unsafeStoreEntity,
  unsafeStoreGroup,
  unsafeStoreSpace,
  unsafeStoreVersion,
 )
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Store (
  Shared,
 )
import Data.List.NonEmpty (NonEmpty)
import Control.Monad.Extra (andM)
import Control.Monad.State (MonadState)

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
