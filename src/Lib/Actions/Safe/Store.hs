module Lib.Actions.Safe.Store where

import Lib.Actions.Safe.Verify (
    canCreateActor,
    canCreateEntity,
    canCreateGroup,
    canCreateMember,
    canCreateSpace,
    canReadEntity,
    canReadVersion,
    canUpdateEntity,
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
    store,
    toEntities,
    toVersions,
 )
import Lib.Types.Store.Entity (versions)
import Lib.Types.Store.Version (entity)

import Control.Lens (at, (%~), (&), (^.), _Left)
import Control.Monad.Extra (andM)
import Control.Monad.State (MonadState (get))
import qualified Data.List.NonEmpty as NE

storeGroup ::
    (MonadState Shared m) =>
    -- | actor storing the group
    ActorId ->
    -- | group being stored
    GroupId ->
    m Bool
storeGroup creator gId =
    canCreateGroup creator
        >>= conditionally (unsafeStoreGroup gId)

storeActor ::
    (MonadState Shared m) =>
    -- | actor storing the created actor
    ActorId ->
    -- | created actor being stored
    ActorId ->
    m Bool
storeActor creator aId =
    canCreateActor creator
        >>= conditionally (unsafeStoreActor aId)

addMember ::
    (MonadState Shared m) =>
    -- | actor creating membership
    ActorId ->
    -- | group gaining a member
    GroupId ->
    -- | new member
    ActorId ->
    m Bool
addMember creator gId aId = do
    canCreateMember creator gId
        >>= conditionally (unsafeAddMember gId aId)

storeSpace ::
    (MonadState Shared m) =>
    -- | actor storing the space
    ActorId ->
    -- | space being created
    SpaceId ->
    m Bool
storeSpace creator sId =
    canCreateSpace creator
        >>= conditionally (unsafeStoreSpace sId)

storeEntity ::
    (MonadState Shared m) =>
    -- | actor storing the entity
    ActorId ->
    -- | entity being stored
    EntityId ->
    -- | space in which entity is being stored
    SpaceId ->
    -- | initial version
    VersionId ->
    -- | forked version
    Maybe VersionId ->
    m (Maybe (Either VersionId ()))
storeEntity creator eId sId vId mFork = do
    canAdjust <-
        andM
            [ canCreateEntity creator sId
            , maybe (pure True) (canReadVersion creator) mFork
            ]
    if not canAdjust
        then pure Nothing
        else Just <$> unsafeStoreEntity eId sId vId mFork

storeNextVersion ::
    (MonadState Shared m) =>
    -- | actor attempting to store a version
    ActorId ->
    -- | entity receiving a new version
    EntityId ->
    -- | version being stored
    VersionId ->
    m (Maybe (Either VersionId ()))
storeNextVersion creator eId vId = do
    canAdjust <- canUpdateEntity creator eId
    if not canAdjust
        then pure Nothing
        else Just <$> unsafeStoreVersion eId vId
