module Lib.Actions.Safe.Store where

import Lib.Actions.Safe.Verify
  ( canCreateGroup
  , canCreateActor
  , canCreateMember
  , canCreateSpace
  , canCreateEntity
  , canUpdateEntity
  , canReadEntity
  , canReadVersion
  , conditionally
  )
import Lib.Actions.Unsafe.Store
  ( unsafeStoreGroup
  , unsafeStoreActor
  , unsafeStoreSpace
  , unsafeStoreEntity
  , unsafeStoreVersion
  , unsafeAddMember
  )
import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Store
  ( Shared
  , store
  , toEntities
  , toVersions
  )
import Lib.Types.Store.Entity (versions)
import Lib.Types.Store.Version (entity)

import qualified Data.List.NonEmpty as NE
import Control.Lens ((^.), at, _Left, (%~), (&))
import Control.Monad.State (MonadState (get))
import Control.Monad.Extra (andM)



storeGroup
  :: MonadState Shared m
  => ActorId -- ^ actor storing the group
  -> GroupId -- ^ group being stored
  -> m Bool
storeGroup creator gId =
  canCreateGroup creator >>=
    conditionally (unsafeStoreGroup gId)

storeActor
  :: MonadState Shared m
  => ActorId -- ^ actor storing the created actor
  -> ActorId -- ^ created actor being stored
  -> m Bool
storeActor creator aId =
  canCreateActor creator >>=
    conditionally (unsafeStoreActor aId)

addMember
  :: MonadState Shared m
  => ActorId -- ^ actor creating membership
  -> GroupId -- ^ group gaining a member
  -> ActorId -- ^ new member
  -> m Bool
addMember creator gId aId = do
  canCreateMember creator gId >>=
    conditionally (unsafeAddMember gId aId)

storeSpace
  :: MonadState Shared m
  => ActorId -- ^ actor storing the space
  -> SpaceId -- ^ space being created
  -> m Bool
storeSpace creator sId =
  canCreateSpace creator >>=
    conditionally (unsafeStoreSpace sId)

storeEntity
  :: MonadState Shared m
  => ActorId -- ^ actor storing the entity
  -> EntityId -- ^ entity being stored
  -> SpaceId -- ^ space in which entity is being stored
  -> VersionId -- ^ initial version
  -> Maybe VersionId -- ^ forked version
  -> m (Maybe (Either VersionId ()))
storeEntity creator eId sId vId mFork = do
  canAdjust <- andM
    [ canCreateEntity creator sId
    , maybe (pure True) (canReadVersion creator) mFork
    ]
  if not canAdjust then pure Nothing else
    Just <$> unsafeStoreEntity eId sId vId mFork

storeNextVersion
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to store a version
  -> EntityId -- ^ entity receiving a new version
  -> VersionId -- ^ version being stored
  -> m (Maybe (Either VersionId ()))
storeNextVersion creator eId vId = do
  canAdjust <- canUpdateEntity creator eId
  if not canAdjust then  pure Nothing else
    Just <$> unsafeStoreVersion eId vId
