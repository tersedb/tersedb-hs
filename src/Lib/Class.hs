module Lib.Class where

import Data.List.NonEmpty (NonEmpty)
import Lib.Types.Id (ActorId, GroupId, SpaceId, EntityId, VersionId)
import qualified Lib.Async.Types.Monad as Async
import qualified Lib.Async.Actions.Safe.Verify as Async
import qualified Lib.Async.Actions.Unsafe.Store as Async
import qualified Lib.Async.Actions.Unsafe.Update as Async
import qualified Lib.Sync.Types.Monad as Sync
import qualified Lib.Sync.Actions.Safe.Verify as Sync
import qualified Lib.Sync.Actions.Unsafe.Store as Sync
import qualified Lib.Sync.Actions.Unsafe.Update as Sync
import Control.Concurrent.STM (STM, atomically)
import Lib.Actions.Safe.Utils (conditionally)
import Control.Monad.Extra (andM)
import Control.Monad.IO.Class (MonadIO)
import System.Random.Stateful (Uniform(uniformM), globalStdGen)
import Data.Maybe.HT (toMaybe)
import Control.Monad.Trans.Control (MonadBaseControl(liftBaseWith))
import Control.Monad.Reader (MonadReader(ask), ReaderT (runReaderT))
import Control.Monad.Base (MonadBase(liftBase))
import DeferredFolds.UnfoldlM (UnfoldlM)
import ListT (ListT)

generateWithAuthority
  :: ( MonadIO m
     , Uniform a
     )
  => (a -> m Bool)
  -> m (Maybe a)
generateWithAuthority perform = do
  ident <- uniformM globalStdGen
  worked <- perform ident
  pure (toMaybe worked ident)

class Monad m => TerseDBGen m where
  newActor :: NonEmpty ActorId -> m (Maybe ActorId)
  newGroup :: NonEmpty ActorId -> m (Maybe GroupId)
  newSpace :: NonEmpty ActorId -> m (Maybe SpaceId)
  newEntity :: NonEmpty ActorId -> SpaceId -> Maybe VersionId -> m (Maybe (EntityId, VersionId))
  newVersion :: NonEmpty ActorId -> EntityId -> m (Maybe VersionId)

instance TerseDBGen (Sync.TerseM IO) where
  newGroup = generateWithAuthority . storeGroup
  newActor = generateWithAuthority . storeActor
  newSpace = generateWithAuthority . storeSpace
  newEntity creator sId mFork =
    generateWithAuthority (\(eId, vId) -> storeEntity creator eId sId vId mFork)
  newVersion creator eId =
    generateWithAuthority (storeNextVersion creator eId)

instance TerseDBGen (Async.TerseM IO) where
  newGroup creator = generateWithAuthority $ \gId -> do
    s <- ask
    liftBase . atomically $ runReaderT (storeGroup creator gId) s
  newActor creator = generateWithAuthority $ \aId -> do
    s <- ask
    liftBase . atomically $ runReaderT (storeActor creator aId) s
  newSpace creator = generateWithAuthority $ \sId -> do
    s <- ask
    liftBase . atomically $ runReaderT (storeSpace creator sId) s
  newEntity creator sId mFork = generateWithAuthority $ \(eId, vId) -> do
    s <- ask
    liftBase . atomically $ runReaderT (storeEntity creator eId sId vId mFork) s
  newVersion creator eId = generateWithAuthority $ \vId -> do
    s <- ask
    liftBase . atomically $ runReaderT (storeNextVersion creator eId vId) s


class Monad m => TerseDB m where
  anyCanReadActor :: NonEmpty ActorId -> m Bool
  anyCanCreateActor :: NonEmpty ActorId -> m Bool
  anyCanUpdateActor :: NonEmpty ActorId -> ActorId -> m Bool
  anyCanDeleteActor :: NonEmpty ActorId -> ActorId -> m Bool
  anyCanReadGroup :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanCreateGroup :: NonEmpty ActorId -> m Bool
  anyCanUpdateGroup :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanDeleteGroup :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanReadMember :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanCreateMember :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanUpdateMember :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanDeleteMember :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanReadSpace :: NonEmpty ActorId -> SpaceId -> m Bool
  anyCanCreateSpace :: NonEmpty ActorId -> m Bool
  anyCanUpdateSpace :: NonEmpty ActorId -> SpaceId -> m Bool
  anyCanDeleteSpace :: NonEmpty ActorId -> SpaceId -> m Bool
  anyCanReadEntity :: NonEmpty ActorId -> EntityId -> m Bool
  anyCanCreateEntity :: NonEmpty ActorId -> SpaceId -> m Bool
  anyCanUpdateEntity :: NonEmpty ActorId -> EntityId -> m Bool
  anyCanDeleteEntity :: NonEmpty ActorId -> EntityId -> m Bool
  anyCanReadVersion :: NonEmpty ActorId -> VersionId -> m Bool
  anyCanCreateVersion :: NonEmpty ActorId -> EntityId -> m Bool
  anyCanUpdateVersion :: NonEmpty ActorId -> VersionId -> m Bool
  anyCanDeleteVersion :: NonEmpty ActorId -> VersionId -> m Bool
  unsafeReadReferencesEager :: VersionId -> UnfoldlM m VersionId
  unsafeReadReferencesLazy :: VersionId -> ListT m VersionId
  unsafeReadReferencesFromEager :: VersionId -> UnfoldlM m VersionId
  unsafeReadReferencesFromLazy :: VersionId -> ListT m VersionId
  unsafeReadSubscriptionsEager :: VersionId -> UnfoldlM m EntityId
  unsafeReadSubscriptionsLazy :: VersionId -> ListT m EntityId
  unsafeReadSubscriptionsFromEager :: VersionId -> UnfoldlM m EntityId
  unsafeReadSubscriptionsFromLazy :: VersionId -> ListT m EntityId
  unsafeReadEntitiesEager :: VersionId -> UnfoldlM m EntityId
  unsafeReadEntitiesLazy :: VersionId -> ListT m EntityId
  unsafeStoreGroup :: GroupId -> m ()
  unsafeStoreActor :: ActorId -> m ()
  unsafeAddMember :: GroupId -> ActorId -> m ()
  unsafeStoreSpace :: SpaceId -> m ()
  unsafeStoreEntity :: EntityId -> SpaceId -> VersionId -> Maybe VersionId -> m ()
  unsafeStoreVersion :: EntityId -> VersionId -> m ()
  unsafeAddReference :: VersionId -> VersionId -> m ()
  unsafeRemoveReference :: VersionId -> VersionId -> m ()
  unsafeAddSubscription :: VersionId -> EntityId -> m ()
  unsafeRemoveSubscription :: VersionId -> EntityId -> m ()
  unsafeUpdateFork :: EntityId -> Maybe VersionId -> m ()
  unsafeMoveEntity :: EntityId -> SpaceId -> m ()
  unsafeOffsetVersionIndex :: VersionId -> Int -> m ()
  unsafeSetVersionIndex :: VersionId -> Int -> m ()
  unsafeRemoveVersion :: VersionId -> m ()
  unsafeRemoveEntity :: EntityId -> m ()
  unsafeRemoveSpace :: SpaceId -> m ()
  unsafeRemoveMember :: GroupId -> ActorId -> m ()
  unsafeRemoveActor :: ActorId -> m ()
  unsafeRemoveGroup :: GroupId -> m ()

instance Monad m => TerseDB (Sync.TerseM m) where
  anyCanReadActor = Sync.anyCanReadActor
  anyCanCreateActor = Sync.anyCanCreateActor
  anyCanUpdateActor = Sync.anyCanUpdateActor
  anyCanDeleteActor = Sync.anyCanDeleteActor
  anyCanReadGroup = Sync.anyCanReadGroup
  anyCanCreateGroup = Sync.anyCanCreateGroup
  anyCanUpdateGroup = Sync.anyCanUpdateGroup
  anyCanDeleteGroup = Sync.anyCanDeleteGroup
  anyCanReadMember = Sync.anyCanReadMember
  anyCanCreateMember = Sync.anyCanCreateMember
  anyCanUpdateMember = Sync.anyCanUpdateMember
  anyCanDeleteMember = Sync.anyCanDeleteMember
  anyCanReadSpace = Sync.anyCanReadSpace
  anyCanCreateSpace = Sync.anyCanCreateSpace
  anyCanUpdateSpace = Sync.anyCanUpdateSpace
  anyCanDeleteSpace = Sync.anyCanDeleteSpace
  anyCanReadEntity = Sync.anyCanReadEntity
  anyCanCreateEntity = Sync.anyCanCreateEntity
  anyCanUpdateEntity = Sync.anyCanUpdateEntity
  anyCanDeleteEntity = Sync.anyCanDeleteEntity
  anyCanReadVersion = Sync.anyCanReadVersion
  anyCanCreateVersion = Sync.anyCanCreateVersion
  anyCanUpdateVersion = Sync.anyCanUpdateVersion
  anyCanDeleteVersion = Sync.anyCanDeleteVersion
  unsafeStoreGroup = Sync.unsafeStoreGroup
  unsafeStoreActor = Sync.unsafeStoreActor
  unsafeAddMember = Sync.unsafeAddMember
  unsafeStoreSpace = Sync.unsafeStoreSpace
  unsafeStoreEntity = Sync.unsafeStoreEntity
  unsafeStoreVersion = Sync.unsafeStoreVersion
  unsafeAddReference = Sync.unsafeAddReference
  unsafeRemoveReference = Sync.unsafeRemoveReference
  unsafeAddSubscription = Sync.unsafeAddSubscription
  unsafeRemoveSubscription = Sync.unsafeRemoveSubscription
  unsafeUpdateFork = Sync.unsafeUpdateFork
  unsafeMoveEntity = Sync.unsafeMoveEntity
  unsafeOffsetVersionIndex = Sync.unsafeOffsetVersionIndex
  unsafeSetVersionIndex = Sync.unsafeSetVersionIndex

instance TerseDB (Async.TerseM STM) where
  anyCanReadActor = Async.anyCanReadActor
  anyCanCreateActor = Async.anyCanCreateActor
  anyCanUpdateActor = Async.anyCanUpdateActor
  anyCanDeleteActor = Async.anyCanDeleteActor
  anyCanReadGroup = Async.anyCanReadGroup
  anyCanCreateGroup = Async.anyCanCreateGroup
  anyCanUpdateGroup = Async.anyCanUpdateGroup
  anyCanDeleteGroup = Async.anyCanDeleteGroup
  anyCanReadMember = Async.anyCanReadMember
  anyCanCreateMember = Async.anyCanCreateMember
  anyCanUpdateMember = Async.anyCanUpdateMember
  anyCanDeleteMember = Async.anyCanDeleteMember
  anyCanReadSpace = Async.anyCanReadSpace
  anyCanCreateSpace = Async.anyCanCreateSpace
  anyCanUpdateSpace = Async.anyCanUpdateSpace
  anyCanDeleteSpace = Async.anyCanDeleteSpace
  anyCanReadEntity = Async.anyCanReadEntity
  anyCanCreateEntity = Async.anyCanCreateEntity
  anyCanUpdateEntity = Async.anyCanUpdateEntity
  anyCanDeleteEntity = Async.anyCanDeleteEntity
  anyCanReadVersion = Async.anyCanReadVersion
  anyCanCreateVersion = Async.anyCanCreateVersion
  anyCanUpdateVersion = Async.anyCanUpdateVersion
  anyCanDeleteVersion = Async.anyCanDeleteVersion
  unsafeStoreGroup = Async.unsafeStoreGroup
  unsafeStoreActor = Async.unsafeStoreActor
  unsafeAddMember = Async.unsafeAddMember
  unsafeStoreSpace = Async.unsafeStoreSpace
  unsafeStoreEntity = Async.unsafeStoreEntity
  unsafeStoreVersion = Async.unsafeStoreVersion
  unsafeAddReference = Async.unsafeAddReference
  unsafeRemoveReference = Async.unsafeRemoveReference
  unsafeAddSubscription = Async.unsafeAddSubscription
  unsafeRemoveSubscription = Async.unsafeRemoveSubscription
  unsafeUpdateFork = Async.unsafeUpdateFork
  unsafeMoveEntity = Async.unsafeMoveEntity
  unsafeOffsetVersionIndex = Async.unsafeOffsetVersionIndex
  unsafeSetVersionIndex = Async.unsafeSetVersionIndex

storeGroup
  :: (TerseDB m)
  => NonEmpty ActorId
  -- ^ actor storing the group
  -> GroupId
  -- ^ group being stored
  -> m Bool
storeGroup creator gId =
  anyCanCreateGroup creator
    >>= conditionally (unsafeStoreGroup gId)

storeActor
  :: (TerseDB m)
  => NonEmpty ActorId
  -- ^ actor storing the created actor
  -> ActorId
  -- ^ created actor being stored
  -> m Bool
storeActor creator aId =
  anyCanCreateActor creator
    >>= conditionally (unsafeStoreActor aId)

addMember
  :: (TerseDB m)
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
  :: (TerseDB m)
  => NonEmpty ActorId
  -- ^ actor storing the space
  -> SpaceId
  -- ^ space being created
  -> m Bool
storeSpace creator sId =
  anyCanCreateSpace creator
    >>= conditionally (unsafeStoreSpace sId)

storeEntity
  :: (TerseDB m)
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
  -> m Bool
storeEntity creator eId sId vId mFork = do
  canAdjust <-
    andM
      [ anyCanCreateEntity creator sId
      , maybe (pure True) (anyCanReadVersion creator) mFork
      ]
  conditionally (unsafeStoreEntity eId sId vId mFork) canAdjust

storeNextVersion
  :: (TerseDB m)
  => NonEmpty ActorId
  -- ^ actor attempting to store a version
  -> EntityId
  -- ^ entity receiving a new version
  -> VersionId
  -- ^ version being stored
  -> m Bool
storeNextVersion creator eId vId = do
  anyCanUpdateEntity creator eId
    >>= conditionally (unsafeStoreVersion eId vId)

-- | Moving an entity between spaces requires delete authority on the current space, and create authority on the destination space
moveEntity
  :: (TerseDB m) => NonEmpty ActorId -> EntityId -> SpaceId -> m Bool
moveEntity updater eId newSId = do
  canAdjust <-
    andM
      [ anyCanDeleteEntity updater eId
      , anyCanCreateEntity updater newSId
      ]
  conditionally (unsafeMoveEntity eId newSId) canAdjust

addReference
  :: (TerseDB m)
  => NonEmpty ActorId
  -> VersionId
  -> VersionId
  -> m Bool 
addReference updater vId refId = do
  canAdjust <-
    andM
      [ anyCanReadVersion updater refId
      , anyCanUpdateVersion updater vId
      ]
  conditionally (unsafeAddReference vId refId) canAdjust

removeReference
  :: (TerseDB m)
  => NonEmpty ActorId
  -> VersionId
  -> VersionId
  -> m Bool
removeReference updater vId refId = do
  canAdjust <-
    andM
      [ anyCanReadVersion updater refId
      , anyCanUpdateVersion updater vId
      ]
  conditionally (unsafeRemoveReference vId refId) canAdjust

addSubscription
  :: (TerseDB m)
  => NonEmpty ActorId
  -> VersionId
  -> EntityId
  -> m Bool
addSubscription updater vId subId = do
  canAdjust <-
    andM
      [ anyCanReadEntity updater subId
      , anyCanUpdateVersion updater vId
      ]
  conditionally (unsafeAddSubscription vId subId) canAdjust

removeSubscription
  :: (TerseDB m)
  => NonEmpty ActorId
  -> VersionId
  -> EntityId
  -> m Bool
removeSubscription updater vId subId = do
  canAdjust <-
    andM
      [ anyCanReadEntity updater subId
      , anyCanUpdateVersion updater vId
      ]
  conditionally (unsafeRemoveSubscription vId subId) canAdjust

updateFork
  :: (TerseDB m)
  => NonEmpty ActorId
  -> EntityId
  -> Maybe VersionId
  -> m Bool
updateFork updater eId mFork = do
  canAdjust <-
    andM
      [ anyCanUpdateEntity updater eId
      , maybe (pure True) (anyCanReadVersion updater) mFork
      ]
  conditionally (unsafeUpdateFork eId mFork) canAdjust

offsetVersionIndex
  :: (TerseDB m)
  => NonEmpty ActorId
  -> VersionId
  -> Int
  -> m Bool
offsetVersionIndex updater vId offset = do
  anyCanUpdateVersion updater vId
    >>= conditionally (unsafeOffsetVersionIndex vId offset)

setVersionIndex
  :: (TerseDB m)
  => NonEmpty ActorId
  -> VersionId
  -> Int
  -> m Bool
setVersionIndex updater vId idx = do
  anyCanUpdateVersion updater vId
    >>= conditionally (unsafeSetVersionIndex vId idx)
