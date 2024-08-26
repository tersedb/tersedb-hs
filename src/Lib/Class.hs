module Lib.Class where

import Data.List.NonEmpty (NonEmpty)
import Lib.Types.Id (ActorId, GroupId, SpaceId, EntityId, VersionId)
import qualified Lib.Async.Types.Monad as Async
import qualified Lib.Async.Actions.Safe.Verify as Async
import qualified Lib.Async.Actions.Unsafe.Store as Async
import qualified Lib.Sync.Types.Monad as Sync
import qualified Lib.Sync.Actions.Safe.Verify as Sync
import qualified Lib.Sync.Actions.Unsafe.Store as Sync
import Data.Functor.Identity (Identity)
import Control.Concurrent.STM (STM, atomically)
import Lib.Actions.Safe.Utils (conditionally)
import Control.Monad.Extra (andM)
import Control.Monad.IO.Class (MonadIO)
import System.Random.Stateful (Uniform(uniformM), globalStdGen)
import Data.Maybe.HT (toMaybe)
import Control.Monad.Trans.Control (MonadBaseControl(liftBaseWith))
import Control.Monad.State (StateT(runStateT))
import Control.Monad.Reader (MonadReader(ask), ReaderT (runReaderT))
import Control.Monad.Base (MonadBase(liftBase))

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
  unsafeStoreGroup :: GroupId -> m ()
  unsafeStoreActor :: ActorId -> m ()
  unsafeAddMember :: GroupId -> ActorId -> m ()
  unsafeStoreSpace :: SpaceId -> m ()
  unsafeStoreEntity :: EntityId -> SpaceId -> VersionId -> Maybe VersionId -> m ()
  unsafeStoreVersion :: EntityId -> VersionId -> m ()

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
