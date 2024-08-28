module Lib.Async.Actions.Unsafe.Read where

import Control.Concurrent.STM (STM)
import Control.Lens ((^.))
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import DeferredFolds.UnfoldlM (UnfoldlM)
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import GHC.Generics (Generic)
import Lib.Async.Types.Monad (TerseM)
import Lib.Async.Types.Store (
  store,
  temp,
  toReferences,
  toReferencesFrom,
  toSpaceEntities,
  toSubscriptions,
  toSubscriptionsFrom,
 )
import Lib.Types.Id (EntityId, SpaceId, VersionId)
import ListT (ListT)
import qualified StmContainers.Multimap as Multimap

newtype ReadingStateT s m a = ReadingStateT
  { runReadingStateT :: ReaderT s m a
  }
  deriving (Generic, Functor, Applicative, Monad)
deriving instance (MonadBase b m) => MonadBase b (ReadingStateT s m)

ask' :: (Monad m) => ReadingStateT s m s
ask' = ReadingStateT ask

unsafeReadReferencesEager :: VersionId -> TerseM STM (UnfoldlM (TerseM STM) VersionId)
unsafeReadReferencesEager vId = do
  s <- ask
  let refs :: UnfoldlM STM VersionId
      refs = Multimap.unfoldlMByKey vId (s ^. store . toReferences)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  pure . UnfoldlM.hoist lifter reducer $ refs

unsafeReadReferencesLazy
  :: VersionId -> TerseM STM (ListT (TerseM STM) VersionId)
unsafeReadReferencesLazy vId = do
  s <- ask
  let refs = Multimap.listTByKey vId (s ^. store . toReferences)
  pure (hoist liftBase refs)

unsafeReadReferencesFromEager :: VersionId -> TerseM STM (UnfoldlM (TerseM STM) VersionId)
unsafeReadReferencesFromEager vId = do
  s <- ask
  let refs :: UnfoldlM STM VersionId
      refs = Multimap.unfoldlMByKey vId (s ^. temp . toReferencesFrom)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  pure . UnfoldlM.hoist lifter reducer $ refs

unsafeReadReferencesFromLazy
  :: VersionId -> TerseM STM (ListT (TerseM STM) VersionId)
unsafeReadReferencesFromLazy vId = do
  s <- ask
  let refs = Multimap.listTByKey vId (s ^. temp . toReferencesFrom)
  pure (hoist liftBase refs)

unsafeReadSubscriptionsEager :: VersionId -> TerseM STM (UnfoldlM (TerseM STM) EntityId)
unsafeReadSubscriptionsEager vId = do
  s <- ask
  let subs :: UnfoldlM STM EntityId
      subs = Multimap.unfoldlMByKey vId (s ^. store . toSubscriptions)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  pure . UnfoldlM.hoist lifter reducer $ subs

unsafeReadSubscriptionsLazy
  :: VersionId -> TerseM STM (ListT (TerseM STM) EntityId)
unsafeReadSubscriptionsLazy vId = do
  s <- ask
  let subs = Multimap.listTByKey vId (s ^. store . toSubscriptions)
  pure (hoist liftBase subs)

unsafeReadSubscriptionsFromEager :: EntityId -> TerseM STM (UnfoldlM (TerseM STM) VersionId)
unsafeReadSubscriptionsFromEager eId = do
  s <- ask
  let subs :: UnfoldlM STM VersionId
      subs = Multimap.unfoldlMByKey eId (s ^. temp . toSubscriptionsFrom)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  pure . UnfoldlM.hoist lifter reducer $ subs

unsafeReadSubscriptionsFromLazy
  :: EntityId -> TerseM STM (ListT (TerseM STM) VersionId)
unsafeReadSubscriptionsFromLazy eId = do
  s <- ask
  let subs = Multimap.listTByKey eId (s ^. temp . toSubscriptionsFrom)
  pure (hoist liftBase subs)

unsafeReadEntitiesEager :: SpaceId -> TerseM STM (UnfoldlM (TerseM STM) EntityId)
unsafeReadEntitiesEager sId = do
  s <- ask
  let subs :: UnfoldlM STM EntityId
      subs = Multimap.unfoldlMByKey sId (s ^. store . toSpaceEntities)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  pure . UnfoldlM.hoist lifter reducer $ subs

unsafeReadEntitiesLazy :: SpaceId -> TerseM STM (ListT (TerseM STM) EntityId)
unsafeReadEntitiesLazy sId = do
  s <- ask
  let subs = Multimap.listTByKey sId (s ^. store . toSpaceEntities)
  pure (hoist liftBase subs)
