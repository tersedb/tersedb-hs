module Lib.Async.Actions.Unsafe.Read where

import Lib.Types.Id (VersionId, EntityId, SpaceId)
import Lib.Async.Types.Monad (TerseM)
import Control.Concurrent.STM (STM, TVar, newTVar, writeTVar, readTVar)
import DeferredFolds.UnfoldlM (UnfoldlM (UnfoldlM))
import Lib.Async.Types.Store (toReferences, store, temp, toReferencesFrom, toSubscriptions, toSubscriptionsFrom, toEntities, toSpaceEntities)
import qualified StmContainers.Multimap as Multimap
import Control.Lens ((^.))
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import Control.Monad.Reader (MonadReader(ask), ReaderT (runReaderT), MonadIO)
import Control.Monad.Base (MonadBase(liftBase))
import ListT (ListT)
import qualified ListT
import GHC.Generics (Generic)
import Control.Monad.Morph (hoist)

newtype ReadingStateT s m a = ReadingStateT
  { runReadingStateT :: ReaderT s m a
  } deriving (Generic, Functor, Applicative, Monad)
deriving instance MonadBase b m => MonadBase b (ReadingStateT s m)

ask' :: Monad m => ReadingStateT s m s
ask' = ReadingStateT ask

unsafeReadReferencesEager :: VersionId -> UnfoldlM (TerseM STM) VersionId
unsafeReadReferencesEager vId = UnfoldlM $ \f acc -> do
  s <- ask
  let refs :: UnfoldlM STM VersionId
      refs = Multimap.unfoldlMByKey vId (s ^. store . toReferences)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  UnfoldlM.foldlM' f acc . UnfoldlM.hoist lifter reducer $ refs

unsafeReadReferencesLazy :: VersionId -> ListT (TerseM STM) VersionId
unsafeReadReferencesLazy vId = hoist loadRefs $ ListT.unfoldM go ()
  where
    loadRefs :: forall a. ReadingStateT (TVar (ListT STM VersionId)) (TerseM STM) a -> TerseM STM a
    loadRefs m = do
      s <- ask
      refsVar <- liftBase . newTVar $ Multimap.listTByKey vId (s ^. store . toReferences)
      runReaderT (runReadingStateT m) refsVar
    go :: () -> ReadingStateT (TVar (ListT STM VersionId)) (TerseM STM) (Maybe (VersionId, ()))
    go _ = do
      refsVar <- ask'
      liftBase $ do
        refs <- readTVar refsVar
        mNext <- ListT.uncons refs
        case mNext of
          Nothing -> pure Nothing
          Just (x, next) -> do
            writeTVar refsVar next
            pure (Just (x, ()))

unsafeReadReferencesFromEager :: VersionId -> UnfoldlM (TerseM STM) VersionId
unsafeReadReferencesFromEager vId = UnfoldlM $ \f acc -> do
  s <- ask
  let refs :: UnfoldlM STM VersionId
      refs = Multimap.unfoldlMByKey vId (s ^. temp . toReferencesFrom)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  UnfoldlM.foldlM' f acc . UnfoldlM.hoist lifter reducer $ refs

unsafeReadReferencesFromLazy :: VersionId -> ListT (TerseM STM) VersionId
unsafeReadReferencesFromLazy vId = hoist loadRefs $ ListT.unfoldM go ()
  where
    loadRefs :: forall a. ReadingStateT (TVar (ListT STM VersionId)) (TerseM STM) a -> TerseM STM a
    loadRefs m = do
      s <- ask
      refsVar <- liftBase . newTVar $ Multimap.listTByKey vId (s ^. temp . toReferencesFrom)
      runReaderT (runReadingStateT m) refsVar
    go :: () -> ReadingStateT (TVar (ListT STM VersionId)) (TerseM STM) (Maybe (VersionId, ()))
    go _ = do
      refsVar <- ask'
      liftBase $ do
        refs <- readTVar refsVar
        mNext <- ListT.uncons refs
        case mNext of
          Nothing -> pure Nothing
          Just (x, next) -> do
            writeTVar refsVar next
            pure (Just (x, ()))

unsafeReadSubscriptionsEager :: VersionId -> UnfoldlM (TerseM STM) EntityId
unsafeReadSubscriptionsEager vId = UnfoldlM $ \f acc -> do
  s <- ask
  let subs :: UnfoldlM STM EntityId
      subs = Multimap.unfoldlMByKey vId (s ^. store . toSubscriptions)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  UnfoldlM.foldlM' f acc . UnfoldlM.hoist lifter reducer $ subs

unsafeReadSubscriptionsLazy :: VersionId -> ListT (TerseM STM) EntityId
unsafeReadSubscriptionsLazy vId = hoist loadsubs $ ListT.unfoldM go ()
  where
    loadsubs :: forall a. ReadingStateT (TVar (ListT STM EntityId)) (TerseM STM) a -> TerseM STM a
    loadsubs m = do
      s <- ask
      subsVar <- liftBase . newTVar $ Multimap.listTByKey vId (s ^. store . toSubscriptions)
      runReaderT (runReadingStateT m) subsVar
    go :: () -> ReadingStateT (TVar (ListT STM EntityId)) (TerseM STM) (Maybe (EntityId, ()))
    go _ = do
      subsVar <- ask'
      liftBase $ do
        subs <- readTVar subsVar
        mNext <- ListT.uncons subs
        case mNext of
          Nothing -> pure Nothing
          Just (x, next) -> do
            writeTVar subsVar next
            pure (Just (x, ()))

unsafeReadSubscriptionsFromEager :: EntityId -> UnfoldlM (TerseM STM) VersionId
unsafeReadSubscriptionsFromEager eId = UnfoldlM $ \f acc -> do
  s <- ask
  let subs :: UnfoldlM STM VersionId
      subs = Multimap.unfoldlMByKey eId (s ^. temp . toSubscriptionsFrom)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  UnfoldlM.foldlM' f acc . UnfoldlM.hoist lifter reducer $ subs

unsafeReadSubscriptionsFromLazy :: EntityId -> ListT (TerseM STM) VersionId
unsafeReadSubscriptionsFromLazy eId = hoist loadsubs $ ListT.unfoldM go ()
  where
    loadsubs :: forall a. ReadingStateT (TVar (ListT STM VersionId)) (TerseM STM) a -> TerseM STM a
    loadsubs m = do
      s <- ask
      subsVar <- liftBase . newTVar $ Multimap.listTByKey eId (s ^. temp . toSubscriptionsFrom)
      runReaderT (runReadingStateT m) subsVar
    go :: () -> ReadingStateT (TVar (ListT STM VersionId)) (TerseM STM) (Maybe (VersionId, ()))
    go _ = do
      subsVar <- ask'
      liftBase $ do
        subs <- readTVar subsVar
        mNext <- ListT.uncons subs
        case mNext of
          Nothing -> pure Nothing
          Just (x, next) -> do
            writeTVar subsVar next
            pure (Just (x, ()))

unsafeReadEntitiesEager :: SpaceId -> UnfoldlM (TerseM STM) EntityId
unsafeReadEntitiesEager sId = UnfoldlM $ \f acc -> do
  s <- ask
  let subs :: UnfoldlM STM EntityId
      subs = Multimap.unfoldlMByKey sId (s ^. store . toSpaceEntities)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  UnfoldlM.foldlM' f acc . UnfoldlM.hoist lifter reducer $ subs

unsafeReadEntitiesLazy :: SpaceId -> ListT (TerseM STM) EntityId
unsafeReadEntitiesLazy sId = hoist loadsubs $ ListT.unfoldM go ()
  where
    loadsubs :: forall a. ReadingStateT (TVar (ListT STM EntityId)) (TerseM STM) a -> TerseM STM a
    loadsubs m = do
      s <- ask
      subsVar <- liftBase . newTVar $ Multimap.listTByKey sId (s ^. store . toSpaceEntities)
      runReaderT (runReadingStateT m) subsVar
    go :: () -> ReadingStateT (TVar (ListT STM EntityId)) (TerseM STM) (Maybe (EntityId, ()))
    go _ = do
      subsVar <- ask'
      liftBase $ do
        subs <- readTVar subsVar
        mNext <- ListT.uncons subs
        case mNext of
          Nothing -> pure Nothing
          Just (x, next) -> do
            writeTVar subsVar next
            pure (Just (x, ()))
