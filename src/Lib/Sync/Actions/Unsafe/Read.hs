module Lib.Sync.Actions.Unsafe.Read where

import Lib.Types.Id (VersionId, EntityId, SpaceId)
import DeferredFolds.UnfoldlM (UnfoldlM (..))
import Lib.Sync.Types.Store (Shared, temp, toSubscriptionsFrom, toReferencesFrom, store, toVersions, toSpaces)
import Control.Monad.State (MonadState (get, put), StateT, MonadIO, evalStateT)
import Control.Lens ((^.), at, non, ix, (^?))
import Data.Foldable (foldlM)
import qualified ListT
import qualified Data.HashSet as HS
import ListT (ListT)
import Lib.Sync.Types.Store.Version (references, subscriptions)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Control.Monad.Base (MonadBase)
import Control.Monad.Morph (hoist)


newtype ReadingStateT s m a = ReadingStateT
  { runReadingStateT :: StateT s m a
  } deriving (Generic, Functor, Applicative, Monad, MonadIO)

get' :: Monad m => ReadingStateT s m s
get' = ReadingStateT get

put' :: Monad m => s -> ReadingStateT s m ()
put' s = ReadingStateT (put s)


unsafeReadReferencesEager :: MonadState Shared m => VersionId -> UnfoldlM m VersionId
unsafeReadReferencesEager vId = UnfoldlM $ \f acc -> do
  s <- get
  let refs = fromMaybe mempty $ s ^? store . toVersions . ix vId . references
  foldlM f acc refs


unsafeReadReferencesLazy :: forall m. MonadState Shared m => VersionId -> ListT m VersionId
unsafeReadReferencesLazy vId = hoist loadRefs $ ListT.unfoldM go ()
  where
    loadRefs :: forall a. ReadingStateT [VersionId] m a -> m a
    loadRefs m = do
      s <- get
      let refs :: [VersionId]
          refs = HS.toList . fromMaybe mempty $ s ^? store . toVersions . ix vId . references
      evalStateT (runReadingStateT m) refs
    go _ = do
      refs <- get'
      if null refs
      then pure Nothing
      else do
        put' (tail refs)
        pure (Just (head refs, ()))

unsafeReadReferencesFromEager :: MonadState Shared m => VersionId -> UnfoldlM m VersionId
unsafeReadReferencesFromEager vId = UnfoldlM $ \f acc -> do
  s <- get
  let refs = s ^. temp . toReferencesFrom . at vId . non mempty
  foldlM f acc refs


unsafeReadReferencesFromLazy :: forall m. MonadState Shared m => VersionId -> ListT m VersionId
unsafeReadReferencesFromLazy vId = hoist loadRefs $ ListT.unfoldM go ()
  where
    loadRefs :: forall a. ReadingStateT [VersionId] m a -> m a
    loadRefs m = do
      s <- get
      let refs :: [VersionId]
          refs = HS.toList $ s ^. temp . toReferencesFrom . at vId . non mempty
      evalStateT (runReadingStateT m) refs
    go _ = do
      refs <- get'
      if null refs
      then pure Nothing
      else do
        put' (tail refs)
        pure (Just (head refs, ()))

unsafeReadSubscriptionsEager :: MonadState Shared m => VersionId -> UnfoldlM m EntityId
unsafeReadSubscriptionsEager vId = UnfoldlM $ \f acc -> do
  s <- get
  let subs = fromMaybe mempty $ s ^? store . toVersions . ix vId . subscriptions
  foldlM f acc subs


unsafeReadSubscriptionsLazy :: forall m. MonadState Shared m => VersionId -> ListT m EntityId
unsafeReadSubscriptionsLazy vId = hoist loadSubs $ ListT.unfoldM go ()
  where
    loadSubs :: forall a. ReadingStateT [EntityId] m a -> m a
    loadSubs m = do
      s <- get
      let subs :: [EntityId]
          subs = HS.toList . fromMaybe mempty $ s ^? store . toVersions . ix vId . subscriptions
      evalStateT (runReadingStateT m) subs
    go _ = do
      subs <- get'
      if null subs
      then pure Nothing
      else do
        put' (tail subs)
        pure (Just (head subs, ()))

unsafeReadSubscriptionsFromEager :: MonadState Shared m => EntityId -> UnfoldlM m VersionId
unsafeReadSubscriptionsFromEager eId = UnfoldlM $ \f acc -> do
  s <- get
  let subs = s ^. temp . toSubscriptionsFrom . at eId . non mempty
  foldlM f acc subs


unsafeReadSubscriptionsFromLazy :: forall m. MonadState Shared m => EntityId -> ListT m VersionId
unsafeReadSubscriptionsFromLazy eId = hoist loadSubs $ ListT.unfoldM go ()
  where
    loadSubs :: forall a. ReadingStateT [VersionId] m a -> m a
    loadSubs m = do
      s <- get
      let subs :: [VersionId]
          subs = HS.toList $ s ^. temp . toSubscriptionsFrom . at eId . non mempty
      evalStateT (runReadingStateT m) subs
    go _ = do
      subs <- get'
      if null subs
      then pure Nothing
      else do
        put' (tail subs)
        pure (Just (head subs, ()))

unsafeReadEntitiesEager :: MonadState Shared m => SpaceId -> UnfoldlM m EntityId
unsafeReadEntitiesEager sId = UnfoldlM $ \f acc -> do
  s <- get
  let es = s ^. store . toSpaces . at sId . non mempty
  foldlM f acc es

unsafeReadEntitiesLazy :: forall m. MonadState Shared m => SpaceId -> ListT m EntityId
unsafeReadEntitiesLazy sId = hoist loadEs $ ListT.unfoldM go ()
  where
    loadEs :: forall a. ReadingStateT [EntityId] m a -> m a
    loadEs m = do
      s <- get
      let es :: [EntityId]
          es = HS.toList $ s ^. store . toSpaces . at sId . non mempty
      evalStateT (runReadingStateT m) es
    go _ = do
      es <- get'
      if null es
      then pure Nothing
      else do
        put' (tail es)
        pure (Just (head es, ()))
