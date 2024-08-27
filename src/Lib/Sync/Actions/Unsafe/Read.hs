module Lib.Sync.Actions.Unsafe.Read where

import Control.Lens (at, ix, non, (^.), (^?))
import Control.Monad.Base (MonadBase)
import Control.Monad.Morph (hoist)
import Control.Monad.State (MonadIO, MonadState (get, put), StateT, evalStateT)
import Data.Foldable (foldlM)
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)
import DeferredFolds.UnfoldlM (UnfoldlM (..))
import GHC.Generics (Generic)
import Lib.Sync.Types.Store (
  Shared,
  store,
  temp,
  toReferencesFrom,
  toSpaces,
  toSubscriptionsFrom,
  toVersions,
 )
import Lib.Sync.Types.Store.Version (references, subscriptions)
import Lib.Types.Id (EntityId, SpaceId, VersionId)
import ListT (ListT)
import qualified ListT

newtype ReadingStateT s m a = ReadingStateT
  { runReadingStateT :: StateT s m a
  }
  deriving (Generic, Functor, Applicative, Monad, MonadIO)

get' :: (Monad m) => ReadingStateT s m s
get' = ReadingStateT get

put' :: (Monad m) => s -> ReadingStateT s m ()
put' s = ReadingStateT (put s)

unsafeReadReferencesEager
  :: (MonadState Shared m) => VersionId -> UnfoldlM m VersionId
unsafeReadReferencesEager vId = UnfoldlM $ \f acc -> do
  s <- get
  let refs = fromMaybe mempty $ s ^? store . toVersions . ix vId . references
  foldlM f acc refs

unsafeReadReferencesLazy
  :: forall m. (MonadState Shared m) => VersionId -> m (ListT m VersionId)
unsafeReadReferencesLazy vId = do
  s <- get
  let refs = fromMaybe mempty $ s ^? store . toVersions . ix vId . references
  pure (ListT.fromFoldable refs)

unsafeReadReferencesFromEager
  :: (MonadState Shared m) => VersionId -> UnfoldlM m VersionId
unsafeReadReferencesFromEager vId = UnfoldlM $ \f acc -> do
  s <- get
  let refs = s ^. temp . toReferencesFrom . at vId . non mempty
  foldlM f acc refs

unsafeReadReferencesFromLazy
  :: forall m. (MonadState Shared m) => VersionId -> m (ListT m VersionId)
unsafeReadReferencesFromLazy vId = do
  s <- get
  let refs = s ^. temp . toReferencesFrom . at vId . non mempty
  pure (ListT.fromFoldable refs)

unsafeReadSubscriptionsEager
  :: (MonadState Shared m) => VersionId -> UnfoldlM m EntityId
unsafeReadSubscriptionsEager vId = UnfoldlM $ \f acc -> do
  s <- get
  let subs = fromMaybe mempty $ s ^? store . toVersions . ix vId . subscriptions
  foldlM f acc subs

unsafeReadSubscriptionsLazy
  :: forall m. (MonadState Shared m) => VersionId -> m (ListT m EntityId)
unsafeReadSubscriptionsLazy vId = do
  s <- get
  let subs = fromMaybe mempty $ s ^? store . toVersions . ix vId . subscriptions
  pure (ListT.fromFoldable subs)

unsafeReadSubscriptionsFromEager
  :: (MonadState Shared m) => EntityId -> UnfoldlM m VersionId
unsafeReadSubscriptionsFromEager eId = UnfoldlM $ \f acc -> do
  s <- get
  let subs = s ^. temp . toSubscriptionsFrom . at eId . non mempty
  foldlM f acc subs

unsafeReadSubscriptionsFromLazy
  :: forall m. (MonadState Shared m) => EntityId -> m (ListT m VersionId)
unsafeReadSubscriptionsFromLazy eId = do
  s <- get
  let subs = s ^. temp . toSubscriptionsFrom . at eId . non mempty
  pure (ListT.fromFoldable subs)

unsafeReadEntitiesEager
  :: (MonadState Shared m) => SpaceId -> UnfoldlM m EntityId
unsafeReadEntitiesEager sId = UnfoldlM $ \f acc -> do
  s <- get
  let es = s ^. store . toSpaces . at sId . non mempty
  foldlM f acc es

unsafeReadEntitiesLazy
  :: forall m. (MonadState Shared m) => SpaceId -> m (ListT m EntityId)
unsafeReadEntitiesLazy sId = do
  s <- get
  let es = s ^. store . toSpaces . at sId . non mempty
  pure (ListT.fromFoldable es)
