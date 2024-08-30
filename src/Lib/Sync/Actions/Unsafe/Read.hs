module Lib.Sync.Actions.Unsafe.Read where

import Control.Lens (at, ix, non, (^.), (^?))
import Control.Monad.Base (MonadBase)
import Control.Monad.Morph (hoist)
import Control.Monad.State (MonadIO, MonadState (get, put), StateT, evalStateT)
import Data.Foldable (foldlM)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)
import DeferredFolds.UnfoldlM (UnfoldlM (..))
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import GHC.Generics (Generic)
import Lib.Sync.Types.Store (
  Shared,
  store,
  temp,
  toActors,
  toEntities,
  toForksFrom,
  toGroups,
  toMemberOf,
  toReferencesFrom,
  toSpaces,
  toSubscriptionsFrom,
  toVersions,
 )
import Lib.Sync.Types.Store.Entity (versions)
import Lib.Sync.Types.Store.Groups (members, nodes)
import Lib.Sync.Types.Store.Version (references, subscriptions)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import ListT (ListT)
import qualified ListT

unsafeReadReferencesEager
  :: (MonadState Shared m) => VersionId -> m (UnfoldlM m VersionId)
unsafeReadReferencesEager vId = do
  s <- get
  let refs = fromMaybe mempty $ s ^? store . toVersions . ix vId . references
  pure (UnfoldlM.foldable refs)

unsafeReadReferencesLazy
  :: forall m. (MonadState Shared m) => VersionId -> m (ListT m VersionId)
unsafeReadReferencesLazy vId = do
  s <- get
  let refs = fromMaybe mempty $ s ^? store . toVersions . ix vId . references
  pure (ListT.fromFoldable refs)

unsafeReadReferencesFromEager
  :: (MonadState Shared m) => VersionId -> m (UnfoldlM m VersionId)
unsafeReadReferencesFromEager vId = do
  s <- get
  let refs = s ^. temp . toReferencesFrom . at vId . non mempty
  pure (UnfoldlM.foldable refs)

unsafeReadReferencesFromLazy
  :: forall m. (MonadState Shared m) => VersionId -> m (ListT m VersionId)
unsafeReadReferencesFromLazy vId = do
  s <- get
  let refs = s ^. temp . toReferencesFrom . at vId . non mempty
  pure (ListT.fromFoldable refs)

unsafeReadSubscriptionsEager
  :: (MonadState Shared m) => VersionId -> m (UnfoldlM m EntityId)
unsafeReadSubscriptionsEager vId = do
  s <- get
  let subs = fromMaybe mempty $ s ^? store . toVersions . ix vId . subscriptions
  pure (UnfoldlM.foldable subs)

unsafeReadSubscriptionsLazy
  :: forall m. (MonadState Shared m) => VersionId -> m (ListT m EntityId)
unsafeReadSubscriptionsLazy vId = do
  s <- get
  let subs = fromMaybe mempty $ s ^? store . toVersions . ix vId . subscriptions
  pure (ListT.fromFoldable subs)

unsafeReadSubscriptionsFromEager
  :: (MonadState Shared m) => EntityId -> m (UnfoldlM m VersionId)
unsafeReadSubscriptionsFromEager eId = do
  s <- get
  let subs = s ^. temp . toSubscriptionsFrom . at eId . non mempty
  pure (UnfoldlM.foldable subs)

unsafeReadSubscriptionsFromLazy
  :: forall m. (MonadState Shared m) => EntityId -> m (ListT m VersionId)
unsafeReadSubscriptionsFromLazy eId = do
  s <- get
  let subs = s ^. temp . toSubscriptionsFrom . at eId . non mempty
  pure (ListT.fromFoldable subs)

unsafeReadForkedByEager
  :: forall m. (MonadState Shared m) => VersionId -> m (UnfoldlM m EntityId)
unsafeReadForkedByEager eId = do
  s <- get
  let forks = s ^. temp . toForksFrom . at eId . non mempty
  pure (UnfoldlM.foldable forks)

unsafeReadForkedByLazy
  :: forall m. (MonadState Shared m) => VersionId -> m (ListT m EntityId)
unsafeReadForkedByLazy eId = do
  s <- get
  let forks = s ^. temp . toForksFrom . at eId . non mempty
  pure (ListT.fromFoldable forks)

-- * Collections

unsafeReadActorsEager
  :: (MonadState Shared m) => m (UnfoldlM m ActorId)
unsafeReadActorsEager = do
  s <- get
  let as = s ^. store . toActors
  pure (UnfoldlM.foldable as)

unsafeReadActorsLazy
  :: forall m. (MonadState Shared m) => m (ListT m ActorId)
unsafeReadActorsLazy = do
  s <- get
  let as = s ^. store . toActors
  pure (ListT.fromFoldable as)

unsafeReadGroupsEager
  :: (MonadState Shared m) => m (UnfoldlM m GroupId)
unsafeReadGroupsEager = do
  s <- get
  let gs = HM.keysSet $ s ^. store . toGroups . nodes
  pure (UnfoldlM.foldable gs)

unsafeReadGroupsLazy
  :: forall m. (MonadState Shared m) => m (ListT m GroupId)
unsafeReadGroupsLazy = do
  s <- get
  let gs = HM.keysSet $ s ^. store . toGroups . nodes
  pure (ListT.fromFoldable gs)

unsafeReadMembersEager
  :: (MonadState Shared m) => GroupId -> m (UnfoldlM m ActorId)
unsafeReadMembersEager gId = do
  s <- get
  let as = fromMaybe mempty $ s ^? store . toGroups . nodes . ix gId . members
  pure (UnfoldlM.foldable as)

unsafeReadMembersLazy
  :: forall m. (MonadState Shared m) => GroupId -> m (ListT m ActorId)
unsafeReadMembersLazy gId = do
  s <- get
  let as = fromMaybe mempty $ s ^? store . toGroups . nodes . ix gId . members
  pure (ListT.fromFoldable as)

unsafeReadMembersOfEager
  :: (MonadState Shared m) => ActorId -> m (UnfoldlM m GroupId)
unsafeReadMembersOfEager aId = do
  s <- get
  let gs = fromMaybe mempty $ s ^? temp . toMemberOf . ix aId
  pure (UnfoldlM.foldable gs)

unsafeReadMembersOfLazy
  :: forall m. (MonadState Shared m) => ActorId -> m (ListT m GroupId)
unsafeReadMembersOfLazy aId = do
  s <- get
  let gs = fromMaybe mempty $ s ^? temp . toMemberOf . ix aId
  pure (ListT.fromFoldable gs)

unsafeReadSpacesEager
  :: (MonadState Shared m) => m (UnfoldlM m SpaceId)
unsafeReadSpacesEager = do
  s <- get
  let es = HM.keysSet $ s ^. store . toSpaces
  pure (UnfoldlM.foldable es)

unsafeReadSpacesLazy
  :: forall m. (MonadState Shared m) => m (ListT m SpaceId)
unsafeReadSpacesLazy = do
  s <- get
  let es = HM.keysSet $ s ^. store . toSpaces
  pure (ListT.fromFoldable es)

unsafeReadEntitiesEager
  :: (MonadState Shared m) => SpaceId -> m (UnfoldlM m EntityId)
unsafeReadEntitiesEager sId = do
  s <- get
  let es = s ^. store . toSpaces . at sId . non mempty
  pure (UnfoldlM.foldable es)

unsafeReadEntitiesLazy
  :: forall m. (MonadState Shared m) => SpaceId -> m (ListT m EntityId)
unsafeReadEntitiesLazy sId = do
  s <- get
  let es = s ^. store . toSpaces . at sId . non mempty
  pure (ListT.fromFoldable es)

unsafeReadVersionsEager
  :: (MonadState Shared m) => EntityId -> m (UnfoldlM m VersionId)
unsafeReadVersionsEager eId = do
  s <- get
  let vs = maybe [] NE.toList $ s ^? store . toEntities . ix eId . versions
  pure (UnfoldlM.foldable vs)

unsafeReadVersionsLazy
  :: forall m. (MonadState Shared m) => EntityId -> m (ListT m VersionId)
unsafeReadVersionsLazy eId = do
  s <- get
  let vs = maybe [] NE.toList $ s ^? store . toEntities . ix eId . versions
  pure (ListT.fromFoldable vs)
