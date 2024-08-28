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
  toSubscriptionsFrom, toActors, toGroups,
  toMemberOf, toMembers, toEntities, toSpaces, toGroupsNext, toForksFrom
 )
import Lib.Types.Id (EntityId, SpaceId, VersionId, ActorId, GroupId)
import ListT (ListT)
import qualified ListT
import qualified StmContainers.Multimap as Multimap
import qualified StmContainers.Set as Set
import qualified StmContainers.Map as Map
import qualified Data.List.NonEmpty as NE


unsafeReadNextGroupsEager :: GroupId -> TerseM STM (UnfoldlM (TerseM STM) GroupId)
unsafeReadNextGroupsEager gId = do
  s <- ask
  let ns = Multimap.unfoldlMByKey gId (s ^. store . toGroupsNext)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  pure $ UnfoldlM.hoist lifter reducer ns

unsafeReadNextGroupsLazy
  :: GroupId -> TerseM STM (ListT (TerseM STM) GroupId)
unsafeReadNextGroupsLazy gId = do
  s <- ask
  let refs = Multimap.listTByKey gId (s ^. store . toGroupsNext)
  pure (hoist liftBase refs)

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

unsafeReadForkedByEager
  :: VersionId -> TerseM STM (UnfoldlM (TerseM STM) EntityId)
unsafeReadForkedByEager vId = do
  s <- ask
  let forks = Multimap.unfoldlMByKey vId (s ^. temp . toForksFrom)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  pure $ UnfoldlM.hoist lifter reducer forks

unsafeReadForkedByLazy
  :: VersionId -> TerseM STM (ListT (TerseM STM) EntityId)
unsafeReadForkedByLazy eId = do
  s <- ask
  let subs = Multimap.listTByKey eId (s ^. temp . toForksFrom)
  pure (hoist liftBase subs)


-- * Collections

unsafeReadActorsEager :: TerseM STM (UnfoldlM (TerseM STM) ActorId)
unsafeReadActorsEager = do
  s <- ask
  let as :: UnfoldlM STM ActorId
      as = Set.unfoldlM (s ^. store . toActors)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  pure . UnfoldlM.hoist lifter reducer $ as

unsafeReadActorsLazy :: TerseM STM (ListT (TerseM STM) ActorId)
unsafeReadActorsLazy = do
  s <- ask
  let as = Set.listT (s ^. store . toActors)
  pure (hoist liftBase as)

unsafeReadGroupsEager :: TerseM STM (UnfoldlM (TerseM STM) GroupId)
unsafeReadGroupsEager = do
  s <- ask
  let gs :: UnfoldlM STM GroupId
      gs = Set.unfoldlM (s ^. store . toGroups)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  pure . UnfoldlM.hoist lifter reducer $ gs

unsafeReadGroupsLazy :: TerseM STM (ListT (TerseM STM) GroupId)
unsafeReadGroupsLazy = do
  s <- ask
  let gs = Set.listT (s ^. store . toGroups)
  pure (hoist liftBase gs)

unsafeReadMembersEager :: GroupId -> TerseM STM (UnfoldlM (TerseM STM) ActorId)
unsafeReadMembersEager gId = do
  s <- ask
  let as :: UnfoldlM STM ActorId
      as = Multimap.unfoldlMByKey gId (s ^. store . toMembers)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  pure . UnfoldlM.hoist lifter reducer $ as

unsafeReadMembersLazy :: GroupId -> TerseM STM (ListT (TerseM STM) ActorId)
unsafeReadMembersLazy gId = do
  s <- ask
  let as = Multimap.listTByKey gId (s ^. store . toMembers)
  pure (hoist liftBase as)

unsafeReadMembersOfEager :: ActorId -> TerseM STM (UnfoldlM (TerseM STM) GroupId)
unsafeReadMembersOfEager aId = do
  s <- ask
  let gs :: UnfoldlM STM GroupId
      gs = Multimap.unfoldlMByKey aId (s ^. temp . toMemberOf)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  pure . UnfoldlM.hoist lifter reducer $ gs

unsafeReadMembersOfLazy :: ActorId -> TerseM STM (ListT (TerseM STM) GroupId)
unsafeReadMembersOfLazy aId = do
  s <- ask
  let gs = Multimap.listTByKey aId (s ^. temp . toMemberOf)
  pure (hoist liftBase gs)

unsafeReadSpacesEager :: TerseM STM (UnfoldlM (TerseM STM) SpaceId)
unsafeReadSpacesEager = do
  s <- ask
  let ss :: UnfoldlM STM SpaceId
      ss = Set.unfoldlM (s ^. store . toSpaces)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  pure . UnfoldlM.hoist lifter reducer $ ss

unsafeReadSpacesLazy :: TerseM STM (ListT (TerseM STM) SpaceId)
unsafeReadSpacesLazy = do
  s <- ask
  let ss = Set.listT (s ^. store . toSpaces)
  pure (hoist liftBase ss)

unsafeReadEntitiesEager :: SpaceId -> TerseM STM (UnfoldlM (TerseM STM) EntityId)
unsafeReadEntitiesEager sId = do
  s <- ask
  let es :: UnfoldlM STM EntityId
      es = Multimap.unfoldlMByKey sId (s ^. store . toSpaceEntities)
      lifter :: forall a. STM a -> TerseM STM a
      lifter = liftBase
      reducer :: forall a. TerseM STM a -> STM a
      reducer = flip runReaderT s
  pure . UnfoldlM.hoist lifter reducer $ es

unsafeReadEntitiesLazy :: SpaceId -> TerseM STM (ListT (TerseM STM) EntityId)
unsafeReadEntitiesLazy sId = do
  s <- ask
  let es = Multimap.listTByKey sId (s ^. store . toSpaceEntities)
  pure (hoist liftBase es)

unsafeReadVersionsEager :: EntityId -> TerseM STM (UnfoldlM (TerseM STM) VersionId)
unsafeReadVersionsEager eId = do
  s <- ask
  vs <- liftBase $ maybe [] NE.toList <$> Map.lookup eId (s ^. store . toEntities)
  pure $ UnfoldlM.foldable vs

unsafeReadVersionsLazy :: EntityId -> TerseM STM (ListT (TerseM STM) VersionId)
unsafeReadVersionsLazy eId = do
  s <- ask
  vs <- liftBase $ maybe [] NE.toList <$> Map.lookup eId (s ^. store . toEntities)
  pure $ ListT.fromFoldable vs
