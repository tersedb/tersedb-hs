module Lib.Async.Actions.Unsafe.Update where

import Control.Concurrent.STM (STM)
import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Reader (MonadReader (ask))
import Data.List (elemIndex)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import qualified Focus
import Lib.Async.Types.Monad (TerseM)
import Lib.Async.Types.Store (
  store,
  temp,
  toEntities,
  toEntityOf,
  toForks,
  toForksFrom,
  toReferences,
  toReferencesFrom,
  toSpaceEntities,
  toSpaceOf,
  toSubscriptions,
  toSubscriptionsFrom,
 )
import Lib.Types.Id (EntityId, SpaceId, VersionId)
import qualified StmContainers.Map as Map
import qualified StmContainers.Multimap as Multimap

unsafeAddReference :: VersionId -> VersionId -> TerseM STM ()
unsafeAddReference vId refId
  | vId == refId = pure ()
  | otherwise = do
      s <- ask
      liftBase $ do
        Multimap.insert refId vId (s ^. store . toReferences)
        Multimap.insert vId refId (s ^. temp . toReferencesFrom)

unsafeRemoveReference :: VersionId -> VersionId -> TerseM STM ()
unsafeRemoveReference vId refId = do
  s <- ask
  liftBase $ do
    Multimap.delete refId vId (s ^. store . toReferences)
    Multimap.delete vId refId (s ^. temp . toReferencesFrom)

unsafeAddSubscription :: VersionId -> EntityId -> TerseM STM ()
unsafeAddSubscription vId subId = do
  s <- ask
  liftBase $ do
    mEId <- Map.lookup vId (s ^. temp . toEntityOf)
    case mEId of
      Just eId | eId /= subId -> do
        Multimap.insert subId vId (s ^. store . toSubscriptions)
        Multimap.insert vId subId (s ^. temp . toSubscriptionsFrom)
      _ -> pure ()

unsafeRemoveSubscription :: VersionId -> EntityId -> TerseM STM ()
unsafeRemoveSubscription vId subId = do
  s <- ask
  liftBase $ do
    Multimap.delete subId vId (s ^. store . toSubscriptions)
    Multimap.delete vId subId (s ^. temp . toSubscriptionsFrom)

unsafeUpdateFork :: EntityId -> Maybe VersionId -> TerseM STM ()
unsafeUpdateFork eId mNewFork = do
  s <- ask
  liftBase $ do
    mOldFork <- Map.lookup eId (s ^. store . toForks)
    unless (mOldFork == mNewFork) $ do
      case mOldFork of
        Nothing -> do
          let newFork = fromJust mNewFork
          Multimap.insert eId newFork (s ^. temp . toForksFrom)
          Map.insert newFork eId (s ^. store . toForks)
        Just oldFork -> do
          Multimap.delete eId oldFork (s ^. temp . toForksFrom)
          case mNewFork of
            Nothing -> Map.delete eId (s ^. store . toForks)
            Just newFork -> do
              Multimap.insert eId newFork (s ^. temp . toForksFrom)
              Map.insert newFork eId (s ^. store . toForks)

unsafeMoveEntity :: EntityId -> SpaceId -> TerseM STM ()
unsafeMoveEntity eId newSId = do
  s <- ask
  liftBase $ do
    mOldSId <- Map.lookup eId (s ^. temp . toSpaceOf)
    case mOldSId of
      Nothing -> pure ()
      Just oldSId ->
        unless (oldSId == newSId) $ do
          Multimap.delete eId oldSId (s ^. store . toSpaceEntities)
          Multimap.insert eId newSId (s ^. store . toSpaceEntities)
          Map.insert newSId eId (s ^. temp . toSpaceOf)

unsafeOffsetVersionIndex :: VersionId -> Int -> TerseM STM ()
unsafeOffsetVersionIndex _ 0 = pure ()
unsafeOffsetVersionIndex vId offset = do
  s <- ask
  liftBase $ do
    mEId <- Map.lookup vId (s ^. temp . toEntityOf)
    case mEId of
      Nothing -> pure ()
      Just eId -> Map.focus (Focus.adjust go) eId (s ^. store . toEntities)
 where
  go vs =
    let vs' = NE.toList vs
        oldIdx = fromJust $ elemIndex vId vs'
        newIdx = oldIdx + offset
        vs'' = filter (/= vId) vs'
        vs''' = take newIdx vs'' <> (vId : drop newIdx vs'')
     in NE.fromList vs'''

unsafeSetVersionIndex :: VersionId -> Int -> TerseM STM ()
unsafeSetVersionIndex vId newIdx = do
  s <- ask
  liftBase $ do
    mEId <- Map.lookup vId (s ^. temp . toEntityOf)
    case mEId of
      Nothing -> pure ()
      Just eId -> Map.focus (Focus.adjust go) eId (s ^. store . toEntities)
 where
  go vs =
    let vs' = NE.toList vs
        vs'' = filter (/= vId) vs'
        vs''' = take newIdx vs'' <> (vId : drop newIdx vs'')
     in NE.fromList vs'''
