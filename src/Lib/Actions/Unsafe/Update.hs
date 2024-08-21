module Lib.Actions.Unsafe.Update where

import Control.Lens (at, ix, non, (%~), (&), (.~), (^.), (^?), _Left)
import Control.Monad.Extra (anyM, unless, when)
import Control.Monad.State (MonadState (get, put), execState, modify, runState)
import Data.Foldable (foldlM)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (elemIndex)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, isJust)
import Lib.Actions.Tabulation (updateTabulationStartingAt)
import Lib.Types.Id (EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Store (
    Shared,
    Temp,
    store,
    temp,
    toEntities,
    toForksFrom,
    toGroups,
    toReferencesFrom,
    toSpaces,
    toSubscriptionsFrom,
    toVersions,
 )
import Lib.Types.Store.Entity (fork, space, versions)
import Lib.Types.Store.Groups (next, nodes, prev)
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Version (entity, references, subscriptions)

unsafeUpdateVersionReferences ::
    (MonadState Shared m) =>
    VersionId ->
    HashSet VersionId ->
    m (Either VersionId ())
unsafeUpdateVersionReferences vId refIds = do
    s <- get
    case s ^. store . toVersions . at vId of
        Nothing -> pure $ Left vId
        Just v -> do
            let oldRefs = v ^. references
                refsToAdd = refIds `HS.difference` oldRefs
                refsToRemove = oldRefs `HS.difference` refIds

                addNewRefs :: VersionId -> Temp -> Temp
                addNewRefs refId t =
                    t
                        & toReferencesFrom . at refId . non mempty . at vId .~ Just ()

                removeRefs :: VersionId -> Temp -> Temp
                removeRefs refId t =
                    t
                        & toReferencesFrom . at refId
                            %~ maybe
                                Nothing
                                (\x -> let y = HS.delete vId x in if null y then Nothing else Just y)
            put $
                s
                    & temp .~ foldr removeRefs (foldr addNewRefs (s ^. temp) refsToAdd) refsToRemove
                    & store . toVersions . ix vId . references .~ refIds
            pure (Right ())

unsafeAddReference ::
    (MonadState Shared m) =>
    VersionId ->
    VersionId ->
    m (Either VersionId ())
unsafeAddReference vId refId = do
    s <- get
    case s ^? store . toVersions . ix vId . references of
        Nothing -> pure (Left vId)
        Just refs -> unsafeUpdateVersionReferences vId (HS.insert refId refs)

unsafeRemoveReference ::
    (MonadState Shared m) =>
    -- | Referrer
    VersionId ->
    -- | Referred
    VersionId ->
    m (Either VersionId ())
unsafeRemoveReference vId refId = do
    s <- get
    case s ^? store . toVersions . ix vId . references of
        Nothing -> pure (Left vId)
        Just refs -> unsafeUpdateVersionReferences vId (HS.delete refId refs)

unsafeUpdateVersionSubscriptions ::
    (MonadState Shared m) =>
    VersionId ->
    HashSet EntityId ->
    m (Either VersionId ())
unsafeUpdateVersionSubscriptions vId subIds = do
    s <- get
    case s ^. store . toVersions . at vId of
        Nothing -> pure $ Left vId
        Just v -> do
            let oldSubs = v ^. subscriptions
                subsToAdd = subIds `HS.difference` oldSubs
                subsToRemove = oldSubs `HS.difference` subIds

                addNewSubs :: EntityId -> Temp -> Temp
                addNewSubs subId t =
                    t
                        & toSubscriptionsFrom . at subId . non mempty . at vId .~ Just ()

                removeSubs :: EntityId -> Temp -> Temp
                removeSubs subId t =
                    t
                        & toSubscriptionsFrom . at subId
                            %~ maybe Nothing (\x -> let y = HS.delete vId x in if null y then Nothing else Just y)

            put $
                s
                    & temp .~ foldr removeSubs (foldr addNewSubs (s ^. temp) subsToAdd) subsToRemove
                    & store . toVersions . ix vId . subscriptions .~ subIds
            pure (Right ())

unsafeAddSubscription ::
    (MonadState Shared m) =>
    VersionId ->
    EntityId ->
    m (Either VersionId ())
unsafeAddSubscription vId subId = do
    s <- get
    case s ^? store . toVersions . ix vId . subscriptions of
        Nothing -> pure (Left vId)
        Just subs -> unsafeUpdateVersionSubscriptions vId (HS.insert subId subs)

unsafeRemoveSubscription ::
    (MonadState Shared m) =>
    VersionId ->
    EntityId ->
    m (Either VersionId ())
unsafeRemoveSubscription vId subId = do
    s <- get
    case s ^? store . toVersions . ix vId . subscriptions of
        Nothing -> pure (Left vId)
        Just subs -> unsafeUpdateVersionSubscriptions vId (HS.delete subId subs)

-- -- | Only works if this is the newest version in its owner entity
-- unsafeMoveVersionToNewFork
--   :: MonadState Shared m
--   => VersionId -- ^ Last version in a different entity
--   -> EntityId -- ^ New, empty parent entity
--   -> m Bool

unsafeUpdateFork ::
    (MonadState Shared m) =>
    EntityId ->
    Maybe VersionId ->
    m (Either EntityId ())
unsafeUpdateFork eId mFork = do
    s <- get
    case s ^. store . toEntities . at eId of
        Nothing -> pure $ Left eId
        Just e
            | e ^. fork == mFork -> pure (Right ())
            | otherwise -> do
                case e ^. fork of
                    Nothing -> pure ()
                    Just oldForkId ->
                        modify $
                            temp . toForksFrom . at oldForkId
                                %~ maybe Nothing (\x -> let y = HS.delete eId x in if null y then Nothing else Just y)
                case mFork of
                    Nothing -> pure ()
                    Just newForkId ->
                        modify $ temp . toForksFrom . at newForkId . non mempty . at eId .~ Just ()
                modify $ store . toEntities . ix eId . fork .~ mFork
                pure (Right ())

unsafeMoveEntity ::
    (MonadState Shared m) =>
    EntityId ->
    SpaceId ->
    m (Either EntityId ())
unsafeMoveEntity eId newSId = do
    s <- get
    case s ^. store . toEntities . at eId of
        Nothing -> pure $ Left eId
        Just e
            | e ^. space == newSId -> pure (Right ())
            | otherwise -> do
                let oldSpaceId = e ^. space
                modify $ store . toSpaces . ix oldSpaceId . entities . at eId .~ Nothing
                modify $ store . toSpaces . ix newSId . entities . at eId .~ Just ()
                modify $ store . toEntities . ix eId . space .~ newSId
                pure (Right ())

unsafeOffsetVersionIndex ::
    (MonadState Shared m) =>
    VersionId ->
    -- | 0 - don't move it, negative - move it earlier, positive - move it later
    Int ->
    m (Either VersionId ())
unsafeOffsetVersionIndex _ 0 = pure (Right ())
unsafeOffsetVersionIndex vId offset = do
    s <- get
    case s ^. store . toVersions . at vId of
        Nothing -> pure $ Left vId
        Just v -> do
            modify $ store . toEntities . ix (v ^. entity) . versions %~ go
            pure $ Right ()
  where
    go vs =
        -- FIXME it'd be nice to use mutable vectors here
        let vs' = NE.toList vs
            oldIdx = fromJust $ elemIndex vId vs'
            newIdx = oldIdx + offset
            vs'' = filter (/= vId) vs'
            vs''' = take newIdx vs'' <> (vId : drop newIdx vs'')
         in NE.fromList vs'''

unsafeSetVersionIndex ::
    (MonadState Shared m) =>
    VersionId ->
    Int ->
    m (Either VersionId ())
unsafeSetVersionIndex vId newIdx = do
    s <- get
    case s ^. store . toVersions . at vId of
        Nothing -> pure $ Left vId
        Just v -> do
            modify $ store . toEntities . ix (v ^. entity) . versions %~ go
            pure $ Right ()
  where
    go vs =
        let vs' = NE.toList vs
            vs'' = filter (/= vId) vs'
            vs''' = take newIdx vs'' <> (vId : drop newIdx vs'')
         in NE.fromList vs'''

-- -- TODO could delete versions
-- unsafeReSortVersions
--   :: MonadState Shared m
--   => EntityId
--   -> (Int -> [Int]) -- ^ Return a list of indicies to retain, and their new order (no duplicates)
--   -> m Bool

-- -- TODO could delete versions
-- unsafeUpdateVersions
--   :: MonadState Shared m
--   => EntityId
--   -> (NonEmpty VersionId -> NonEmpty VersionId)
--   -> m Bool
