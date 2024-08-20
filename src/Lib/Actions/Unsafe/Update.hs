module Lib.Actions.Unsafe.Update where

import Lib.Actions.Tabulation (updateTabulationStartingAt)
import Lib.Types.Id (VersionId, EntityId, GroupId, SpaceId)
import Lib.Types.Store
  ( Shared
  , Temp
  , store
  , temp
  , toVersions
  , toEntities
  , toGroups
  , toSpaces
  , toReferencesFrom
  , toSubscriptionsFrom
  , toForksFrom
  )
import Lib.Types.Store.Groups (prev, nodes, next)
import Lib.Types.Store.Version (entity, references, subscriptions)
import Lib.Types.Store.Entity (space, versions, fork)
import Lib.Types.Store.Space (entities)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Foldable (foldlM)
import Data.List (elemIndex)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, isJust)
import Control.Lens ((^.), (.~), (&), (^?), (%~), at, ix, non, _Left)
import Control.Monad.State (MonadState (get, put), modify, runState, execState)
import Control.Monad.Extra (unless, when, anyM)


unsafeUpdateVersionReferences
  :: MonadState Shared m
  => VersionId
  -> HashSet VersionId
  -> m (Either VersionId ())
unsafeUpdateVersionReferences vId refIds = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure $ Left vId
    Just v -> do
      let oldRefs = v ^. references
          refsToAdd = refIds `HS.difference` oldRefs
          refsToRemove = oldRefs `HS.difference` refIds

          addNewRefs :: VersionId -> Temp -> Temp
          addNewRefs refId t = t
            & toReferencesFrom . at refId . non mempty . at vId .~ Just ()

          removeRefs :: VersionId -> Temp -> Temp
          removeRefs refId t = t
            & toReferencesFrom . at refId
            %~ maybe Nothing
                (\x -> let y = HS.delete vId x in if null y then Nothing else Just y)
      put $ s
          & temp .~ foldr removeRefs (foldr addNewRefs (s ^. temp) refsToAdd) refsToRemove
          & store . toVersions . ix vId . references .~ refIds
      pure (Right ())


unsafeAddReference
  :: MonadState Shared m
  => VersionId
  -> VersionId
  -> m (Either VersionId ())
unsafeAddReference vId refId = do
  s <- get
  case s ^? store . toVersions . ix vId . references of
    Nothing -> pure (Left vId)
    Just refs -> unsafeUpdateVersionReferences vId (HS.insert refId refs)

unsafeRemoveReference
  :: MonadState Shared m
  => VersionId -- ^ Referrer
  -> VersionId -- ^ Referred
  -> m (Either VersionId ())
unsafeRemoveReference vId refId = do
  s <- get
  case s ^? store . toVersions . ix vId . references of
    Nothing -> pure (Left vId)
    Just refs -> unsafeUpdateVersionReferences vId (HS.delete refId refs)


unsafeUpdateVersionSubscriptions
  :: MonadState Shared m
  => VersionId
  -> HashSet EntityId
  -> m (Either VersionId ())
unsafeUpdateVersionSubscriptions vId subIds = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure $ Left vId
    Just v -> do
      let oldSubs = v ^. subscriptions
          subsToAdd = subIds `HS.difference` oldSubs
          subsToRemove = oldSubs `HS.difference` subIds

          addNewSubs :: EntityId -> Temp -> Temp
          addNewSubs subId t = t
            & toSubscriptionsFrom . at subId . non mempty . at vId .~ Just ()

          removeSubs :: EntityId -> Temp -> Temp
          removeSubs subId t = t
            & toSubscriptionsFrom . at subId
            %~ maybe Nothing (\x -> let y = HS.delete vId x in if null y then Nothing else Just y)

      put $ s
          & temp .~ foldr removeSubs (foldr addNewSubs (s ^. temp) subsToAdd) subsToRemove
          & store . toVersions . ix vId . subscriptions .~ subIds
      pure (Right ())


unsafeAddSubscription
  :: MonadState Shared m
  => VersionId
  -> EntityId
  -> m (Either VersionId ())
unsafeAddSubscription vId subId = do
  s <- get
  case s ^? store . toVersions . ix vId . subscriptions of
    Nothing -> pure (Left vId)
    Just subs -> unsafeUpdateVersionSubscriptions vId (HS.insert subId subs)

unsafeRemoveSubscription
  :: MonadState Shared m
  => VersionId
  -> EntityId
  -> m (Either VersionId ())
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


-- | Deletes a version from an entity iff. there's another replacement version in it
unsafeRemoveVersion
  :: MonadState Shared m
  => VersionId
  -> m (Either VersionId ())
unsafeRemoveVersion vId = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure $ Left vId
    Just v ->
      let eId :: EntityId = v ^. entity
      in  case s ^? store . toEntities . ix eId of
            Just e
              | length (e ^. versions) > 1 -> do
                let removeRef :: Shared -> VersionId -> Either VersionId Shared
                    removeRef s refId =
                      let (mE, s' :: Shared) = runState (unsafeRemoveReference vId refId) s
                      in  case mE of
                        Left e -> Left e
                        Right () -> Right s'
                    removeSub :: Shared -> EntityId -> Either VersionId Shared
                    removeSub s subId =
                      let (mE, s') = runState (unsafeRemoveSubscription vId subId) s
                      in  case mE of
                        Left e -> Left e
                        Right () -> Right s'
                    removeReferred :: Shared -> VersionId -> Either VersionId Shared
                    removeReferred s referrerId =
                      let (mE, s' :: Shared) = runState (unsafeRemoveReference referrerId vId) s
                      in  case mE of
                        Left e -> Left e
                        Right () -> Right s'
                case foldlM removeRef s (v ^. references) >>=
                       flip (foldlM removeReferred)
                         (s ^. temp . toReferencesFrom . at vId . non mempty) >>=
                       flip (foldlM removeSub) (v ^. subscriptions) of
                  Left e -> pure (Left e)
                  Right s' -> do
                    put s'
                    modify $ store . toVersions . at vId .~ Nothing
                    modify $ store . toEntities . ix eId . versions
                      %~ NE.fromList . NE.filter (/= vId)
                    pure $ Right ()
                    -- TODO filter all references so nothing can point to vId
            _ -> pure $ Left vId -- TODO better error?


unsafeUpdateFork
  :: MonadState Shared m
  => EntityId
  -> Maybe VersionId
  -> m (Either EntityId ())
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
            modify $ temp . toForksFrom . at oldForkId
              %~ maybe Nothing (\x -> let y = HS.delete eId x in if null y then Nothing else Just y)
        case mFork of
          Nothing -> pure ()
          Just newForkId ->
            modify $ temp . toForksFrom . at newForkId . non mempty . at eId .~ Just ()
        modify $ store . toEntities . ix eId . fork .~ mFork
        pure (Right ())

unsafeMoveEntity
  :: MonadState Shared m
  => EntityId
  -> SpaceId
  -> m (Either EntityId ())
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


unsafeOffsetVersionIndex
  :: MonadState Shared m
  => VersionId
  -> Int -- ^ 0 - don't move it, negative - move it earlier, positive - move it later
  -> m (Either VersionId ())
unsafeOffsetVersionIndex _ 0 = pure (Right ())
unsafeOffsetVersionIndex vId offset = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure $ Left vId
    Just v -> do
      modify $ store . toEntities . ix (v ^. entity) . versions %~ go
      pure $ Right ()
  where
    go vs = -- FIXME it'd be nice to use mutable vectors here
      let vs' = NE.toList vs
          oldIdx = fromJust $ elemIndex vId vs'
          newIdx = oldIdx + offset
          vs'' = filter (/= vId) vs'
          vs''' = take newIdx vs'' <> (vId : drop newIdx vs'')
      in  NE.fromList vs'''

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
