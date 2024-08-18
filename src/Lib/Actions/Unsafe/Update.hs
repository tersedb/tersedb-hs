module Lib.Actions.Unsafe.Update where

import Lib.Actions.Tabulation (updateTabulationStartingAt)
import Lib.Types.Id (VersionId, EntityId, GroupId)
import Lib.Types.Store
  ( Shared
  , Temp
  , store
  , temp
  , toVersions
  , toEntities
  , toGroups
  , toReferencesFrom
  , toReferencesFromEntities
  , toReferencesFromSpaces
  , toSubscriptionsFrom
  , toSubscriptionsFromSpaces
  )
import Lib.Types.Store.Groups (prev, nodes, next)
import Lib.Types.Store.Version (entity, references, subscriptions)
import Lib.Types.Store.Entity (space, versions)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Foldable (foldlM)
import Data.List (findIndex)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Control.Lens ((^.), (.~), (&), (^?), (%~), at, ix, non, _Left)
import Control.Monad.State (MonadState (get, put), modify, runState, execState)


unsafeUpdateVersionReferences
  :: MonadState Shared m
  => VersionId
  -> HashSet VersionId
  -> m (Either (Either VersionId EntityId) ())
unsafeUpdateVersionReferences vId refIds = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure . Left $ Left vId
    Just v -> do
      let oldRefs = v ^. references
          refsToAdd = refIds `HS.difference` oldRefs
          refsToRemove = oldRefs `HS.difference` refIds

          addNewRefs :: Temp -> VersionId -> Either (Either VersionId EntityId) Temp
          addNewRefs t refId = case s ^. store . toVersions . at refId of
            Nothing -> Left $ Left refId
            Just v -> case s ^. store . toEntities . at (v ^. entity) of
              Nothing -> Left . Right $ v ^. entity
              Just e -> pure
                $ t
                & toReferencesFrom . at refId . non mempty . at vId .~ Just ()
                & toReferencesFromEntities . at (v ^. entity) . non mempty . at vId .~ Just ()
                & toReferencesFromSpaces . at (e ^. space) . non mempty . at vId .~ Just ()

          removeRefs :: Temp -> VersionId -> Either (Either VersionId EntityId) Temp
          removeRefs t refId = case s ^. store . toVersions . at refId of
            Nothing -> Left $ Left refId
            Just v -> case s ^. store . toEntities . at (v ^. entity) of
              Nothing -> Left . Right $ v ^. entity
              Just e ->
                let t' = t
                      & toReferencesFrom . ix refId . at vId .~ Nothing
                      & toReferencesFromEntities . ix (v ^. entity) . at vId .~ Nothing
                      & toReferencesFromSpaces . ix (e ^. space) . at vId .~ Nothing
                    t'' = t'
                      & toReferencesFrom . at refId
                      %~ maybe Nothing (\x -> if null x then Nothing else Just x)
                      & toReferencesFromEntities . at (v ^. entity)
                      %~ maybe Nothing (\x -> if null x then Nothing else Just x)
                      & toReferencesFromSpaces . at (e ^. space)
                      %~ maybe Nothing (\x -> if null x then Nothing else Just x)
                in  pure t''
      case do t <- foldlM addNewRefs (s ^. temp) (HS.toList refsToAdd)
              foldlM removeRefs t (HS.toList refsToRemove) of
        Left e -> pure (Left e)
        Right t -> do
          put $ s
              & temp .~ t
              & store . toVersions . ix vId . references .~ refIds
          pure (Right ())


unsafeAddReference
  :: MonadState Shared m
  => VersionId
  -> VersionId
  -> m (Either (Either VersionId EntityId) ())
unsafeAddReference vId refId = do
  s <- get
  case s ^? store . toVersions . ix vId . references of
    Nothing -> pure (Left (Left vId))
    Just refs -> unsafeUpdateVersionReferences vId (HS.insert refId refs)

unsafeRemoveReference
  :: MonadState Shared m
  => VersionId -- ^ Referrer
  -> VersionId -- ^ Referred
  -> m (Either (Either VersionId EntityId) ())
unsafeRemoveReference vId refId = do
  s <- get
  case s ^? store . toVersions . ix vId . references of
    Nothing -> pure (Left (Left vId))
    Just refs -> unsafeUpdateVersionReferences vId (HS.delete refId refs)


unsafeUpdateVersionSubscriptions
  :: MonadState Shared m
  => VersionId
  -> HashSet EntityId
  -> m (Either (Either VersionId EntityId) ())
unsafeUpdateVersionSubscriptions vId subIds = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure . Left $ Left vId
    Just v -> do
      let oldSubs = v ^. subscriptions
          subsToAdd = subIds `HS.difference` oldSubs
          subsToRemove = oldSubs `HS.difference` subIds

          addNewSubs :: Temp -> EntityId -> Either (Either VersionId EntityId) Temp
          addNewSubs t subId = case s ^. store . toEntities . at subId of
            Nothing -> Left $ Right subId
            Just e -> pure
              $ t
              & toSubscriptionsFrom . at subId . non mempty . at vId .~ Just ()
              & toSubscriptionsFromSpaces . at (e ^. space) . non mempty . at vId .~ Just ()

          removeSubs :: Temp -> EntityId -> Either (Either VersionId EntityId) Temp
          removeSubs t subId = case s ^. store . toEntities . at subId of
            Nothing -> Left . Right $ subId
            Just e ->
              let t' = t
                    & toSubscriptionsFrom . ix subId . at vId .~ Nothing
                    & toSubscriptionsFromSpaces . ix (e ^. space) . at vId .~ Nothing
                  t'' = t'
                    & toSubscriptionsFrom . at subId
                    %~ maybe Nothing (\x -> if null x then Nothing else Just x)
                    & toSubscriptionsFromSpaces . at (e ^. space)
                    %~ maybe Nothing (\x -> if null x then Nothing else Just x)
              in  pure t''

      case do t <- foldlM addNewSubs (s ^. temp) (HS.toList subsToAdd)
              foldlM removeSubs t (HS.toList subsToRemove) of
        Left e -> pure (Left e)
        Right t -> do
          put $ s
              & temp .~ t
              & store . toVersions . ix vId . subscriptions .~ subIds
          pure (Right ())


unsafeAddSubscription
  :: MonadState Shared m
  => VersionId
  -> EntityId
  -> m (Either (Either VersionId EntityId) ())
unsafeAddSubscription vId subId = do
  s <- get
  case s ^? store . toVersions . ix vId . subscriptions of
    Nothing -> pure (Left (Left vId))
    Just subs -> unsafeUpdateVersionSubscriptions vId (HS.insert subId subs)

unsafeRemoveSubscription
  :: MonadState Shared m
  => VersionId
  -> EntityId
  -> m (Either (Either VersionId EntityId) ())
unsafeRemoveSubscription vId subId = do
  s <- get
  case s ^? store . toVersions . ix vId . subscriptions of
    Nothing -> pure (Left (Left vId))
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
  -> m (Either (Either VersionId EntityId) ())
unsafeRemoveVersion vId = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure . Left $ Left vId
    Just v ->
      let eId :: EntityId = v ^. entity
      in  case s ^? store . toEntities . ix eId of
            Just e
              | length (e ^. versions) > 1 -> do
                let removeRef :: Shared -> VersionId -> Either (Either VersionId EntityId) Shared
                    removeRef s refId =
                      let (mE, s' :: Shared) = runState (unsafeRemoveReference vId refId) s
                      in  case mE of
                        Left e -> Left e
                        Right () -> Right s'
                    removeSub :: Shared -> EntityId -> Either (Either VersionId EntityId) Shared
                    removeSub s subId =
                      let (mE, s') = runState (unsafeRemoveSubscription vId subId) s
                      in  case mE of
                        Left e -> Left e
                        Right () -> Right s'
                    removeReferred :: Shared -> VersionId -> Either (Either VersionId EntityId) Shared
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
            _ -> pure . Left $ Left vId -- TODO better error?

-- unsafeOffsetVersion
--   :: MonadState Shared m
--   => VersionId
--   -> Int -- ^ 0 - don't move it, negative - move it earlier, positive - move it later
--   -> m ()

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
