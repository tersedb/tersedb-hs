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
import Lib.Types.Store.Version (entity, references, subscriptions, prevVersion)
import Lib.Types.Store.Entity (space)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Foldable (foldlM)
import Control.Lens ((^.), (.~), (&), (^?), (%~), at, ix, non, _Left)
import Control.Monad.State (MonadState (get, put), modify)


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
          refsToRemove = (maybe id HS.delete (v ^. prevVersion)) (oldRefs `HS.difference` refIds)

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
  => VersionId
  -> VersionId
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
  -> m (Either EntityId ())
unsafeUpdateVersionSubscriptions vId subIds = do
  s <- get
  eTemp' <- (\f -> foldlM f (Right (s ^. temp)) (HS.toList subIds)) $ \eTemp subId ->
    case eTemp of
      Left e -> pure (Left e)
      Right t -> case s ^. store . toEntities . at subId of
        Nothing -> pure $ Left subId
        Just e -> pure . Right
          $ t
          & toSubscriptionsFrom . at subId . non mempty . at vId .~ Just ()
          & toSubscriptionsFromSpaces . at (e ^. space) . non mempty . at vId .~ Just ()
  case eTemp' of
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
    Just subs -> (_Left %~ Right) <$> unsafeUpdateVersionSubscriptions vId (HS.insert subId subs)

unsafeRemoveSubscription
  :: MonadState Shared m
  => VersionId
  -> EntityId
  -> m (Either (Either VersionId EntityId) ())
unsafeRemoveSubscription vId subId = do
  s <- get
  case s ^? store . toVersions . ix vId . subscriptions of
    Nothing -> pure (Left (Left vId))
    Just subs -> (_Left %~ Right) <$> unsafeUpdateVersionSubscriptions vId (HS.delete subId subs)
