module Lib.Actions.Unsafe.Update where

import Lib.Types.Id (VersionId, EntityId)
import Lib.Types.Store
  ( Shared
  , store
  , temp
  , toVersions
  , toEntities
  , toReferencesFrom
  , toReferencesFromEntities
  , toReferencesFromSpaces
  , toSubscriptionsFrom
  , toSubscriptionsFromSpaces
  )
import Lib.Types.Store.Version (entity, references, subscriptions)
import Lib.Types.Store.Entity (space)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Foldable (foldlM)
import Control.Lens ((^.), (.~), (&), at, ix, non)
import Control.Monad.State (MonadState (get, put))


unsafeUpdateVersionReferences
  :: MonadState Shared m
  => VersionId
  -> HashSet VersionId
  -> m (Either (Either VersionId EntityId) ())
unsafeUpdateVersionReferences vId refIds = do
  s <- get
  eTemp' <- (\f -> foldlM f (Right (s ^. temp)) (HS.toList refIds)) $ \eTemp refId ->
    case eTemp of
      Left e -> pure (Left e)
      Right t -> case s ^. store . toVersions . at refId of
        Nothing -> pure . Left $ Left refId
        Just v -> case s ^. store . toEntities . at (v ^. entity) of
          Nothing -> pure . Left . Right $ v ^. entity
          Just e -> pure . Right
            $ t
            & toReferencesFrom . at refId . non mempty . at vId .~ Just ()
            & toReferencesFromEntities . at (v ^. entity) . non mempty . at vId .~ Just ()
            & toReferencesFromSpaces . at (e ^. space) . non mempty . at vId .~ Just ()
  case eTemp' of
    Left e -> pure (Left e)
    Right t -> do
      put $ s
          & temp .~ t
          & store . toVersions . ix vId . references .~ refIds
      pure (Right ())


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
