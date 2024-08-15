module Lib.Actions.Unsafe.Update where

import Lib.Actions.Tabulation (updateTabulationStartingAt)
import Lib.Types.Id (VersionId, EntityId, GroupId)
import Lib.Types.Store
  ( Shared
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
import Lib.Types.Store.Entity (space)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Foldable (foldlM)
import Control.Lens ((^.), (.~), (&), at, ix, non)
import Control.Monad.State (MonadState (get, put), modify)


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


unsafeUpdateGroupChildren
  :: MonadState Shared m
  => GroupId
  -> HashSet GroupId
  -> m ()
unsafeUpdateGroupChildren gId children = do
  let children' = HS.delete gId children
  modify $ store . toGroups . nodes . ix gId . next .~ children'
  updateTabulationStartingAt gId

unsafeAddGroupChild
  :: MonadState Shared m
  => GroupId
  -> GroupId
  -> m ()
unsafeAddGroupChild gId childId
  | gId == childId = pure ()
  | otherwise = do
  unsafeUpdateGroupParent childId Nothing
  modify $ store . toGroups . nodes . ix gId . next . at childId .~ Just ()
  modify $ store . toGroups . nodes . ix childId . prev .~ Just gId
  updateTabulationStartingAt gId

unsafeRemoveGroupChild
  :: MonadState Shared m
  => GroupId
  -> GroupId
  -> m ()
unsafeRemoveGroupChild gId childId = do
  modify $ store . toGroups . nodes . ix gId . next . at childId .~ Nothing
  modify $ store . toGroups . nodes . ix childId . prev .~ Nothing
  updateTabulationStartingAt childId

unsafeUpdateGroupParent
  :: MonadState Shared m
  => GroupId
  -> Maybe GroupId
  -> m ()
unsafeUpdateGroupParent gId mParent = do
  s <- get
  case s ^. store . toGroups . nodes . at gId of
    Nothing -> pure ()
    Just g
      | g ^. prev == mParent -> pure ()
      | otherwise -> do
          case g ^. prev of
            Nothing -> pure ()
            Just oldParentGId ->
              unsafeRemoveGroupChild oldParentGId gId
          case mParent of
            Nothing -> pure ()
            Just parentGId -> do
              unsafeAddGroupChild parentGId gId
