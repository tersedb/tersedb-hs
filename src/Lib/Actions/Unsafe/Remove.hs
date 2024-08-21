module Lib.Actions.Unsafe.Remove where

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
import Lib.Actions.Unsafe.Update (unsafeRemoveReference, unsafeRemoveSubscription, unsafeUpdateFork)
import Lib.Types.Id (EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Store
  ( Shared,
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
import Lib.Types.Store.Entity (Entity, fork, space, versions)
import Lib.Types.Store.Groups (next, nodes, prev)
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Version (entity, references, subscriptions)

-- | Deletes a version from an entity iff. there's another replacement version in it
unsafeRemoveVersion ::
  (MonadState Shared m) =>
  VersionId ->
  m (Either (Either VersionId EntityId) ())
unsafeRemoveVersion vId = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure . Left $ Left vId
    Just v ->
      let eId :: EntityId = v ^. entity
       in case foldlM removeRef s (v ^. references)
            >>= flip
              (foldlM removeReferred)
              (s ^. temp . toReferencesFrom . at vId . non mempty)
            >>= flip (foldlM removeSub) (v ^. subscriptions)
            >>= flip
              (foldlM removeFork)
              (s ^. temp . toForksFrom . at vId . non mempty) of
            Left e -> pure (Left e)
            Right (s' :: Shared) -> do
              put s'
              modify $ store . toVersions . at vId .~ Nothing
              modify $
                store . toEntities . ix eId . versions
                  %~ NE.fromList . NE.filter (/= vId)
              pure $ Right ()
  where
    removeRef :: Shared -> VersionId -> Either (Either VersionId EntityId) Shared
    removeRef s refId =
      let (mE, s' :: Shared) = runState (unsafeRemoveReference vId refId) s
       in _Left %~ Left $ const s' <$> mE
    removeSub :: Shared -> EntityId -> Either (Either VersionId EntityId) Shared
    removeSub s subId =
      let (mE, s') = runState (unsafeRemoveSubscription vId subId) s
       in _Left %~ Left $ const s' <$> mE
    removeReferred :: Shared -> VersionId -> Either (Either VersionId EntityId) Shared
    removeReferred s referrerId =
      let (mE, s' :: Shared) = runState (unsafeRemoveReference referrerId vId) s
       in _Left %~ Left $ const s' <$> mE
    removeFork :: Shared -> EntityId -> Either (Either VersionId EntityId) Shared
    removeFork s eId =
      let (mE, s') = runState (unsafeUpdateFork eId Nothing) s
       in _Left %~ Right $ const s' <$> mE

unsafeRemoveEntity ::
  (MonadState Shared m) =>
  EntityId ->
  m (Either (Either VersionId EntityId) ())
unsafeRemoveEntity eId = do
  s <- get
  case s ^. store . toEntities . at eId of
    Nothing -> pure . Left $ Right eId
    Just e ->
      case foldlM rmVersion s (e ^. versions)
        >>= flip
          (foldlM rmSubscription)
          (s ^. temp . toSubscriptionsFrom . at eId . non mempty)
        >>= rmFork of
        Left e -> pure $ Left e
        Right s' -> do
          put s'
          modify $ store . toEntities . at eId .~ Nothing
          pure (Right ())
  where
    rmVersion :: Shared -> VersionId -> Either (Either VersionId EntityId) Shared
    rmVersion s vId =
      let (eUnit, s') = runState (unsafeRemoveVersion vId) s
       in const s' <$> eUnit

    rmSubscription :: Shared -> VersionId -> Either (Either VersionId EntityId) Shared
    rmSubscription s vId =
      let (eUnit, s') = runState (unsafeRemoveSubscription vId eId) s
       in _Left %~ Left $ const s' <$> eUnit

    rmFork :: Shared -> Either (Either VersionId EntityId) Shared
    rmFork s =
      let (mE, s') = runState (unsafeUpdateFork eId Nothing) s
       in _Left %~ Right $ const s' <$> mE

-- unsafeRemoveSpace ::
--   (MonadState Shared m) =>
--   SpaceId ->
--   m (Either SpaceId ())
-- unsafeRemoveSpace sId = do
--   s <- get
--   case s ^. store . toSpaces . at sId of
--     Nothing -> pure (Left sId)
--     Just sp -> do
--       foldlM goEnt s (sp ^. entities) $ \eId ->
--   where
--     goEnt :: Shared -> EntityId -> Either EntityId Shared
--     goEnt s eId =
--       case s ^. shared . toEntities . at eId of
--         Nothing -> Left eId
--         Just e ->
--           foldr  (e ^. versions)
--           pure $ s & store .
--           modify $ store . toEntities . at eId .~ Nothing
