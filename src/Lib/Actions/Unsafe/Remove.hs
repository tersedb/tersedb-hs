{-
TerseDB - Entity Management System
Copyright (C) 2024  Athan Clark

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

You can reach me at athan.clark@gmail.com.
-}

module Lib.Actions.Unsafe.Remove where

import Control.Lens (at, ix, non, (%~), (.~), (^.), _Left)
import Control.Monad.State (MonadState (get, put), modify, runState)
import Data.Foldable (foldlM, for_)
import qualified Data.List.NonEmpty as NE
import Lib.Actions.Unsafe.Update (
  unsafeRemoveReference,
  unsafeRemoveSubscription,
  unsafeUpdateFork,
 )
import Lib.Actions.Unsafe.Update.Group (unsafeUnlinkGroups)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Store (
  Shared,
  store,
  temp,
  toActors,
  toEntities,
  toForksFrom,
  toGroups,
  toReferencesFrom,
  toSpaces,
  toSubscriptionsFrom,
  toVersions,
 )
import Lib.Types.Store.Entity (versions)
import Lib.Types.Store.Groups (
  emptyGroup,
  members,
  next,
  nodes,
  outs,
  prev,
  roots,
 )
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Version (entity, references, subscriptions)

-- | Deletes a version from an entity iff. there's another replacement version in it
unsafeRemoveVersion
  :: (MonadState Shared m)
  => VersionId
  -> m (Either (Either VersionId EntityId) ())
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
  removeReferred
    :: Shared -> VersionId -> Either (Either VersionId EntityId) Shared
  removeReferred s referrerId =
    let (mE, s' :: Shared) = runState (unsafeRemoveReference referrerId vId) s
     in _Left %~ Left $ const s' <$> mE
  removeFork :: Shared -> EntityId -> Either (Either VersionId EntityId) Shared
  removeFork s eId =
    let (mE, s') = runState (unsafeUpdateFork eId Nothing) s
     in _Left %~ Right $ const s' <$> mE

unsafeRemoveEntity
  :: (MonadState Shared m)
  => EntityId
  -> m (Either (Either VersionId EntityId) ())
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

  rmSubscription
    :: Shared -> VersionId -> Either (Either VersionId EntityId) Shared
  rmSubscription s vId =
    let (eUnit, s') = runState (unsafeRemoveSubscription vId eId) s
     in _Left %~ Left $ const s' <$> eUnit

  rmFork :: Shared -> Either (Either VersionId EntityId) Shared
  rmFork s =
    let (mE, s') = runState (unsafeUpdateFork eId Nothing) s
     in _Left %~ Right $ const s' <$> mE

unsafeRemoveSpace
  :: (MonadState Shared m)
  => SpaceId
  -> m (Either (Either (Either VersionId EntityId) SpaceId) ())
unsafeRemoveSpace sId = do
  s <- get
  case s ^. store . toSpaces . at sId of
    Nothing -> pure . Left $ Right sId
    Just sp -> case foldlM rmEntity s (sp ^. entities) of
      Left e -> pure $ Left e
      Right s' -> do
        put s'
        modify $ store . toSpaces . at sId .~ Nothing
        pure (Right ())
 where
  rmEntity
    :: Shared -> EntityId -> Either (Either (Either VersionId EntityId) SpaceId) Shared
  rmEntity s eId =
    let (mE, s') = runState (unsafeRemoveEntity eId) s
     in _Left %~ Left $ const s' <$> mE

unsafeRemoveMember
  :: (MonadState Shared m)
  => GroupId
  -> ActorId
  -> m ()
unsafeRemoveMember gId aId = do
  modify $ store . toActors . at aId . non mempty . at gId .~ Nothing
  modify $
    store . toGroups . nodes . at gId . non emptyGroup . members . at aId .~ Nothing

unsafeRemoveActor
  :: (MonadState Shared m)
  => ActorId
  -> m ()
unsafeRemoveActor aId = do
  s <- get
  for_ (s ^. store . toActors . at aId . non mempty) $ \gId ->
    unsafeRemoveMember gId aId
  modify $ store . toActors . at aId .~ Nothing

unsafeRemoveGroup
  :: (MonadState Shared m)
  => GroupId
  -> m (Either GroupId ())
unsafeRemoveGroup gId = do
  s <- get
  case s ^. store . toGroups . nodes . at gId of
    Nothing -> pure (Left gId)
    Just g -> do
      for_ (g ^. members) $ \aId ->
        unsafeRemoveMember gId aId
      for_ (g ^. prev) $ \prevId ->
        unsafeUnlinkGroups prevId gId
      for_ (g ^. next) $ \nextId ->
        unsafeUnlinkGroups gId nextId
      modify $ store . toGroups . nodes . at gId .~ Nothing
      modify $ store . toGroups . roots . at gId .~ Nothing
      modify $ store . toGroups . outs . at gId .~ Nothing
      pure (Right ())
