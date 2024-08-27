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

module Lib.Sync.Actions.Unsafe.Remove where

import Control.Lens (at, ix, non, (%~), (.~), (^.), (^?!), _Left)
import Control.Monad.State (MonadState (get, put), execState, modify, runState)
import Data.Foldable (foldlM, for_)
import qualified Data.List.NonEmpty as NE
import Lib.Sync.Actions.Unsafe.Update (
  unsafeRemoveReference,
  unsafeRemoveSubscription,
  unsafeUpdateFork,
 )
import Lib.Sync.Actions.Unsafe.Update.Group (unsafeUnlinkGroups)
import Lib.Sync.Types.Store (
  Shared,
  store,
  temp,
  toActors,
  toEntities,
  toEntityOf,
  toForksFrom,
  toGroups,
  toMemberOf,
  toReferencesFrom,
  toSpaces,
  toSubscriptionsFrom,
  toVersions,
 )
import Lib.Sync.Types.Store.Entity (versions)
import Lib.Sync.Types.Store.Groups (
  emptyGroup,
  members,
  next,
  nodes,
  outs,
  prev,
  roots,
 )
import Lib.Sync.Types.Store.Version (references, subscriptions)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)

-- | Deletes a version from an entity iff. there's another replacement version in it
unsafeRemoveVersion
  :: (MonadState Shared m)
  => VersionId
  -> m ()
unsafeRemoveVersion vId = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure ()
    Just v -> do
      let eId :: EntityId = s ^?! temp . toEntityOf . ix vId
          s' =
            foldr
              removeFork
              ( foldr
                  removeSub
                  ( foldr
                      removeReferred
                      (foldr removeRef s (v ^. references))
                      (s ^. temp . toReferencesFrom . at vId . non mempty)
                  )
                  (v ^. subscriptions)
              )
              (s ^. temp . toForksFrom . at vId . non mempty)
      put s'
      modify $ store . toVersions . at vId .~ Nothing
      modify $
        store . toEntities . ix eId . versions
          %~ NE.fromList . NE.filter (/= vId)
 where
  removeRef :: VersionId -> Shared -> Shared
  removeRef refId = execState (unsafeRemoveReference vId refId)
  removeSub :: EntityId -> Shared -> Shared
  removeSub subId = execState (unsafeRemoveSubscription vId subId)
  removeReferred :: VersionId -> Shared -> Shared
  removeReferred referrerId = execState (unsafeRemoveReference referrerId vId)
  removeFork :: EntityId -> Shared -> Shared
  removeFork eId = execState (unsafeUpdateFork eId Nothing)

unsafeRemoveEntity
  :: (MonadState Shared m)
  => EntityId
  -> m ()
unsafeRemoveEntity eId = do
  s <- get
  case s ^. store . toEntities . at eId of
    Nothing -> pure ()
    Just e -> do
      let s' =
            rmFork
              ( foldr
                  rmSubscription
                  (foldr rmVersion s (e ^. versions))
                  (s ^. temp . toSubscriptionsFrom . at eId . non mempty)
              )
      put s'
      modify $ store . toEntities . at eId .~ Nothing
 where
  rmVersion :: VersionId -> Shared -> Shared
  rmVersion vId = execState (unsafeRemoveVersion vId)

  rmSubscription
    :: VersionId -> Shared -> Shared
  rmSubscription vId = execState (unsafeRemoveSubscription vId eId)

  rmFork :: Shared -> Shared
  rmFork = execState (unsafeUpdateFork eId Nothing)

unsafeRemoveSpace
  :: (MonadState Shared m)
  => SpaceId
  -> m ()
unsafeRemoveSpace sId = do
  s <- get
  case s ^. store . toSpaces . at sId of
    Nothing -> pure ()
    Just es -> do
      let s' = foldr rmEntity s es
      put s'
      modify $ store . toSpaces . at sId .~ Nothing
 where
  rmEntity :: EntityId -> Shared -> Shared
  rmEntity eId = execState (unsafeRemoveEntity eId)

unsafeRemoveMember
  :: (MonadState Shared m)
  => GroupId
  -> ActorId
  -> m ()
unsafeRemoveMember gId aId = do
  modify $ temp . toMemberOf . at aId . non mempty . at gId .~ Nothing
  modify $
    store . toGroups . nodes . at gId . non emptyGroup . members . at aId .~ Nothing

unsafeRemoveActor
  :: (MonadState Shared m)
  => ActorId
  -> m ()
unsafeRemoveActor aId = do
  s <- get
  for_ (s ^. temp . toMemberOf . at aId . non mempty) $ \gId ->
    unsafeRemoveMember gId aId
  modify $ store . toActors . at aId .~ Nothing

unsafeRemoveGroup
  :: (MonadState Shared m)
  => GroupId
  -> m ()
unsafeRemoveGroup gId = do
  s <- get
  case s ^. store . toGroups . nodes . at gId of
    Nothing -> pure ()
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
