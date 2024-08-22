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

module Lib.Actions.Safe.Remove where

import Control.Lens (ix, (^.), (^?))
import Control.Monad.Extra (allM, andM)
import Control.Monad.State (MonadState (get))
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty)
import Lib.Actions.Safe.Verify (
  anyCanDeleteActor,
  anyCanDeleteEntity,
  anyCanDeleteGroup,
  anyCanDeleteMember,
  anyCanDeleteSpace,
  anyCanDeleteVersion,
  anyCanUpdateVersion,
  conditionally,
 )
import Lib.Actions.Unsafe.Remove (
  unsafeRemoveActor,
  unsafeRemoveEntity,
  unsafeRemoveGroup,
  unsafeRemoveMember,
  unsafeRemoveSpace,
  unsafeRemoveVersion,
 )
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Store (
  Shared,
  store,
  temp,
  toEntities,
  toReferencesFrom,
  toSpaces,
  toSubscriptionsFrom,
 )
import Lib.Types.Store.Entity (space)
import Lib.Types.Store.Space (entities)

removeVersion
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> VersionId
  -> m (Maybe (Either (Either VersionId EntityId) ()))
removeVersion remover vId = do
  s <- get
  canAdjust <-
    andM
      [ anyCanDeleteVersion remover vId
      , case s ^? temp . toReferencesFrom . ix vId of
          Nothing -> pure True
          Just refs -> allM (anyCanUpdateVersion remover) (HS.toList refs)
          -- NOTE we don't need to check for `canReadVersion` or `canReadEntity` for
          -- this version's references and subscriptions, because we're essentially
          -- making this version blind to them by deleting it
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeRemoveVersion vId

removeEntity
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> EntityId
  -> m (Maybe (Either (Either VersionId EntityId) ()))
removeEntity remover eId = do
  s <- get
  canAdjust <-
    andM
      [ anyCanDeleteEntity remover eId
      , case s ^? temp . toSubscriptionsFrom . ix eId of
          Nothing -> pure True
          Just subs -> allM (anyCanUpdateVersion remover) (HS.toList subs)
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeRemoveEntity eId

removeSpace
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> SpaceId
  -> m (Maybe (Either (Either (Either VersionId EntityId) SpaceId) ()))
removeSpace remover sId = do
  s <- get
  canAdjust <- case s ^? store . toSpaces . ix sId of
    Nothing -> pure False
    Just s ->
      andM
        [ anyCanDeleteSpace remover sId
        , allM (anyCanDeleteEntity remover) (HS.toList (s ^. entities))
        ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeRemoveSpace sId

removeMember
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> GroupId
  -> ActorId
  -> m Bool
removeMember remover gId aId =
  anyCanDeleteMember remover gId >>= conditionally (unsafeRemoveMember gId aId)

removeActor
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> ActorId
  -> m Bool
removeActor remover aId =
  anyCanDeleteActor remover aId >>= conditionally (unsafeRemoveActor aId)

removeGroup
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> GroupId
  -> m (Maybe (Either GroupId ()))
removeGroup remover gId = do
  canAdjust <- anyCanDeleteGroup remover gId
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeRemoveGroup gId
