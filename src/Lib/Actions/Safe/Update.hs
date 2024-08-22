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


module Lib.Actions.Safe.Update where

import Control.Lens (at, ix, (.~), (?~), (^.))
import Control.Monad.Extra (allM, andM)
import Control.Monad.State (MonadState (get), modify)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty)
import Lib.Actions.Safe.Verify (
  anyCanCreateEntity,
  anyCanDeleteEntity,
  anyCanReadEntity,
  anyCanReadVersion,
  anyCanUpdateEntity,
  anyCanUpdateVersion,
  conditionally,
 )
import Lib.Actions.Unsafe.Update (
  unsafeAddReference,
  unsafeAddSubscription,
  unsafeMoveEntity,
  unsafeOffsetVersionIndex,
  unsafeRemoveReference,
  unsafeRemoveSubscription,
  unsafeSetVersionIndex,
  unsafeUpdateFork,
  unsafeUpdateVersionReferences,
  unsafeUpdateVersionSubscriptions,
 )
import Lib.Types.Id (ActorId, EntityId, SpaceId, VersionId)
import Lib.Types.Store (
  Shared,
  store,
  toEntities,
  toSpaces,
  toVersions,
 )
import Lib.Types.Store.Entity (space)
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Version (entity)

-- | Moving an entity between spaces requires delete authority on the current space, and create authority on the destination space
updateEntitySpace
  :: (MonadState Shared m) => NonEmpty ActorId -> EntityId -> SpaceId -> m Bool
updateEntitySpace updater eId newSId = do
  s <- get
  case s ^. store . toEntities . at eId of
    Nothing -> pure False -- FIXME
    Just e -> do
      canAdjust <-
        andM
          [ anyCanDeleteEntity updater eId
          , anyCanCreateEntity updater newSId
          ]
      flip conditionally canAdjust $ do
        modify $ store . toEntities . ix eId . space .~ newSId
        modify $ store . toSpaces . ix (e ^. space) . entities . at eId .~ Nothing
        modify $ store . toSpaces . ix newSId . entities . at eId ?~ ()

updateVersionReferences
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> VersionId
  -> HashSet VersionId
  -> m (Maybe (Either VersionId ()))
updateVersionReferences updater vId refIds = do
  canAdjust <-
    andM
      [ allM (anyCanReadVersion updater) (HS.toList refIds)
      , anyCanUpdateVersion updater vId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeUpdateVersionReferences vId refIds

addReference
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> VersionId
  -> VersionId
  -> m (Maybe (Either VersionId ()))
addReference updater vId refId = do
  canAdjust <-
    andM
      [ anyCanReadVersion updater refId
      , anyCanUpdateVersion updater vId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeAddReference vId refId

removeReference
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> VersionId
  -> VersionId
  -> m (Maybe (Either VersionId ()))
removeReference updater vId refId = do
  canAdjust <-
    andM
      [ anyCanReadVersion updater refId
      , anyCanUpdateVersion updater vId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeRemoveReference vId refId

updateVersionSubscriptions
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> VersionId
  -> HashSet EntityId
  -> m (Maybe (Either VersionId ()))
updateVersionSubscriptions updater vId subIds = do
  canAdjust <-
    andM
      [ allM (anyCanReadEntity updater) (HS.toList subIds)
      , anyCanUpdateVersion updater vId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeUpdateVersionSubscriptions vId subIds

addSubscription
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> VersionId
  -> EntityId
  -> m (Maybe (Either VersionId ()))
addSubscription updater vId subId = do
  canAdjust <-
    andM
      [ anyCanReadEntity updater subId
      , anyCanUpdateVersion updater vId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeAddSubscription vId subId

removeSubscription
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> VersionId
  -> EntityId
  -> m (Maybe (Either VersionId ()))
removeSubscription updater vId subId = do
  canAdjust <-
    andM
      [ anyCanReadEntity updater subId
      , anyCanUpdateVersion updater vId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeRemoveSubscription vId subId

updateFork
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> EntityId
  -> Maybe VersionId
  -> m (Maybe (Either EntityId ()))
updateFork updater eId mFork = do
  canAdjust <-
    andM
      [ anyCanUpdateEntity updater eId
      , maybe (pure True) (anyCanReadVersion updater) mFork
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeUpdateFork eId mFork

moveEntity
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> EntityId
  -> SpaceId
  -> m (Maybe (Either EntityId ()))
moveEntity updater eId newSId = do
  canAdjust <-
    andM
      [ anyCanUpdateEntity updater eId
      , anyCanCreateEntity updater newSId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeMoveEntity eId newSId

offsetVersionIndex
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> VersionId
  -> Int
  -> m (Maybe (Either VersionId ()))
offsetVersionIndex updater vId offset = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure . Just $ Left vId
    Just v -> do
      canAdjust <- anyCanUpdateEntity updater (v ^. entity)
      if not canAdjust
        then pure Nothing
        else Just <$> unsafeOffsetVersionIndex vId offset

setVersionIndex
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> VersionId
  -> Int
  -> m (Maybe (Either VersionId ()))
setVersionIndex updater vId idx = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure . Just $ Left vId
    Just v -> do
      canAdjust <- anyCanUpdateEntity updater (v ^. entity)
      if not canAdjust
        then pure Nothing
        else Just <$> unsafeSetVersionIndex vId idx
