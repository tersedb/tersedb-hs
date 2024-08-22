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

module Lib.Actions.Gen where

import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe.HT (toMaybe)
import Lib.Actions.Safe.Store (
  storeActor,
  storeEntity,
  storeGroup,
  storeNextVersion,
  storeSpace,
 )
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Monad (SheepdogM)
import System.Random.Stateful (Uniform (uniformM), globalStdGen)

generateWithAuthority
  :: ( MonadIO m
     , Uniform a
     )
  => (a -> m Bool)
  -> m (Maybe a)
generateWithAuthority perform = do
  ident <- uniformM globalStdGen
  worked <- perform ident
  pure (toMaybe worked ident)

newGroup :: NonEmpty ActorId -> SheepdogM (Maybe GroupId)
newGroup = generateWithAuthority . storeGroup

newActor :: NonEmpty ActorId -> SheepdogM (Maybe ActorId)
newActor = generateWithAuthority . storeActor

newSpace :: NonEmpty ActorId -> SheepdogM (Maybe SpaceId)
newSpace = generateWithAuthority . storeSpace

newEntity
  :: NonEmpty ActorId
  -> SpaceId
  -> Maybe VersionId
  -> SheepdogM (Maybe (Either VersionId (EntityId, VersionId)))
newEntity creator sId mFork = do
  (eId, vId) <- uniformM globalStdGen
  mE <- storeEntity creator eId sId vId mFork
  pure $ fmap (fmap (const (eId, vId))) mE

-- Adds a version to an existing entity
newVersion
  :: NonEmpty ActorId
  -> EntityId
  -> SheepdogM (Maybe (Either VersionId VersionId))
newVersion creator eId = do
  vId <- uniformM globalStdGen
  mE <- storeNextVersion creator eId vId
  pure $ fmap (fmap (const vId)) mE
