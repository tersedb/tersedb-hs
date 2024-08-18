module Lib.Actions.Gen where

import Lib.Actions.Safe.Store
  ( storeNextVersion
  , storeActor
  , storeGroup
  , storeSpace
  , storeEntity
  )
import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Monad (SheepdogM)

import Data.Maybe.HT (toMaybe)
import System.Random.Stateful (globalStdGen, Uniform (uniformM))
import Control.Monad.IO.Class (MonadIO)


generateWithAuthority
  :: ( MonadIO m
     , Uniform a
     ) => (a -> m Bool)
       -> m (Maybe a)
generateWithAuthority perform = do
  ident <- uniformM globalStdGen
  worked <- perform ident
  pure (toMaybe worked ident)

newGroup :: ActorId -> SheepdogM (Maybe GroupId)
newGroup = generateWithAuthority . storeGroup

newActor :: ActorId -> SheepdogM (Maybe ActorId)
newActor = generateWithAuthority . storeActor

newSpace :: ActorId -> SheepdogM (Maybe SpaceId)
newSpace = generateWithAuthority . storeSpace

newEntity
  :: ActorId
  -> SpaceId
  -> Maybe VersionId
  -> SheepdogM (Maybe (Either (Either VersionId EntityId) (EntityId, VersionId)))
newEntity creator sId mFork = do
  (eId, vId) <- uniformM globalStdGen
  mE <- storeEntity creator eId sId vId mFork
  pure $ fmap (fmap (const (eId, vId))) mE

-- Adds a version to an existing entity
newVersion
  :: ActorId
  -> EntityId
  -> SheepdogM (Maybe (Either (Either VersionId EntityId) VersionId))
newVersion creator eId = do
  vId <- uniformM globalStdGen
  mE <- storeNextVersion creator eId vId
  pure $ fmap (fmap (const vId)) mE
