module Lib.Actions.Gen where

import Lib.Actions.Safe.Store (
  storeActor,
  storeEntity,
  storeGroup,
  storeNextVersion,
  storeSpace,
 )
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Monad (SheepdogM)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe.HT (toMaybe)
import Data.List.NonEmpty (NonEmpty)
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
