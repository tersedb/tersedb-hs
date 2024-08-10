{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
  , DerivingVia
  , DataKinds
  , DeriveGeneric
  , RankNTypes
  , TemplateHaskell
  , FlexibleContexts
  #-}

module Lib.Actions.Gen where

import Lib.Actions.Safe
  ( storeVersion
  , storeActor
  , storeGroup
  , storeSpace
  , storeEntity
  , storeForkedEntity
  , StoreForkedEntityError
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

newEntity :: ActorId -> SpaceId -> SheepdogM (Maybe (EntityId, VersionId))
newEntity creator sId =
  generateWithAuthority (\(eId, vId) -> storeEntity creator eId sId vId)

-- Forks a new entity from a version in an existing entity
newForkedEntity
  :: ActorId
  -> SpaceId
  -> VersionId
  -> SheepdogM (Either StoreForkedEntityError (Maybe (EntityId, VersionId)))
newForkedEntity creator sId prevVId = do
  (eId, vId) <- uniformM globalStdGen
  eWorked <- storeForkedEntity creator eId sId vId prevVId
  pure $ fmap (flip toMaybe (eId, vId)) eWorked

-- Adds a version to an existing entity
newVersion
  :: ActorId
  -> EntityId
  -> SheepdogM (Maybe VersionId)
newVersion creator eId =
  generateWithAuthority (\vId -> storeVersion creator eId vId)
