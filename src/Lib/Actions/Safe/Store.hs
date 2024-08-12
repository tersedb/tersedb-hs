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

module Lib.Actions.Safe.Store where

import Lib.Actions.Safe.Verify (canDo, conditionally)
import Lib.Actions.Tabulation (LoadRefsAndSubsError)
import Lib.Actions.Unsafe.Store
  ( unsafeStoreGroup
  , unsafeStoreActor
  , unsafeStoreSpace
  , unsafeStoreEntity
  , unsafeStoreVersion
  , unsafeAddMember
  )
import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , collectionPermission
  )
import Lib.Types.Store
  ( Shared
  , store
  , toEntities
  , toVersions
  )
import Lib.Types.Store.Tabulation.Group
  ( forUniverse
  , forOrganization
  , forRecruiter
  , forEntities
  , forMembers
  )
import Lib.Types.Store.Entity (space, versions)
import Lib.Types.Store.Version (genesisVersion, forkVersion, entity)

import qualified Data.List.NonEmpty as NE
import Control.Lens ((^.), at, non, _Left, (%~), (&))
import Control.Monad.State (MonadState (get))
import Control.Monad.Extra (andM)



storeGroup
  :: MonadState Shared m
  => ActorId -- ^ actor storing the group
  -> GroupId -- ^ group being stored
  -> m Bool
storeGroup creator gId = do
  canDo (\t -> t ^. forOrganization . collectionPermission) creator Create >>=
    conditionally (unsafeStoreGroup gId)

storeActor
  :: MonadState Shared m
  => ActorId -- ^ actor storing the created actor
  -> ActorId -- ^ created actor being stored
  -> m Bool
storeActor creator aId =
  canDo (\t -> t ^. forRecruiter) creator Create >>=
    conditionally (unsafeStoreActor aId)

addMember
  :: MonadState Shared m
  => ActorId -- ^ actor creating membership
  -> GroupId -- ^ group gaining a member
  -> ActorId -- ^ new member
  -> m Bool
addMember creator gId aId = do
  canDo (\t -> t ^. forMembers . at gId . non Blind) creator Create >>=
    conditionally (unsafeAddMember gId aId)

storeSpace
  :: MonadState Shared m
  => ActorId -- ^ actor storing the space
  -> SpaceId -- ^ space being created
  -> m Bool
storeSpace creator sId =
  canDo (\t -> t ^. forUniverse . collectionPermission) creator Create >>=
    conditionally (unsafeStoreSpace sId)

storeEntity
  :: MonadState Shared m
  => ActorId -- ^ actor storing the entity
  -> EntityId -- ^ entity being stored
  -> SpaceId -- ^ space in which entity is being stored
  -> VersionId -- ^ initial version
  -> m Bool
storeEntity creator eId sId vId = do
  canAdjust <- canDo (\t -> t ^. forEntities . at sId . non Blind) creator Create
  if not canAdjust then pure False else do
    eVErr <- unsafeStoreEntity eId sId vId genesisVersion
    case eVErr of
      Left e -> error $ "Impossible error - genesis version has undefined reference " <> show (e, eId, sId, vId)
      Right () -> pure True

data StoreForkedEntityError
  = PreviousVersionDoesntExist
  | PreviousEntityDoesntExist
  | ForkingSelf
  deriving (Eq, Show, Read)

storeForkedEntity
  :: MonadState Shared m
  => ActorId -- ^ actor forking the entity
  -> EntityId -- ^ entity being forked
  -> SpaceId -- ^ space in which entity is being forked to
  -> VersionId -- ^ initial version of forked entitiy
  -> VersionId -- ^ previous version of entity (possibly in a different space, definitely in a different entity)
  -> m (Maybe (Either (Either StoreForkedEntityError LoadRefsAndSubsError) ()))
storeForkedEntity creator eId sId vId prevVId
  | vId == prevVId = pure . Just . Left $ Left ForkingSelf
  | otherwise = do
  s <- get
  case s ^. store . toVersions . at prevVId of
    Nothing -> pure . Just . Left $ Left PreviousVersionDoesntExist
    Just prevV -> case s ^. store . toEntities . at (prevV ^. entity) of
      Nothing -> pure . Just . Left $ Left PreviousEntityDoesntExist
      Just prevE -> do
        let prevSId = prevE ^. space
        canAdjust <- andM
          [ canDo (\t -> t ^. forEntities . at sId . non Blind) creator Create
          , canDo (\t -> t ^. forEntities . at prevSId . non Blind) creator Read
          ]
        if not canAdjust then pure Nothing else do
          eVErr <- unsafeStoreEntity eId sId vId (flip forkVersion prevVId)
          pure . Just $ eVErr & _Left %~ Right

storeVersion
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to store a version
  -> EntityId -- ^ entity receiving a new version
  -> VersionId -- ^ version being stored
  -> m (Maybe (Either LoadRefsAndSubsError ()))
storeVersion creator eId vId = do
  s <- get
  case s ^. store . toEntities . at eId of
    Nothing -> pure Nothing
    Just e -> do
      canAdjust <- canDo (\t -> t ^. forEntities . at (e ^. space) . non Blind) creator Create
      if not canAdjust then pure Nothing else do
        fmap Just . unsafeStoreVersion eId vId . flip forkVersion . NE.head $ e ^. versions
