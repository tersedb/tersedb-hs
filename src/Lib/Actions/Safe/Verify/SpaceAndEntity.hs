module Lib.Actions.Safe.Verify.SpaceAndEntity where

import Lib.Actions.Safe.Verify.Utils (canDo, canDoWithTab, withCollectionPermission)
import Lib.Types.Store
  ( Shared
  , store
  , temp
  , toReferencesFromEntities
  , toReferencesFromSpaces
  , toSubscriptionsFrom
  , toSubscriptionsFromSpaces
  , toVersions
  , toEntities
  , toActors
  , toSpacesHiddenTo
  )
import Lib.Types.Store.Tabulation.Group
  ( TabulatedPermissionsForGroup
  , forUniverse
  , forSpaces
  , forEntities
  )
import Lib.Types.Store.Entity (space)
import Lib.Types.Store.Version (entity)
import Lib.Types.Id (ActorId, SpaceId, EntityId, VersionId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption (..)
  , SinglePermission (NonExistent)
  , escalate
  , collectionPermission
  )

import Data.Maybe (fromMaybe, fromJust, isNothing, isJust)
import qualified Data.HashSet as HS
import Control.Lens ((^.), at, non, ix)
import Control.Monad.State (MonadState, get)
import Control.Monad.Extra (andM, orM, anyM)

-- * Spaces

canReadSpace :: MonadState Shared m => ActorId -> SpaceId -> m Bool
canReadSpace reader sId = do
  s <- get
  case s ^. store . toActors . at reader of
    Nothing -> pure False
    Just gs -> flip anyM (HS.toList gs) $ \gId ->
      if isJust (s ^. temp . toSpacesHiddenTo . ix sId . at gId)
      then canDo
            (\t -> t ^. forUniverse)
            reader
            (CollectionPermissionWithExemption Read True)
      else pure True

canReadSpaceOld :: MonadState Shared m => ActorId -> SpaceId -> m Bool
canReadSpaceOld reader sId =
  canDo (withCollectionPermission sId forUniverse forSpaces) reader Read

canCreateSpace :: MonadState Shared m => ActorId -> m Bool
canCreateSpace creater =
  canDo (\t -> t ^. forUniverse . collectionPermission) creater Create

canUpdateSpace :: MonadState Shared m => ActorId -> SpaceId -> m Bool
canUpdateSpace updater sId =
  canDo (withCollectionPermission sId forUniverse forSpaces) updater Update

canDeleteSpace :: MonadState Shared m => ActorId -> SpaceId -> m Bool
canDeleteSpace deleter sId = do
  refsAndSubs <- do
    s <- get
    let refs = s ^. temp . toReferencesFromSpaces . at sId . non mempty
        subs = s ^. temp . toSubscriptionsFromSpaces . at sId . non mempty
    pure (HS.union refs subs)
  andM $
    (canDo
      (withCollectionPermission sId forUniverse forSpaces)
      deleter
      Delete) : (map (canUpdateVersion deleter) $ HS.toList refsAndSubs)

hasSpacePermission :: MonadState Shared m => ActorId -> SpaceId -> SinglePermission -> m Bool
hasSpacePermission aId sId p =
  canDoWithTab
    (withCollectionPermission sId forUniverse forSpaces)
    aId
    (\t -> escalate (t ^. forUniverse) p)

-- * Entities

canReadAllEntities :: MonadState Shared m => ActorId -> SpaceId -> m Bool
canReadAllEntities reader sId = andM
  [ canDo (\t -> t ^. forEntities . at sId . non Blind) reader Read
  , canReadSpace reader sId
  ]

canReadEntity :: MonadState Shared m => ActorId -> EntityId -> m Bool
canReadEntity reader eId = do
  s <- get
  case s ^. store . toEntities . at eId of
    Nothing -> pure False
    Just e -> canReadAllEntities reader (e ^. space)

canCreateEntity :: MonadState Shared m => ActorId -> SpaceId -> m Bool
canCreateEntity creater sId = andM
  [ canDo (\t -> t ^. forEntities . at sId . non Blind) creater Create
  , canReadSpace creater sId
  ]

canUpdateAllEntities :: MonadState Shared m => ActorId -> SpaceId -> m Bool
canUpdateAllEntities updater sId = andM
  [ canDo (\t -> t ^. forEntities . at sId . non Blind) updater Update
  , canReadSpace updater sId
  ]

canUpdateEntity :: MonadState Shared m => ActorId -> EntityId -> m Bool
canUpdateEntity reader eId = do
  s <- get
  case s ^. store . toEntities . at eId of
    Nothing -> pure False
    Just e -> canUpdateAllEntities reader (e ^. space)

canDeleteEntity :: MonadState Shared m => ActorId -> SpaceId -> EntityId -> m Bool
canDeleteEntity deleter sId eId = do
  refsAndSubs <- do
    s <- get
    let refs = s ^. temp . toReferencesFromEntities . at eId . non mempty
        subs = s ^. temp . toSubscriptionsFrom . at eId . non mempty
    pure (HS.union refs subs)
  andM $
    (canDo (\t -> t ^. forEntities . at sId . non Blind) deleter Delete) :
    (canReadSpace deleter sId) :
    (map (canUpdateVersion deleter) $ HS.toList refsAndSubs)

hasEntityPermission :: MonadState Shared m => ActorId -> SpaceId -> CollectionPermission -> m Bool
hasEntityPermission aId sId p = andM
  [ canReadSpace aId sId
  , orM
    [ canDo (\t -> t ^. forEntities . at sId . non Blind) aId p
    , canDo (\t -> t ^. forUniverse . collectionPermission) aId Update
    ]
  ]

canReadVersion :: MonadState Shared m => ActorId -> VersionId -> m Bool
canReadVersion updater vId = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure False
    Just v -> canReadEntity updater (v ^. entity)

canCreateVersion :: MonadState Shared m => ActorId -> EntityId -> m Bool
canCreateVersion = canUpdateEntity

canUpdateVersion :: MonadState Shared m => ActorId -> VersionId -> m Bool
canUpdateVersion updater vId = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure False
    Just v -> canUpdateEntity updater (v ^. entity)

-- | FIXME doesn't check to see if this is the last version in its entity
canDeteteVersion :: MonadState Shared m => ActorId -> VersionId -> m Bool
canDeteteVersion updater vId = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure False
    Just v -> canUpdateEntity updater (v ^. entity)

canDeteteVersionAndEntity :: MonadState Shared m => ActorId -> VersionId -> m Bool
canDeteteVersionAndEntity updater vId = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure False
    Just v -> case s ^. store . toEntities . at (v ^. entity) of
      Nothing -> pure False
      Just e -> canDeleteEntity updater (e ^. space) (v ^. entity)
