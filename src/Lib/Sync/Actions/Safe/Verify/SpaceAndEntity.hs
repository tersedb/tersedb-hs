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

module Lib.Sync.Actions.Safe.Verify.SpaceAndEntity (
  anyCanReadSpace,
  anyCanCreateSpace,
  anyCanUpdateSpace,
  anyCanDeleteSpace,
  anyCanReadAllEntities,
  anyCanReadEntity,
  anyCanCreateEntity,
  anyCanUpdateAllEntities,
  anyCanUpdateEntity,
  anyCanDeleteEntity,
  anyCanReadVersion,
  anyCanCreateVersion,
  anyCanUpdateVersion,
  anyCanDeleteVersion,
  hasEntityPermission,
  hasSpacePermission,
  anyCanReadSpaceOld,
) where

import Control.Lens (at, ix, non, (^.), (^?), (^?!))
import Control.Monad.Extra (allM, andM, anyM, orM)
import Control.Monad.State (MonadState, get)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Lib.Sync.Actions.Safe.Verify.Utils (
  canDo,
  canDoWithTab,
  withCollectionPermission,
 )
import Lib.Sync.Types.Store (
  Shared,
  store,
  temp,
  toEntities,
  toEntityOf,
  toForksFrom,
  toMemberOf,
  toReferencesFrom,
  toSpaceOf,
  toSpaces,
  toSpacesHiddenTo,
  toSubscriptionsFrom,
  toVersions,
 )
import Lib.Sync.Types.Store.Entity (versions)
import Lib.Sync.Types.Store.Tabulation.Group (
  forEntities,
  forSpaces,
  forUniverse,
 )
import Lib.Types.Id (ActorId, EntityId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption (..),
  SinglePermission,
  collectionPermission,
  escalate,
 )

-- * Spaces

canReadSpace :: (MonadState Shared m) => ActorId -> SpaceId -> m Bool
canReadSpace reader sId = do
  s <- get
  case s ^. temp . toMemberOf . at reader of
    Nothing -> pure False
    Just gs -> flip anyM (HS.toList gs) $ \gId ->
      if isJust (s ^. temp . toSpacesHiddenTo . ix sId . at gId)
        then
          canDo
            (^. forUniverse)
            reader
            (CollectionPermissionWithExemption Read True)
        else pure True

anyCanReadSpace
  :: (MonadState Shared m) => NonEmpty ActorId -> SpaceId -> m Bool
anyCanReadSpace readers sId =
  anyM (`canReadSpace` sId) (NE.toList readers)

-- legacy implementation that's slow
canReadSpaceOld :: (MonadState Shared m) => ActorId -> SpaceId -> m Bool
canReadSpaceOld reader sId =
  canDo (withCollectionPermission sId forUniverse forSpaces) reader Read

anyCanReadSpaceOld
  :: (MonadState Shared m) => NonEmpty ActorId -> SpaceId -> m Bool
anyCanReadSpaceOld readers sId =
  anyM (`canReadSpaceOld` sId) (NE.toList readers)

canCreateSpace :: (MonadState Shared m) => ActorId -> m Bool
canCreateSpace creater =
  canDo (\t -> t ^. forUniverse . collectionPermission) creater Create

anyCanCreateSpace :: (MonadState Shared m) => NonEmpty ActorId -> m Bool
anyCanCreateSpace = anyM canCreateSpace . NE.toList

canUpdateSpace :: (MonadState Shared m) => ActorId -> SpaceId -> m Bool
canUpdateSpace updater sId =
  canDo (withCollectionPermission sId forUniverse forSpaces) updater Update

anyCanUpdateSpace
  :: (MonadState Shared m) => NonEmpty ActorId -> SpaceId -> m Bool
anyCanUpdateSpace updaters sId =
  anyM (`canUpdateSpace` sId) (NE.toList updaters)

canDeleteSpace :: (MonadState Shared m) => ActorId -> SpaceId -> m Bool
canDeleteSpace deleter sId = do
  s <- get
  case s ^. store . toSpaces . at sId of
    Nothing -> pure False
    Just es -> do
      let vs :: HashSet VersionId =
            foldMap
              ( \eId ->
                  maybe mempty (HS.fromList . NE.toList) $
                    s ^? store . toEntities . ix eId . versions
              )
              es
          refs =
            HM.filterWithKey (\vId _ -> vId `HS.member` vs) (s ^. temp . toReferencesFrom)
          subs =
            HM.filterWithKey
              (\eId _ -> eId `HS.member` es)
              (s ^. temp . toSubscriptionsFrom)
          forks = HM.filterWithKey (\vId _ -> vId `HS.member` vs) (s ^. temp . toForksFrom)
      andM
        [ canDo
            (withCollectionPermission sId forUniverse forSpaces)
            deleter
            Delete
        , allM (canUpdateVersion deleter) . HS.toList . HS.unions $ HM.elems refs
        , allM (canUpdateVersion deleter) . HS.toList . HS.unions $ HM.elems subs
        , allM (canUpdateEntity deleter) . HS.toList . HS.unions $ HM.elems forks
        ]

anyCanDeleteSpace
  :: (MonadState Shared m) => NonEmpty ActorId -> SpaceId -> m Bool
anyCanDeleteSpace deleters sId =
  anyM (`canDeleteSpace` sId) (NE.toList deleters)

hasSpacePermission
  :: (MonadState Shared m) => ActorId -> SpaceId -> SinglePermission -> m Bool
hasSpacePermission aId sId p =
  canDoWithTab
    (withCollectionPermission sId forUniverse forSpaces)
    aId
    (\t -> escalate (t ^. forUniverse) p)

-- * Entities

canReadAllEntities :: (MonadState Shared m) => ActorId -> SpaceId -> m Bool
canReadAllEntities reader sId =
  andM
    [ canDo (\t -> t ^. forEntities . at sId . non Blind) reader Read
    , canReadSpace reader sId
    ]

anyCanReadAllEntities
  :: (MonadState Shared m) => NonEmpty ActorId -> SpaceId -> m Bool
anyCanReadAllEntities readers sId =
  anyM (`canReadAllEntities` sId) (NE.toList readers)

canReadEntity :: (MonadState Shared m) => ActorId -> EntityId -> m Bool
canReadEntity reader eId = do
  s <- get
  case s ^. temp . toSpaceOf . at eId of
    Nothing -> pure False
    Just sId -> canReadAllEntities reader sId

anyCanReadEntity
  :: (MonadState Shared m) => NonEmpty ActorId -> EntityId -> m Bool
anyCanReadEntity readers eId =
  anyM (`canReadEntity` eId) (NE.toList readers)

canCreateEntity :: (MonadState Shared m) => ActorId -> SpaceId -> m Bool
canCreateEntity creater sId =
  andM
    [ canDo (\t -> t ^. forEntities . at sId . non Blind) creater Create
    , canReadSpace creater sId
    ]

anyCanCreateEntity
  :: (MonadState Shared m) => NonEmpty ActorId -> SpaceId -> m Bool
anyCanCreateEntity creaters sId =
  anyM (`canCreateEntity` sId) (NE.toList creaters)

canUpdateAllEntities :: (MonadState Shared m) => ActorId -> SpaceId -> m Bool
canUpdateAllEntities updater sId =
  andM
    [ canDo (\t -> t ^. forEntities . at sId . non Blind) updater Update
    , canReadSpace updater sId
    ]

anyCanUpdateAllEntities
  :: (MonadState Shared m) => NonEmpty ActorId -> SpaceId -> m Bool
anyCanUpdateAllEntities updaters sId =
  anyM (`canUpdateAllEntities` sId) (NE.toList updaters)

canUpdateEntity :: (MonadState Shared m) => ActorId -> EntityId -> m Bool
canUpdateEntity reader eId = do
  s <- get
  case s ^. temp . toSpaceOf . at eId of
    Nothing -> pure False
    Just sId -> canUpdateAllEntities reader sId

anyCanUpdateEntity
  :: (MonadState Shared m) => NonEmpty ActorId -> EntityId -> m Bool
anyCanUpdateEntity updaters eId =
  anyM (`canUpdateEntity` eId) (NE.toList updaters)

canDeleteEntity
  :: (MonadState Shared m) => ActorId -> EntityId -> m Bool
canDeleteEntity deleter eId = do
  s <- get
  case s ^. store . toEntities . at eId of
    Nothing -> pure False
    Just e -> do
      let vs = HS.fromList . NE.toList $ e ^. versions
          refs =
            HM.filterWithKey (\vId _ -> vId `HS.member` vs) (s ^. temp . toReferencesFrom)
          subs = s ^. temp . toSubscriptionsFrom . at eId . non mempty
          forks = HM.filterWithKey (\vId _ -> vId `HS.member` vs) (s ^. temp . toForksFrom)
          sId = s ^?! temp . toSpaceOf . ix eId
      andM
        [ canDo (\t -> t ^. forEntities . at sId . non Blind) deleter Delete
        , canReadSpace deleter sId
        , allM (canUpdateVersion deleter) . HS.toList . HS.unions $ HM.elems refs
        , allM (canUpdateVersion deleter) $ HS.toList subs
        , allM (canUpdateEntity deleter) . HS.toList . HS.unions $ HM.elems forks
        ]

anyCanDeleteEntity
  :: (MonadState Shared m) => NonEmpty ActorId -> EntityId -> m Bool
anyCanDeleteEntity deleters eId =
  anyM (`canDeleteEntity` eId) (NE.toList deleters)

hasEntityPermission
  :: (MonadState Shared m) => ActorId -> SpaceId -> CollectionPermission -> m Bool
hasEntityPermission aId sId p =
  andM
    [ canReadSpace aId sId
    , orM
        [ canDo (\t -> t ^. forEntities . at sId . non Blind) aId p
        , canDo (\t -> t ^. forUniverse . collectionPermission) aId Update
        ]
    ]

canReadVersion :: (MonadState Shared m) => ActorId -> VersionId -> m Bool
canReadVersion updater vId = do
  s <- get
  case s ^. temp . toEntityOf . at vId of
    Nothing -> pure False
    Just eId -> canReadEntity updater eId

anyCanReadVersion
  :: (MonadState Shared m) => NonEmpty ActorId -> VersionId -> m Bool
anyCanReadVersion readers eId =
  anyM (`canReadVersion` eId) (NE.toList readers)

canCreateVersion :: (MonadState Shared m) => ActorId -> EntityId -> m Bool
canCreateVersion = canUpdateEntity

anyCanCreateVersion
  :: (MonadState Shared m) => NonEmpty ActorId -> EntityId -> m Bool
anyCanCreateVersion creaters eId =
  anyM (`canCreateVersion` eId) (NE.toList creaters)

canUpdateVersion :: (MonadState Shared m) => ActorId -> VersionId -> m Bool
canUpdateVersion updater vId = do
  s <- get
  case s ^. temp . toEntityOf . at vId of
    Nothing -> pure False
    Just eId -> canUpdateEntity updater eId

anyCanUpdateVersion
  :: (MonadState Shared m) => NonEmpty ActorId -> VersionId -> m Bool
anyCanUpdateVersion updaters eId =
  anyM (`canUpdateVersion` eId) (NE.toList updaters)

-- | FIXME doesn't check to see if this is the last version in its entity
canDeleteVersion :: (MonadState Shared m) => ActorId -> VersionId -> m Bool
canDeleteVersion updater vId = do
  s <- get
  case s ^. temp . toEntityOf . at vId of
    Nothing -> pure False
    Just eId -> canUpdateEntity updater eId

anyCanDeleteVersion
  :: (MonadState Shared m) => NonEmpty ActorId -> VersionId -> m Bool
anyCanDeleteVersion deleters eId =
  anyM (`canDeleteVersion` eId) (NE.toList deleters)

canDeleteVersionAndEntity
  :: (MonadState Shared m) => ActorId -> VersionId -> m Bool
canDeleteVersionAndEntity updater vId = do
  s <- get
  case s ^. temp . toEntityOf . at vId of
    Nothing -> pure False
    Just eId -> canDeleteEntity updater eId
