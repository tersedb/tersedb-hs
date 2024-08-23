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

module Lib.Sync.Actions.Unsafe.Store where

import Lib.Sync.Actions.Tabulation (
  loadForks,
  loadRefsAndSubs,
  updateTabulationStartingAt,
 )
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (CollectionPermission (Blind), collectionPermission)
import Lib.Sync.Types.Store (
  Shared (..),
  store,
  temp,
  toActors,
  toMemberOf,
  toEntities,
  toGroups,
  toSpaces,
  toSpacesHiddenTo,
  toTabulatedGroups,
  toVersions,
 )
import Lib.Sync.Types.Store.Entity (initEntity, versions)
import Lib.Sync.Types.Store.Groups (
  emptyGroup,
  members,
  nodes,
  prev,
  roots,
 )
import Lib.Sync.Types.Store.Space (entities)
import Lib.Sync.Types.Store.Tabulation.Group (forUniverse)
import Lib.Sync.Types.Store.Version (initVersion)

import Control.Lens (at, ix, non, (%~), (&), (.~), (^.))
import Control.Monad.Extra (when)
import Control.Monad.State (MonadState (get, put), modify)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty ((<|))
import Data.Maybe (fromMaybe)

-- | Sets the group to empty
unsafeStoreGroup :: (MonadState Shared m) => GroupId -> m ()
unsafeStoreGroup gId = do
  modify $ store . toGroups . nodes . at gId %~ Just . fromMaybe emptyGroup
  s <- get
  -- save this group as a root
  when (null (s ^. store . toGroups . nodes . at gId . non emptyGroup . prev)) $
    modify $
      store . toGroups . roots . at gId .~ Just ()
  updateTabulationStartingAt gId

unsafeStoreActor :: (MonadState Shared m) => ActorId -> m ()
unsafeStoreActor aId = do
  modify $ store . toActors . at aId .~ Just ()

unsafeAddMember :: (MonadState Shared m) => GroupId -> ActorId -> m ()
unsafeAddMember gId aId = do
  modify $ temp . toMemberOf . at aId . non mempty . at gId .~ Just ()
  modify $
    store . toGroups . nodes . at gId . non emptyGroup . members . at aId .~ Just ()

-- | Sets the space to empty
unsafeStoreSpace :: (MonadState Shared m) => SpaceId -> m ()
unsafeStoreSpace sId = do
  s <- get
  let blindGroups =
        HM.keysSet $
          HM.filter
            (\t -> (t ^. forUniverse . collectionPermission) == Blind)
            (s ^. temp . toTabulatedGroups) -- FIXME use some kind of non-universally blind tracker
  put $
    s
      & store . toSpaces . at sId .~ Just mempty
      & temp . toSpacesHiddenTo . at sId
        .~ if null blindGroups then Nothing else Just blindGroups

unsafeStoreEntity
  :: (MonadState Shared m)
  => EntityId
  -> SpaceId
  -> VersionId
  -> Maybe VersionId
  -> m (Either VersionId ())
unsafeStoreEntity eId sId vId mForkId = do
  s <- get
  let s' =
        s
          & store . toEntities . at eId .~ Just (initEntity sId vId mForkId)
          & store . toSpaces . ix sId . entities . at eId .~ Just ()
          & store . toVersions . at vId .~ Just (initVersion eId)
  case do
    t' <- loadRefsAndSubs vId (s' ^. store) (s' ^. temp)
    pure $ loadForks eId (s' ^. store) t' of
    Left e -> pure (Left e)
    Right t -> Right () <$ put (s' & temp .~ t)

unsafeStoreVersion
  :: (MonadState Shared m)
  => EntityId
  -> VersionId
  -> m (Either VersionId ())
unsafeStoreVersion eId vId = do
  s <- get
  let s' =
        s
          & store . toEntities . ix eId . versions %~ (vId <|)
          & store . toVersions . at vId .~ Just (initVersion eId)
  case loadRefsAndSubs vId (s' ^. store) (s' ^. temp) of -- don't need to check forks
    Left e -> pure (Left e)
    Right t -> Right () <$ put (s' & temp .~ t)
