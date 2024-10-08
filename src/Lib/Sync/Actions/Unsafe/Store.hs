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

import Control.Lens (at, ix, non, (%~), (&), (.~), (?~), (^.))
import Control.Monad.Extra (when)
import Control.Monad.State (MonadState (get, put), modify)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty ((<|))
import Data.Maybe (fromMaybe)
import Lib.Sync.Actions.Tabulation (
  loadForks,
  loadRefsAndSubs,
  updateTabulationStartingAt,
 )
import Lib.Sync.Types.Store (
  Shared (..),
  store,
  temp,
  toActors,
  toEntities,
  toEntityOf,
  toGroups,
  toMemberOf,
  toSpaceOf,
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
import Lib.Sync.Types.Store.Tabulation.Group (forUniverse)
import Lib.Sync.Types.Store.Version (initVersion)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (CollectionPermission (Blind), collectionPermission)

-- | Sets the group to empty
unsafeStoreGroup :: (MonadState Shared m) => GroupId -> m ()
unsafeStoreGroup gId = do
  modify $ store . toGroups . nodes . at gId %~ Just . fromMaybe emptyGroup
  s <- get
  -- save this group as a root
  when (null (s ^. store . toGroups . nodes . at gId . non emptyGroup . prev)) $
    modify $
      store . toGroups . roots . at gId ?~ ()
  updateTabulationStartingAt gId

unsafeStoreActor :: (MonadState Shared m) => ActorId -> m ()
unsafeStoreActor aId = do
  modify $ store . toActors . at aId ?~ ()

unsafeAddMember :: (MonadState Shared m) => GroupId -> ActorId -> m ()
unsafeAddMember gId aId = do
  modify $ temp . toMemberOf . at aId . non mempty . at gId ?~ ()
  modify $
    store . toGroups . nodes . at gId . non emptyGroup . members . at aId ?~ ()

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
      & store . toSpaces . at sId ?~ mempty
      & temp . toSpacesHiddenTo . at sId
        .~ if null blindGroups then Nothing else Just blindGroups

unsafeStoreEntity
  :: (MonadState Shared m)
  => EntityId
  -> SpaceId
  -> VersionId
  -> Maybe VersionId
  -> m ()
unsafeStoreEntity eId sId vId mForkId = do
  s <- get
  let s' =
        s
          & store . toEntities . at eId ?~ initEntity vId mForkId
          & store . toSpaces . ix sId . at eId ?~ ()
          & store . toVersions . at vId ?~ initVersion
          & temp . toSpaceOf . at eId ?~ sId
          & temp . toEntityOf . at vId ?~ eId
      t = loadForks eId (s' ^. store) $ loadRefsAndSubs vId (s' ^. store) (s' ^. temp)
  put (s' & temp .~ t)

unsafeStoreVersion
  :: (MonadState Shared m)
  => EntityId
  -> VersionId
  -> m ()
unsafeStoreVersion eId vId = do
  s <- get
  let s' =
        s
          & store . toEntities . ix eId . versions %~ (vId <|)
          & store . toVersions . at vId ?~ initVersion
          & temp . toEntityOf . at vId ?~ eId
      t = loadRefsAndSubs vId (s' ^. store) (s' ^. temp)
  put (s' & temp .~ t)
