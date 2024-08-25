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

module Lib.Sync.Types.Store where

import Control.Lens.TH (makeLensesFor)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Lib.Sync.Types.Store.Groups (Groups)
import Lib.Sync.Types.Store.Tabulation.Group (TabulatedPermissionsForGroup)
import Lib.Sync.Types.Store.Version (Version)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission,
  SinglePermission,
 )
import Lib.Sync.Types.Store.Entity (Entity)

data Temp = Temp
  { tempReferencesFrom :: HashMap VersionId (HashSet VersionId)
  , -- , tempReferencesFromEntities :: HashMap EntityId (HashSet VersionId)
    -- , tempReferencesFromSpaces :: HashMap SpaceId (HashSet VersionId)
    tempSubscriptionsFrom :: HashMap EntityId (HashSet VersionId)
  , -- , tempSubscriptionsFromSpaces :: HashMap SpaceId (HashSet VersionId)
    tempForksFrom :: HashMap VersionId (HashSet EntityId)
  , -- , tempForksFromEntities :: HashMap EntityId (HashSet EntityId)
    -- , tempForksFromSpaces :: HashMap SpaceId (HashSet EntityId)
    tempTabulatedGroups :: HashMap GroupId TabulatedPermissionsForGroup
  , tempSpacesHiddenTo :: HashMap SpaceId (HashSet GroupId)
  , tempMemberOf :: HashMap ActorId (HashSet GroupId)
  , tempSpaceOf :: HashMap EntityId SpaceId
  , tempEntityOf :: HashMap VersionId EntityId
  -- FIXME track universe blind groups
  }
  deriving (Eq, Show, Read)

makeLensesFor
  [ ("tempReferencesFrom", "toReferencesFrom")
  , -- , ("tempReferencesFromEntities", "toReferencesFromEntities")
    -- , ("tempReferencesFromSpaces", "toReferencesFromSpaces")
    ("tempSubscriptionsFrom", "toSubscriptionsFrom")
  , -- , ("tempSubscriptionsFromSpaces", "toSubscriptionsFromSpaces")
    ("tempForksFrom", "toForksFrom")
  , -- , ("tempForksFromEntities", "toForksFromEntities")
    -- , ("tempForksFromSpaces", "toForksFromSpaces")
    ("tempTabulatedGroups", "toTabulatedGroups")
  , ("tempSpacesHiddenTo", "toSpacesHiddenTo")
  , ("tempMemberOf", "toMemberOf")
  , ("tempSpaceOf", "toSpaceOf")
  , ("tempEntityOf", "toEntityOf")
  ]
  ''Temp

emptyTemp :: Temp
emptyTemp =
  Temp
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty

data Store = Store
  { storeGroups :: Groups
  , storeActors :: HashSet ActorId
  , storeSpaces :: HashMap SpaceId (HashSet EntityId)
  , storeEntities :: HashMap EntityId Entity
  , storeVersions :: HashMap VersionId Version
  , storeSpacePermissions :: HashMap GroupId (HashMap SpaceId SinglePermission)
  , storeEntityPermissions :: HashMap GroupId (HashMap SpaceId CollectionPermission)
  , storeGroupPermissions :: HashMap GroupId (HashMap GroupId SinglePermission)
  , storeMemberPermissions :: HashMap GroupId (HashMap GroupId CollectionPermission)
  }
  deriving (Eq, Show, Read)

makeLensesFor
  [ ("storeGroups", "toGroups")
  , ("storeActors", "toActors")
  , ("storeSpaces", "toSpaces")
  , ("storeEntities", "toEntities")
  , ("storeVersions", "toVersions")
  , ("storeSpacePermissions", "toSpacePermissions")
  , ("storeEntityPermissions", "toEntityPermissions")
  , ("storeGroupPermissions", "toGroupPermissions")
  , ("storeMemberPermissions", "toMemberPermissions")
  ]
  ''Store

data Shared = Shared
  { sharedStore :: Store
  , sharedTemp :: Temp
  }
  deriving (Eq, Show, Read)

makeLensesFor
  [ ("sharedStore", "store")
  , ("sharedTemp", "temp")
  ]
  ''Shared
