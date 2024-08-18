module Lib.Types.Store where

import Lib.Types.Store.Tabulation.Group (TabulatedPermissionsForGroup)
import Lib.Types.Store.Groups (Groups)
import Lib.Types.Store.Space (Space)
import Lib.Types.Store.Entity (Entity)
import Lib.Types.Store.Version (Version)
import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission
  , SinglePermission
  )

import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Control.Lens.TH (makeLensesFor)


data Temp = Temp
  { tempReferencesFrom :: HashMap VersionId (HashSet VersionId)
  , tempReferencesFromEntities :: HashMap EntityId (HashSet VersionId)
  , tempReferencesFromSpaces :: HashMap SpaceId (HashSet VersionId)
  , tempSubscriptionsFrom :: HashMap EntityId (HashSet VersionId)
  , tempSubscriptionsFromSpaces :: HashMap SpaceId (HashSet VersionId)
  , tempForksFrom :: HashMap VersionId (HashSet EntityId)
  , tempForksFromEntities :: HashMap EntityId (HashSet EntityId)
  , tempForksFromSpaces :: HashMap SpaceId (HashSet EntityId)
  , tempTabulatedGroups :: HashMap GroupId TabulatedPermissionsForGroup
  , tempSpacesHiddenTo :: HashMap SpaceId (HashSet GroupId)
  -- FIXME track universe blind groups
  } deriving (Eq, Show, Read)
makeLensesFor
  [ ("tempReferencesFrom", "toReferencesFrom")
  , ("tempReferencesFromEntities", "toReferencesFromEntities")
  , ("tempReferencesFromSpaces", "toReferencesFromSpaces")
  , ("tempSubscriptionsFrom", "toSubscriptionsFrom")
  , ("tempSubscriptionsFromSpaces", "toSubscriptionsFromSpaces")
  , ("tempForksFrom", "toForksFrom")
  , ("tempForksFromEntities", "toForksFromEntities")
  , ("tempForksFromSpaces", "toForksFromSpaces")
  , ("tempTabulatedGroups", "toTabulatedGroups")
  , ("tempSpacesHiddenTo", "toSpacesHiddenTo")
  ] ''Temp


emptyTemp :: Temp
emptyTemp = Temp
  mempty
  mempty
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
  , storeActors :: HashMap ActorId (HashSet GroupId)
  , storeSpaces :: HashMap SpaceId Space
  , storeEntities :: HashMap EntityId Entity
  , storeVersions :: HashMap VersionId Version
  , storeSpacePermissions :: HashMap GroupId (HashMap SpaceId SinglePermission)
  , storeEntityPermissions :: HashMap GroupId (HashMap SpaceId CollectionPermission)
  , storeGroupPermissions :: HashMap GroupId (HashMap GroupId SinglePermission)
  , storeMemberPermissions :: HashMap GroupId (HashMap GroupId CollectionPermission)
  } deriving (Eq, Show, Read)
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
  ] ''Store


data Shared = Shared
  { sharedStore :: Store
  , sharedTemp :: Temp
  } deriving (Eq, Show, Read)
makeLensesFor
  [ ("sharedStore", "store")
  , ("sharedTemp", "temp")
  ] ''Shared
