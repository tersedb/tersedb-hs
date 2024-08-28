module Lib.Async.Types.Store where

import Control.Concurrent.STM (STM)
import Control.Lens.TH (makeLensesFor)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Lib.Async.Types.Tabulation (TabulatedPermissions)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission,
  CollectionPermissionWithExemption,
  SinglePermission,
 )
import StmContainers.Map (Map)
import qualified StmContainers.Map as Map
import StmContainers.Multimap (Multimap)
import qualified StmContainers.Multimap as Multimap
import StmContainers.Set (Set)
import qualified StmContainers.Set as Set

data Temp = Temp
  { tempReferencesFrom :: Multimap VersionId VersionId
  , tempSubscriptionsFrom :: Multimap EntityId VersionId
  , tempForksFrom :: Multimap VersionId EntityId
  , tempSpacesHiddenTo :: Multimap SpaceId GroupId
  , tempMemberOf :: Multimap ActorId GroupId
  , tempSpaceOf :: Map EntityId SpaceId
  , tempEntityOf :: Map VersionId EntityId
  , tempTabulatedPermissionsUniverse
      :: Map GroupId CollectionPermissionWithExemption
  , tempTabulatedPermissionsOrganization
      :: Map GroupId CollectionPermissionWithExemption
  , tempTabulatedPermissionsRecruiter :: Map GroupId CollectionPermission
  , tempTabulatedPermissionsOther :: Map GroupId TabulatedPermissions
  }
  deriving (Generic)
makeLensesFor
  [ ("tempReferencesFrom", "toReferencesFrom")
  , ("tempSubscriptionsFrom", "toSubscriptionsFrom")
  , ("tempForksFrom", "toForksFrom")
  , ("tempSpacesHiddenTo", "toSpacesHiddenTo")
  , ("tempMemberOf", "toMemberOf")
  , ("tempSpaceOf", "toSpaceOf")
  , ("tempEntityOf", "toEntityOf")
  , ("tempTabulatedPermissionsUniverse", "toTabUniverse")
  , ("tempTabulatedPermissionsOrganization", "toTabOrganization")
  , ("tempTabulatedPermissionsRecruiter", "toTabRecruiter")
  , ("tempTabulatedPermissionsOther", "toTabOther")
  ]
  ''Temp

newTemp :: STM Temp
newTemp = do
  rf <- Multimap.new
  sf <- Multimap.new
  f <- Multimap.new
  sh <- Multimap.new
  m <- Multimap.new
  s <- Map.new
  e <- Map.new
  u <- Map.new
  o <- Map.new
  r <- Map.new
  oth <- Map.new
  pure
    Temp
      { tempReferencesFrom = rf
      , tempSubscriptionsFrom = sf
      , tempForksFrom = f
      , tempSpacesHiddenTo = sh
      , tempMemberOf = m
      , tempSpaceOf = s
      , tempEntityOf = e
      , tempTabulatedPermissionsUniverse = u
      , tempTabulatedPermissionsOrganization = o
      , tempTabulatedPermissionsRecruiter = r
      , tempTabulatedPermissionsOther = oth
      }

data PermissionsPerGroup = PermissionsPerGroup
  { permissionsPerGroupSpace :: Map SpaceId SinglePermission
  , permissionsPerGroupEntity :: Map SpaceId CollectionPermission
  , permissionsPerGroupGroup :: Map GroupId SinglePermission
  , permissionsPerGroupMember :: Map GroupId CollectionPermission
  }
  deriving (Generic)
makeLensesFor
  [ ("permissionsPerGroupSpace", "spacePermission")
  , ("permissionsPerGroupEntity", "entityPermission")
  , ("permissionsPerGroupGroup", "groupPermission")
  , ("permissionsPerGroupMember", "memberPermission")
  ]
  ''PermissionsPerGroup

newPermissionsPerGroup
  :: STM PermissionsPerGroup
newPermissionsPerGroup = PermissionsPerGroup <$> Map.new <*> Map.new <*> Map.new <*> Map.new

data Store = Store
  { storeGroupsPrev :: Map GroupId GroupId
  , storeGroupsNext :: Multimap GroupId GroupId
  , storeGroupMembers :: Multimap GroupId ActorId
  , storeGroupRoots :: Set GroupId -- ^ Only groups without parents
  , storeGroupEdges :: Set (GroupId, GroupId) -- ^ All group edges
  , storeGroupOuts :: Set GroupId -- ^ Only groups with parents and have no children
  , storeGroups :: Set GroupId -- ^ All groups
  , storeActors :: Set ActorId
  , storeSpaces :: Set SpaceId
  , storeSpaceEntities :: Multimap SpaceId EntityId
  , storeEntities :: Map EntityId (NonEmpty VersionId)
  , storeForks :: Map EntityId VersionId
  , storeVersions :: Set VersionId
  , storeVersionReferences :: Multimap VersionId VersionId
  , storeVersionSubscriptions :: Multimap VersionId EntityId
  , storePermissionsPerGroupUniverse
      :: Map GroupId CollectionPermissionWithExemption
  , storePermissionsPerGroupOrganization
      :: Map GroupId CollectionPermissionWithExemption
  , storePermissionsPerGroupRecruiter :: Map GroupId CollectionPermission
  , storePermissionsPerGroupOther :: Map GroupId PermissionsPerGroup
  }
  deriving (Generic)
makeLensesFor
  [ ("storeGroupsPrev", "toGroupsPrev")
  , ("storeGroupsNext", "toGroupsNext")
  , ("storeGroupMembers", "toMembers")
  , ("storeGroupRoots", "toRoots")
  , ("storeGroupEdges", "toEdges")
  , ("storeGroupOuts", "toOuts")
  , ("storeGroups", "toGroups")
  , ("storeActors", "toActors")
  , ("storeSpaces", "toSpaces")
  , ("storeSpaceEntities", "toSpaceEntities")
  , ("storeEntities", "toEntities")
  , ("storeForks", "toForks")
  , ("storeVersions", "toVersions")
  , ("storeVersionReferences", "toReferences")
  , ("storeVersionSubscriptions", "toSubscriptions")
  , ("storePermissionsPerGroupUniverse", "toPermUniverse")
  , ("storePermissionsPerGroupOrganization", "toPermOrganization")
  , ("storePermissionsPerGroupRecruiter", "toPermRecruiter")
  , ("storePermissionsPerGroupOther", "toPermOther")
  ]
  ''Store

newStore :: STM Store
newStore = do
  prev <- Map.new
  next <- Multimap.new
  members <- Multimap.new
  roots <- Set.new
  edges <- Set.new
  outs <- Set.new
  gs <- Set.new
  as <- Set.new
  ss <- Set.new
  se <- Multimap.new
  es <- Map.new
  fs <- Map.new
  vs <- Set.new
  refs <- Multimap.new
  subs <- Multimap.new
  us <- Map.new
  os <- Map.new
  rs <- Map.new
  oths <- Map.new
  pure
    Store
      { storeGroupsPrev = prev
      , storeGroupsNext = next
      , storeGroupMembers = members
      , storeGroupRoots = roots
      , storeGroupEdges = edges
      , storeGroupOuts = outs
      , storeGroups = gs
      , storeActors = as
      , storeSpaces = ss
      , storeSpaceEntities = se
      , storeEntities = es
      , storeForks = fs
      , storeVersions = vs
      , storeVersionReferences = refs
      , storeVersionSubscriptions = subs
      , storePermissionsPerGroupUniverse = us
      , storePermissionsPerGroupOrganization = os
      , storePermissionsPerGroupRecruiter = rs
      , storePermissionsPerGroupOther = oths
      }

data Shared = Shared
  { sharedStore :: Store
  , sharedTemp :: Temp
  }
  deriving (Generic)
makeLensesFor
  [ ("sharedStore", "store")
  , ("sharedTemp", "temp")
  ]
  ''Shared

newShared :: STM Shared
newShared = Shared <$> newStore <*> newTemp
