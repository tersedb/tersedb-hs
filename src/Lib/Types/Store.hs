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



data Store = Store
  { storeGroups :: Groups
  , storeActors :: HashMap ActorId (HashSet GroupId)
  , storeSpaces :: HashMap SpaceId Space
  , storeEntities :: HashMap EntityId Entity
  , storeVersions :: HashMap VersionId Version
  , storeReferencesFrom :: HashMap VersionId (HashSet VersionId)
  , storeSubscriptionsFrom :: HashMap EntityId (HashSet VersionId)
  , storeSpacePermissions :: HashMap GroupId (HashMap SpaceId SinglePermission)
  , storeEntityPermissions :: HashMap GroupId (HashMap SpaceId CollectionPermission)
  , storeGroupPermissions :: HashMap GroupId (HashMap GroupId SinglePermission)
  , storeMemberPermissions :: HashMap GroupId (HashMap GroupId CollectionPermission)
  , storeTabulatedPermissions :: HashMap GroupId TabulatedPermissionsForGroup 
  } deriving (Eq, Show, Read)
makeLensesFor
  [ ("storeGroups", "toGroups")
  , ("storeActors", "toActors")
  , ("storeSpaces", "toSpaces")
  , ("storeEntities", "toEntities")
  , ("storeVersions", "toVersions")
  , ("storeReferencesFrom", "toReferencesFrom")
  , ("storeSubscriptionsFrom", "toSubscriptionsFrom")
  , ("storeSpacePermissions", "toSpacePermissions")
  , ("storeEntityPermissions", "toEntityPermissions")
  , ("storeGroupPermissions", "toGroupPermissions")
  , ("storeMemberPermissions", "toMemberPermissions")
  , ("storeTabulatedPermissions", "toTabulatedPermissions")
  ] ''Store

