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

import Lib.Types.Store.Groups (Groups, emptyGroups)
import Lib.Types.Store.Space (Space)
import Lib.Types.Store.Entity (Entity)
import Lib.Types.Store.Version (Version)
import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId)
import Lib.Types.Permission (Permission)

import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Lens.TH (makeLensesFor)
import Test.QuickCheck (Arbitrary (arbitrary))


data TabulatedPermissionsForGroup = TabulatedPermissionsForGroup
  { tabulatedPermissionsForGroupUniverse :: Permission
  , tabulatedPermissionsForGroupOrganization :: Permission
  , tabulatedPermissionsForGroupSpaces :: HashMap SpaceId Permission
  , tabulatedPermissionsForGroupEntities :: HashMap SpaceId Permission
  , tabulatedPermissionsForGroupGroups :: HashMap GroupId Permission
  } deriving (Eq, Show, Read)
makeLensesFor
  [ ("tabulatedPermissionsForGroupUniverse", "forGroupUniverse")
  , ("tabulatedPermissionsForGroupOrganization", "forGroupOrganization")
  , ("tabulatedPermissionsForGroupSpaces", "forGroupSpaces")
  , ("tabulatedPermissionsForGroupEntities", "forGroupEntities")
  , ("tabulatedPermissionsForGroupGroups", "forGroupGroups")
  ] ''TabulatedPermissionsForGroup

instance Semigroup TabulatedPermissionsForGroup where
  x <> y = TabulatedPermissionsForGroup
    { tabulatedPermissionsForGroupUniverse =
        tabulatedPermissionsForGroupUniverse x
          <> tabulatedPermissionsForGroupUniverse y
    , tabulatedPermissionsForGroupOrganization =
        tabulatedPermissionsForGroupOrganization x
          <> tabulatedPermissionsForGroupOrganization y
    , tabulatedPermissionsForGroupSpaces =
        HM.unionWith (<>)
          (tabulatedPermissionsForGroupSpaces x)
          (tabulatedPermissionsForGroupSpaces y)
    , tabulatedPermissionsForGroupEntities =
        HM.unionWith (<>)
          (tabulatedPermissionsForGroupEntities x)
          (tabulatedPermissionsForGroupEntities y)
    , tabulatedPermissionsForGroupGroups =
        HM.unionWith (<>)
          (tabulatedPermissionsForGroupGroups x)
          (tabulatedPermissionsForGroupGroups y)
    }
instance Monoid TabulatedPermissionsForGroup where
  mempty = TabulatedPermissionsForGroup
    { tabulatedPermissionsForGroupUniverse = mempty
    , tabulatedPermissionsForGroupOrganization = mempty
    , tabulatedPermissionsForGroupSpaces = mempty
    , tabulatedPermissionsForGroupEntities = mempty
    , tabulatedPermissionsForGroupGroups = mempty
    }
instance Arbitrary TabulatedPermissionsForGroup where
  arbitrary =
    TabulatedPermissionsForGroup
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

hasLessOrEqualPermissionsTo
  :: TabulatedPermissionsForGroup
  -> TabulatedPermissionsForGroup
  -> Bool
hasLessOrEqualPermissionsTo
  (TabulatedPermissionsForGroup xu xo xs xe xg)
  (TabulatedPermissionsForGroup yu yo ys ye yg) =
    xu <= yu
    && xo <= yo
    && HM.isSubmapOfBy (<=) xs ys
    && HM.isSubmapOfBy (<=) xe ye
    && HM.isSubmapOfBy (<=) xg yg

data Store = Store
  { storeGroups :: Groups
  , storeSpaces :: HashMap SpaceId Space
  , storeEntities :: HashMap EntityId Entity
  , storeVersions :: HashMap VersionId Version
  , storeSpacePermissions :: HashMap GroupId (HashMap SpaceId Permission)
  , storeEntityPermissions :: HashMap GroupId (HashMap SpaceId Permission)
  , storeGroupPermissions :: HashMap GroupId (HashMap GroupId Permission)
  , storeTabulatedPermissions :: HashMap GroupId TabulatedPermissionsForGroup 
  } deriving (Eq, Show, Read)
makeLensesFor
  [ ("storeGroups", "toGroups")
  , ("storeSpaces", "toSpaces")
  , ("storeEntities", "toEntities")
  , ("storeVersions", "toVersions")
  , ("storeSpacePermissions", "toSpacePermissions")
  , ("storeEntityPermissions", "toEntityPermissions")
  , ("storeGroupPermissions", "toGroupPermissions")
  , ("storeTabulatedPermissions", "toTabulatedPermissions")
  ] ''Store

emptyStore :: Store
emptyStore = Store
  { storeGroups = emptyGroups
  , storeSpaces = mempty
  , storeEntities = mempty
  , storeVersions = mempty
  , storeSpacePermissions = mempty
  , storeEntityPermissions = mempty
  , storeGroupPermissions = mempty
  , storeTabulatedPermissions = mempty
  }
