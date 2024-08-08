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
import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Permission (Permission)

import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Lens.TH (makeLensesFor)
import Test.QuickCheck (Arbitrary (arbitrary))


data TabulatedPermissionsForGroup = TabulatedPermissionsForGroup
  { tabulatedPermissionsForGroupUniverse :: Permission -- BRCUD for spaces
  , tabulatedPermissionsForGroupOrganization :: Permission -- BRCUD for groups
  , tabulatedPermissionsForGroupRecruiter :: Permission -- BRCUD for actors
  , tabulatedPermissionsForGroupSpaces :: HashMap SpaceId Permission -- BRCUD for spaces that already exist - C does not apply
  , tabulatedPermissionsForGroupEntities :: HashMap SpaceId Permission -- BRCUD for entities & versions within a space
  , tabulatedPermissionsForGroupGroups :: HashMap GroupId Permission -- BRCUD for memberships of actors for a group - U does not apply
  } deriving (Show, Read)
makeLensesFor
  [ ("tabulatedPermissionsForGroupUniverse", "forGroupUniverse")
  , ("tabulatedPermissionsForGroupOrganization", "forGroupOrganization")
  , ("tabulatedPermissionsForGroupRecruiter", "forGroupRecruiter")
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
    , tabulatedPermissionsForGroupRecruiter =
        tabulatedPermissionsForGroupRecruiter x
          <> tabulatedPermissionsForGroupRecruiter y
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
    , tabulatedPermissionsForGroupRecruiter = mempty
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
      <*> arbitrary
instance Eq TabulatedPermissionsForGroup where
  x == y =
    x `hasLessOrEqualPermissionsTo` y && y `hasLessOrEqualPermissionsTo` x

hasLessOrEqualPermissionsTo
  :: TabulatedPermissionsForGroup
  -> TabulatedPermissionsForGroup
  -> Bool
hasLessOrEqualPermissionsTo
  (TabulatedPermissionsForGroup xu xo xr xs xe xg)
  (TabulatedPermissionsForGroup yu yo yr ys ye yg) =
    xu <= yu
    && xo <= yo
    && xr <= yr
    && HM.isSubmapOfBy (<=) xs ys
    && HM.isSubmapOfBy (<=) xe ye
    && HM.isSubmapOfBy (<=) xg yg

data Store = Store
  { storeGroups :: Groups
  , storeActors :: HashMap ActorId (HashSet GroupId)
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
  , ("storeActors", "toActors")
  , ("storeSpaces", "toSpaces")
  , ("storeEntities", "toEntities")
  , ("storeVersions", "toVersions")
  , ("storeSpacePermissions", "toSpacePermissions")
  , ("storeEntityPermissions", "toEntityPermissions")
  , ("storeGroupPermissions", "toGroupPermissions")
  , ("storeTabulatedPermissions", "toTabulatedPermissions")
  ] ''Store

