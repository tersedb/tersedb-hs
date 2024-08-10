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

import Lib.Types.Store.Groups (Groups)
import Lib.Types.Store.Space (Space)
import Lib.Types.Store.Entity (Entity)
import Lib.Types.Store.Version (Version)
import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission
  , CollectionPermissionWithExemption
  , SinglePermission
  )

import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Lens.TH (makeLensesFor)
import Test.QuickCheck (Arbitrary (arbitrary))


-- TODO tabulate groups that actors belong to, omitting redundant groups already inherited

data TabulatedPermissionsForGroup = TabulatedPermissionsForGroup
  { tabulatedPermissionsForGroupUniverse :: CollectionPermissionWithExemption -- Collection of spaces
  , tabulatedPermissionsForGroupOrganization :: CollectionPermissionWithExemption -- Collection of groups
  , tabulatedPermissionsForGroupRecruiter :: CollectionPermission -- Collection of actors
  , tabulatedPermissionsForGroupSpaces :: HashMap SpaceId CollectionPermission -- Single spaces, after being applied to universe. Will not reference universe when it doesn't exist
  , tabulatedPermissionsForGroupEntities :: HashMap SpaceId CollectionPermission -- Collection of entities
  , tabulatedPermissionsForGroupGroups :: HashMap GroupId CollectionPermission -- Single groups, after being applied to organization. Will not reference organization when it doesn't exist.
  , tabulatedPermissionsForGroupMembers :: HashMap GroupId CollectionPermission -- Collection of memberships
  } deriving (Show, Read)
makeLensesFor
  [ ("tabulatedPermissionsForGroupUniverse", "forUniverse")
  , ("tabulatedPermissionsForGroupOrganization", "forOrganization")
  , ("tabulatedPermissionsForGroupRecruiter", "forRecruiter")
  , ("tabulatedPermissionsForGroupSpaces", "forSpaces")
  , ("tabulatedPermissionsForGroupEntities", "forEntities")
  , ("tabulatedPermissionsForGroupGroups", "forGroups")
  , ("tabulatedPermissionsForGroupMembers", "forMembers")
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
    , tabulatedPermissionsForGroupMembers =
        HM.unionWith (<>)
          (tabulatedPermissionsForGroupMembers x)
          (tabulatedPermissionsForGroupMembers y)
    }
instance Monoid TabulatedPermissionsForGroup where
  mempty = TabulatedPermissionsForGroup
    { tabulatedPermissionsForGroupUniverse = mempty
    , tabulatedPermissionsForGroupOrganization = mempty
    , tabulatedPermissionsForGroupRecruiter = mempty
    , tabulatedPermissionsForGroupSpaces = mempty
    , tabulatedPermissionsForGroupEntities = mempty
    , tabulatedPermissionsForGroupGroups = mempty
    , tabulatedPermissionsForGroupMembers = mempty
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
      <*> arbitrary
instance Eq TabulatedPermissionsForGroup where
  x == y =
    x `hasLessOrEqualPermissionsTo` y && y `hasLessOrEqualPermissionsTo` x

hasLessOrEqualPermissionsTo
  :: TabulatedPermissionsForGroup
  -> TabulatedPermissionsForGroup
  -> Bool
hasLessOrEqualPermissionsTo
  (TabulatedPermissionsForGroup xu xo xr xs xe xg xm)
  (TabulatedPermissionsForGroup yu yo yr ys ye yg ym) =
    xu <= yu
    && xo <= yo
    && xr <= yr
    && HM.isSubmapOfBy (<=) xs ys
    && HM.isSubmapOfBy (<=) xe ye
    && HM.isSubmapOfBy (<=) xg yg
    && HM.isSubmapOfBy (<=) xm ym

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
  , ("storeMemberPermissions", "toMemberPermissions")
  , ("storeTabulatedPermissions", "toTabulatedPermissions")
  ] ''Store

