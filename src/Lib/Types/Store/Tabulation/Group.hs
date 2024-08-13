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

module Lib.Types.Store.Tabulation.Group where

import Lib.Types.Id (GroupId, SpaceId)
import Lib.Types.Permission
  ( CollectionPermission
  , CollectionPermissionWithExemption
  )

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Lens.TH (makeLensesFor)
import Test.QuickCheck (Arbitrary (arbitrary))


-- TODO tabulate groups that actors belong to, omitting redundant groups already inherited

data TabulatedPermissionsForGroup = TabulatedPermissionsForGroup
  { -- | Collection of spaces
    tabulatedPermissionsForGroupUniverse :: CollectionPermissionWithExemption
  , -- | Collection of groups
    tabulatedPermissionsForGroupOrganization :: CollectionPermissionWithExemption
  , -- | Collection of actors
    tabulatedPermissionsForGroupRecruiter :: CollectionPermission
  , -- | Single spaces, after being applied to universe.
    -- Will not be applied to universe via 'escalate' when a record doesn't exist
    tabulatedPermissionsForGroupSpaces :: HashMap SpaceId CollectionPermission
  , -- | Collection of entities
    tabulatedPermissionsForGroupEntities :: HashMap SpaceId CollectionPermission
  , -- | Single groups, after being applied to organization.
    -- Will not be applied organization via 'escalate' when a record doesn't exist.
    tabulatedPermissionsForGroupGroups :: HashMap GroupId CollectionPermission
  , -- | Collection of memberships
    tabulatedPermissionsForGroupMembers :: HashMap GroupId CollectionPermission
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
