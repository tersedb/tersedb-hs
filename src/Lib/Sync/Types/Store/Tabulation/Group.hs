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

module Lib.Sync.Types.Store.Tabulation.Group where

import Control.Lens.TH (makeLensesFor)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Lib.Types.Id (GroupId, SpaceId)
import Lib.Types.Permission (
  CollectionPermission,
  CollectionPermissionWithExemption,
  hasMinimumPermission,
 )
import Test.QuickCheck (Arbitrary (arbitrary))

-- TODO tabulate groups that actors belong to, omitting redundant groups already inherited
-- FIXME invariants:
--  - Spaces - values present should not equal universe value if not exempt
--  - Groups - values present should not equal organization value if not exempt
--  - Spaces - values present should not be less than or equal to universe if exempt
--  - Groups - values present should not be less than or equal to universe if exempt
--  - Entities & Members - always have to be declared explicitly

data TabulatedPermissionsForGroup = TabulatedPermissionsForGroup
  { tabulatedPermissionsForGroupUniverse :: CollectionPermissionWithExemption
  -- ^ Collection of spaces
  , tabulatedPermissionsForGroupOrganization :: CollectionPermissionWithExemption
  -- ^ Collection of groups
  , tabulatedPermissionsForGroupRecruiter :: CollectionPermission
  -- ^ Collection of actors
  , tabulatedPermissionsForGroupSpaces :: HashMap SpaceId CollectionPermission
  -- ^ Single spaces, after being applied to universe.
  -- Will not be applied to universe via 'escalate' when a record doesn't exist
  , tabulatedPermissionsForGroupEntities :: HashMap SpaceId CollectionPermission
  -- ^ Collection of entities
  , tabulatedPermissionsForGroupGroups :: HashMap GroupId CollectionPermission
  -- ^ Single groups, after being applied to organization.
  -- Will not be applied organization via 'escalate' when a record doesn't exist.
  , tabulatedPermissionsForGroupMembers :: HashMap GroupId CollectionPermission
  -- ^ Collection of memberships
  }
  deriving (Show, Read)
makeLensesFor
  [ ("tabulatedPermissionsForGroupUniverse", "forUniverse")
  , ("tabulatedPermissionsForGroupOrganization", "forOrganization")
  , ("tabulatedPermissionsForGroupRecruiter", "forRecruiter")
  , ("tabulatedPermissionsForGroupSpaces", "forSpaces")
  , ("tabulatedPermissionsForGroupEntities", "forEntities")
  , ("tabulatedPermissionsForGroupGroups", "forGroups")
  , ("tabulatedPermissionsForGroupMembers", "forMembers")
  ]
  ''TabulatedPermissionsForGroup

instance Semigroup TabulatedPermissionsForGroup where
  x <> y =
    TabulatedPermissionsForGroup
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
          HM.unionWith
            (<>)
            (tabulatedPermissionsForGroupSpaces x)
            (tabulatedPermissionsForGroupSpaces y)
      , tabulatedPermissionsForGroupEntities =
          HM.unionWith
            (<>)
            (tabulatedPermissionsForGroupEntities x)
            (tabulatedPermissionsForGroupEntities y)
      , tabulatedPermissionsForGroupGroups =
          HM.unionWith
            (<>)
            (tabulatedPermissionsForGroupGroups x)
            (tabulatedPermissionsForGroupGroups y)
      , tabulatedPermissionsForGroupMembers =
          HM.unionWith
            (<>)
            (tabulatedPermissionsForGroupMembers x)
            (tabulatedPermissionsForGroupMembers y)
      }
instance Monoid TabulatedPermissionsForGroup where
  mempty =
    TabulatedPermissionsForGroup
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
    yu `hasMinimumPermission` xu
      && yo `hasMinimumPermission` xo
      && xr <= yr
      && HM.isSubmapOfBy (<=) xs ys
      && HM.isSubmapOfBy (<=) xe ye
      && HM.isSubmapOfBy (<=) xg yg
      && HM.isSubmapOfBy (<=) xm ym
