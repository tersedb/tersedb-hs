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

module Spec.Sync.Test.Simple where

import Lib.Sync.Types.Id (GroupId)
import Lib.Sync.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption (..),
  SinglePermission,
  hasMinimumPermission,
 )
import Lib.Sync.Types.Store.Tabulation.Group (
  TabulatedPermissionsForGroup,
  hasLessOrEqualPermissionsTo,
 )

import qualified Data.Aeson as Aeson
import Test.QuickCheck (property)
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Read (readMaybe)

simpleTests :: Spec
simpleTests = do
  describe "Id" $ do
    describe "parsing" $ do
      it "show / read should be isomorphic" $
        property $ \(id :: GroupId) ->
          readMaybe (show id) `shouldBe` Just id
      it "aeson should be isomorphic" $
        property $ \(id :: GroupId) ->
          Aeson.decode (Aeson.encode id) `shouldBe` Just id
  describe "CollectionPermission" $ do
    it "has a lower bound" $
      property $
        \(x :: CollectionPermission) -> x `shouldSatisfy` (>= minBound)
    it "has an upper bound" $
      property $
        \(x :: CollectionPermission) -> x `shouldSatisfy` (<= maxBound)
    it "is a semigroup" $
      property $
        \(x :: CollectionPermission) y z -> (x <> y) <> z `shouldBe` x <> (y <> z)
    it "is a monoid left" $
      property $
        \(x :: CollectionPermission) -> x <> mempty `shouldBe` x
    it "is a monoid right" $
      property $
        \(x :: CollectionPermission) -> mempty <> x `shouldBe` x
    it "is commutative" $
      property $
        \(x :: CollectionPermission) y -> y <> x `shouldBe` x <> y
  describe "CollectionPermissionWithExemption" $ do
    it "has a lower bound" $
      property $
        \(x :: CollectionPermissionWithExemption) ->
          x `shouldSatisfy` (`hasMinimumPermission` minBound)
    it "has an upper bound" $
      property $
        \(x :: CollectionPermissionWithExemption) ->
          x `shouldSatisfy` (maxBound `hasMinimumPermission`)
    it "is a semigroup" $
      property $
        \(x :: CollectionPermissionWithExemption) y z -> (x <> y) <> z `shouldBe` x <> (y <> z)
    it "is a monoid left" $
      property $
        \(x :: CollectionPermissionWithExemption) -> x <> mempty `shouldBe` x
    it "is a monoid right" $
      property $
        \(x :: CollectionPermissionWithExemption) -> mempty <> x `shouldBe` x
    it "is commutative" $
      property $
        \(x :: CollectionPermissionWithExemption) y -> y <> x `shouldBe` x <> y
  describe "SinglePermission" $ do
    it "has a lower bound" $
      property $
        \(x :: SinglePermission) -> x `shouldSatisfy` (>= minBound)
    it "has an upper bound" $
      property $
        \(x :: SinglePermission) -> x `shouldSatisfy` (<= maxBound)
    it "is a semigroup" $
      property $
        \(x :: SinglePermission) y z -> (x <> y) <> z `shouldBe` x <> (y <> z)
    it "is commutative" $
      property $
        \(x :: SinglePermission) y -> y <> x `shouldBe` x <> y
  describe "TabulatedPermissionsForGroup" $ do
    it "is a semigroup" $
      property $ \(x :: TabulatedPermissionsForGroup) y z ->
        (x <> (y <> z)) `shouldBe` ((x <> y) <> z)
    it "is a monoid left" $
      property $ \(x :: TabulatedPermissionsForGroup) ->
        (x <> mempty) `shouldBe` x
    it "is a monoid right" $
      property $ \(x :: TabulatedPermissionsForGroup) ->
        (mempty <> x) `shouldBe` x
    it "commutes" $
      property $ \(x :: TabulatedPermissionsForGroup) y ->
        (x <> y) `shouldBe` (y <> x)
    it "union of two is superset of left" $
      property $ \(x :: TabulatedPermissionsForGroup) y ->
        (x, x <> y) `shouldSatisfy` uncurry hasLessOrEqualPermissionsTo
