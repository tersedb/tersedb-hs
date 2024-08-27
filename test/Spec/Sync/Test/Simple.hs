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

import Lib.Sync.Types.Store.Tabulation.Group (
  TabulatedPermissionsForGroup,
  hasLessOrEqualPermissionsTo,
 )
import Test.QuickCheck (property)
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)

simpleSyncTests :: Spec
simpleSyncTests = do
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
