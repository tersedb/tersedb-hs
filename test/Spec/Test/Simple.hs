{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Test.Simple where

import Lib.Types.Id (GroupId)
import Lib.Types.Permission (
    CollectionPermission (..),
    CollectionPermissionWithExemption (..),
    SinglePermission,
 )
import Lib.Types.Store.Tabulation.Group (
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
                \(x :: CollectionPermissionWithExemption) -> x `shouldSatisfy` (>= minBound)
        it "has an upper bound" $
            property $
                \(x :: CollectionPermissionWithExemption) -> x `shouldSatisfy` (<= maxBound)
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
                (x, x <> y) `shouldSatisfy` (uncurry hasLessOrEqualPermissionsTo)
