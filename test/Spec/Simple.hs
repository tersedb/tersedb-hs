module Spec.Simple where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Data (Proxy (..))
import Lib.Api.Action (Action, MutableAction)
import Lib.Api.Response (Authorize, Response)
import Lib.Types.Id (AnyId, GroupId, IdWithPfx)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption (..),
  SinglePermission,
  hasMinimumPermission,
 )
import Test.QuickCheck (Arbitrary, property)
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Read (readEither)

serialization
  :: forall a
   . (Show a, Read a, ToJSON a, FromJSON a, Arbitrary a, Eq a)
  => Proxy a
  -> Spec
serialization Proxy = do
  it "show / read should be isomorphic" $
    property $ \(id :: a) ->
      readEither (show id) `shouldBe` Right id
  it "aeson should be isomorphic" $
    property $ \(id :: a) ->
      Aeson.decode (Aeson.encode id) `shouldBe` Just id

simpleTests :: Spec
simpleTests = do
  describe "Id" $ do
    describe "parsing" $ do
      describe "IdWithPfx" . serialization $ Proxy @(IdWithPfx "g_")
      describe "AnyId" . serialization $ Proxy @AnyId
      describe "Action" . serialization $ Proxy @Action
      describe "MutableAction" . serialization $ Proxy @MutableAction
      describe "Authorize Response" . serialization $ Proxy @(Authorize Response)
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
