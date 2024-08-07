{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  #-}

import Lib.Types.Id (GroupId)
import Lib.Types.Store
  ( Store
  , TabulatedPermissionsForGroup
  , emptyStore
  , toGroups
  , toTabulatedPermissions
  )
import Lib.Types.Store.Groups (edges, roots, outs)
import Lib.Types.Group
  ( storeGroup
  , linkGroups
  , resetTabulation
  , initTabulatedPermissionsForGroup
  )

import Test.Syd (sydTest, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (arbitrary), property, getSize, resize, listOf)
import qualified Data.Aeson as Aeson
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Text.Read (readMaybe)
import Control.Monad (void)
import Control.Monad.State (State, execState, modify, put, get)
import Control.Lens ((%~), (.~), (&), (^.))

main :: IO ()
main = sydTest $ do
  describe "Id" $ do
    describe "parsing" $ do
      it "show / read should be isomorphic" $
        property $ \(id :: GroupId) ->
          readMaybe (show id) `shouldBe` Just id
      it "aeson should be isomorphic" $
        property $ \(id :: GroupId) ->
          Aeson.decode (Aeson.encode id) `shouldBe` Just id
-- TODO tabulating should be idempotent
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
  describe "Store" $ do
    it "tabulating while linking is the same as tabulating after linking" $
      property $ \(xs :: SampleGroupTree) ->
        loadSample xs `shouldBe` execState resetTabulation (loadSampleNoTab xs)

data SampleGroupTree = SampleGroupTree GroupId [SampleGroupTree]
  deriving (Eq, Show, Read)

instance Arbitrary SampleGroupTree where
  arbitrary = resize 10 go
    where
      go = do
        current <- arbitrary
        s <- getSize
        children <- resize (s `div` 2) (listOf go)
        pure (SampleGroupTree current children)

loadSample :: SampleGroupTree -> Store
loadSample xs = execState (go xs) emptyStore
  where
    go :: SampleGroupTree -> State Store ()
    go (SampleGroupTree current children) = do
      storeGroup current
      currentTab <- initTabulatedPermissionsForGroup current
      -- ensures that singleton maps still have loaded tabs
      modify (toTabulatedPermissions %~ HM.insert current currentTab)
      let children' = map (\(SampleGroupTree x _) -> x) children
      void $ traverse storeGroup children'
      let linkGroups' child = do
            mE <- linkGroups current child
            case mE of
              Left e -> error $ "Error detected " <> show e
              Right _ -> pure ()
      void $ traverse linkGroups' children'
      void $ traverse go children

loadSampleNoTab :: SampleGroupTree -> Store
loadSampleNoTab xs = execState (go xs) emptyStore
  where
    addLink :: GroupId -> GroupId -> State Store ()
    addLink from to = do
      s <- get
      let groups = s ^. toGroups
          newGroups = groups
            & edges %~ HS.insert (from, to)
            & outs %~ HS.insert to
            & roots %~ HS.delete to
      put $ s & toGroups .~ newGroups

    go :: SampleGroupTree -> State Store ()
    go (SampleGroupTree current children) = do
      storeGroup current
      modify (toGroups . roots %~ HS.insert current)
      let children' = map (\(SampleGroupTree x _) -> x) children
      void $ traverse storeGroup children'
      void $ traverse (\child -> addLink current child) children'
      void $ traverse go children
