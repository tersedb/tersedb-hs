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
import Lib.Types.Store.Groups
  ( Groups
  , nodes
  , edges
  , roots
  , outs
  , next
  , prev
  , hasCycle
  , emptyGroup
  , emptyGroups
  )
import Lib.Types.Group
  ( storeGroup
  , linkGroups
  , resetTabulation
  , initTabulatedPermissionsForGroup
  )

import Test.Syd (sydTest, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), property, getSize, resize, listOf)
import qualified Data.Aeson as Aeson
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Text.Read (readMaybe)
import Control.Monad (void)
import Control.Monad.State (State, execState, modify, put, get)
import Control.Lens ((%~), (.~), (&), (^.))
import Topograph (pairs)



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
  describe "Groups" $ do
    it "cycles are detected" $
      property $ \(xs :: [GroupId]) ->
        if length xs <= 1 then True `shouldBe` True
        else hasCycle (loadCycle xs) `shouldBe` Just (xs !! 0 : reverse xs)
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
  shrink (SampleGroupTree current children) =
    [ SampleGroupTree current []
    ] ++ [ SampleGroupTree current children' | children' <- shrink children ]

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
            & outs %~ (HS.insert to . HS.delete from)
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

loadCycle :: [GroupId] -> Groups
loadCycle gs = execState go emptyGroups
  where
    addGroup :: GroupId -> State Groups ()
    addGroup gId = modify (nodes %~ HM.insert gId emptyGroup)

    addSingleLink :: (GroupId, GroupId) -> State Groups ()
    addSingleLink (from, to) =
      modify $
        nodes %~ (HM.adjust (next .~ HS.singleton to) from . HM.adjust (prev .~ Just from) to)

    go :: State Groups ()
    go = do
      void $ traverse addGroup gs
      if length gs <= 1
      then pure ()
      else do
        modify (roots .~ HS.singleton (gs !! 0))
        void $ traverse addSingleLink (pairs gs)
        addSingleLink (gs !! (length gs - 1), gs !! 0)
