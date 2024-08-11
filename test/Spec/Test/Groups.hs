{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , RecordWildCards
  , FlexibleInstances
  , NamedFieldPuns
  #-}

module Spec.Test.Groups where

import Spec.Sample.Tree
  ( SampleGroupTree (..)
  , loadSampleTreeNoTab
  , loadSampleTree
  )

import Lib.Types.Id (GroupId)
import Lib.Types.Store
  ( Store
  , toGroups
  , toTabulatedPermissions
  , hasLessOrEqualPermissionsTo
  )
import Lib.Types.Store.Groups
  ( Groups
  , nodes
  , edges
  , roots
  , next
  , prev
  , hasCycle
  , emptyGroup
  , emptyGroups
  )
import Lib.Actions.Unsafe (unsafeUnlinkGroups)
import Lib.Actions.Tabulation (resetTabulation)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import Topograph (pairs)
import Control.Monad (void)
import Control.Monad.State (State, execState, modify)
import Control.Lens ((.~), (^.), at, ix)
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck
  ( Property
  , property
  , forAll
  , elements
  )


groupsTests :: Spec
groupsTests = do
  describe "only Groups structure" $
    it "cycles are detected" $
      property $ \(xs :: [GroupId]) ->
        if length xs <= 1 then True `shouldBe` True
        else hasCycle (loadCycle xs) `shouldBe` Just (xs !! 0 : reverse xs)
  describe "logical mechanism" $ do
    it "tabulating while linking is the same as tabulating after linking" $
      property $ \(xs :: SampleGroupTree ()) ->
        loadSampleTree xs `shouldBe` execState resetTabulation (loadSampleTreeNoTab xs)
    it "resetting tabulation is idempotent" $
      property $ \(xs :: SampleGroupTree ()) ->
        execState (resetTabulation >> resetTabulation) (loadSampleTreeNoTab xs)
          `shouldBe` execState resetTabulation (loadSampleTreeNoTab xs)
    it "unlinking causes disjoint trees" $
      property $ \(xs :: SampleGroupTree ()) ->
        let store = loadSampleTree xs
            createdEdges = store ^. toGroups . edges
        in if null createdEdges
        then property True
        else forAll (elements (HS.toList createdEdges)) $ \(from, to) ->
          let newStore = execState (unsafeUnlinkGroups from to) store
              newGroups = newStore ^. toGroups
              rootsWithoutTo = HS.delete to (newGroups ^. roots)
              descendants :: GroupId -> HashSet GroupId
              descendants gId =
                let children = fromJust (HM.lookup gId (newGroups ^. nodes)) ^. next
                in  HS.insert gId (HS.unions (map descendants (HS.toList children)))
          in  (newStore, from, to) `shouldSatisfy` (\_ ->
                to `HS.member` (newStore ^. toGroups . roots)
                && all
                  (\descendantOfTo ->
                     not . HS.member descendantOfTo . HS.unions . map descendants $ HS.toList rootsWithoutTo
                  )
                  (HS.toList (descendants to))
              )
        
    describe "total vs. incremental tabulation" $ do
      let testsPerStoreBuild buildStore = do
            it "all descendants are supersets of roots - only universal permission" $
              property $ \(xs :: SampleGroupTree ()) ->
                testPermissionInheritance (current xs) (children xs) (buildStore xs)
      describe "total" . testsPerStoreBuild $ execState resetTabulation . loadSampleTreeNoTab
      describe "incremental" $ testsPerStoreBuild loadSampleTree

testPermissionInheritance :: GroupId -> [SampleGroupTree a] -> Store -> Property
testPermissionInheritance root cs store =
  let tabs = store ^. toTabulatedPermissions
      descendants =
        let go (SampleGroupTree x _ _ _ _ xs) = do
              modify (x:)
              traverse_ go xs
        in  execState (traverse_ go cs) []
  in  if null descendants
      then property True
      else forAll (elements descendants) $ \descendant ->
            case (HM.lookup root tabs, HM.lookup descendant tabs) of
              (Just r, Just d) ->
                (store, r, d) `shouldSatisfy` (\_ -> r `hasLessOrEqualPermissionsTo` d)
              tabs -> error $ "Tab wasn't found " <> show tabs

loadCycle :: [GroupId] -> Groups
loadCycle gs = execState go emptyGroups
  where
    addGroup :: GroupId -> State Groups ()
    addGroup gId = modify $ nodes . at gId .~ Just emptyGroup

    addSingleLink :: (GroupId, GroupId) -> State Groups ()
    addSingleLink (from, to) = do
      modify $ nodes . ix from . next . at to .~ Just ()
      modify $ nodes . ix to . prev .~ Just from

    go :: State Groups ()
    go = do
      void $ traverse addGroup gs
      if length gs <= 1
      then pure ()
      else do
        modify $ roots . at (gs !! 0) .~ Just ()
        void $ traverse addSingleLink (pairs gs)
        addSingleLink (gs !! (length gs - 1), gs !! 0)
