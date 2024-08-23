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

module Spec.Sync.Test.Groups where

import Spec.Sync.Sample.Tree (
  SampleGroupTree (..),
  loadSampleTree,
  loadSampleTreeNoTab,
 )

import Lib.Sync.Actions.Tabulation (resetTabulation)
import Lib.Sync.Actions.Unsafe.Update.Group (unsafeUnlinkGroups)
import Lib.Sync.Types.Id (GroupId)
import Lib.Sync.Types.Store (
  Shared,
  store,
  temp,
  toGroups,
  toTabulatedGroups,
 )
import Lib.Sync.Types.Store.Groups (
  Groups,
  edges,
  emptyGroup,
  emptyGroups,
  hasCycle,
  next,
  nodes,
  prev,
  roots,
 )
import Lib.Sync.Types.Store.Tabulation.Group (hasLessOrEqualPermissionsTo)

import Control.Lens (at, ix, (?~), (^.))
import Control.Monad.State (State, execState, modify)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)
import Test.QuickCheck (
  Property,
  elements,
  forAll,
  property,
 )
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)
import Topograph (pairs)

groupsTests :: Spec
groupsTests = do
  describe "only Groups structure" $
    it "cycles are detected" $
      property $ \(xs :: [GroupId]) ->
        if length xs <= 1
          then True `shouldBe` True
          else hasCycle (loadCycle xs) `shouldBe` Just (head xs : reverse xs)
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
        let s = loadSampleTree xs
            createdEdges = s ^. store . toGroups . edges
         in if null createdEdges
              then property True
              else forAll (elements (HS.toList createdEdges)) $ \(from, to) ->
                let newS = execState (unsafeUnlinkGroups from to) s
                    newGroups = newS ^. store . toGroups
                    rootsWithoutTo = HS.delete to (newGroups ^. roots)
                    descendants :: GroupId -> HashSet GroupId
                    descendants gId =
                      let children = fromJust (HM.lookup gId (newGroups ^. nodes)) ^. next
                       in HS.insert gId (HS.unions (map descendants (HS.toList children)))
                 in (newS, from, to)
                      `shouldSatisfy` ( \_ ->
                                          to `HS.member` (newS ^. store . toGroups . roots)
                                            && all
                                              ( \descendantOfTo ->
                                                  not . HS.member descendantOfTo . HS.unions . map descendants $
                                                    HS.toList rootsWithoutTo
                                              )
                                              (HS.toList (descendants to))
                                      )

    describe "total vs. incremental tabulation" $ do
      let testsPerStoreBuild buildStore = do
            it "all descendants are supersets of roots - only universal permission" $
              property $ \(xs :: SampleGroupTree ()) ->
                testPermissionInheritance (current xs) (children xs) (buildStore xs)
      describe "total" . testsPerStoreBuild $
        execState resetTabulation . loadSampleTreeNoTab
      describe "incremental" $ testsPerStoreBuild loadSampleTree

testPermissionInheritance
  :: GroupId -> [SampleGroupTree a] -> Shared -> Property
testPermissionInheritance root cs s =
  let tabs = s ^. temp . toTabulatedGroups
      descendants =
        let go (SampleGroupTree x _ _ _ _ xs) = do
              modify (x :)
              traverse_ go xs
         in execState (traverse_ go cs) []
   in if null descendants
        then property True
        else forAll (elements descendants) $ \descendant ->
          case (HM.lookup root tabs, HM.lookup descendant tabs) of
            (Just r, Just d) ->
              (s, r, d) `shouldSatisfy` (\_ -> r `hasLessOrEqualPermissionsTo` d)
            tabs -> error $ "Tab wasn't found " <> show tabs

loadCycle :: [GroupId] -> Groups
loadCycle gs = execState go emptyGroups
 where
  addGroup :: GroupId -> State Groups ()
  addGroup gId = modify $ nodes . at gId ?~ emptyGroup

  addSingleLink :: (GroupId, GroupId) -> State Groups ()
  addSingleLink (from, to) = do
    modify $ nodes . ix from . next . at to ?~ ()
    modify $ nodes . ix to . prev ?~ from

  go :: State Groups ()
  go = do
    traverse_ addGroup gs
    if length gs <= 1
      then pure ()
      else do
        modify $ roots . at (head gs) ?~ ()
        traverse_ addSingleLink (pairs gs)
        addSingleLink (gs !! (length gs - 1), head gs)
