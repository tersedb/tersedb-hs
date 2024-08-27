module Spec.Generic.Group where

import Test.Syd (Spec, it, shouldBe, shouldSatisfy)
import Lib.Class (TerseDB (resetTabulation, genSyncShared, commit), TerseDBGen (runTerseDB), unsafeUnlinkGroups)
import Lib.Types.Id (GroupId)
import Test.QuickCheck (Testable(property), forAll, suchThatMap, elements, arbitrary)
import Spec.Sync.Sample.Tree (SampleGroupTree, loadSampleTreeNoTab, loadSampleTree)
import qualified Lib.Sync.Types.Store as Sync
import Data.Data (Proxy (Proxy))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
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
import Control.Lens ((^.))
import Data.Maybe (fromJust)


groupTests :: forall m n. (TerseDB n m, TerseDBGen n) => Proxy m -> Spec
groupTests Proxy = do
  it "tabulating while linking is the same as tabulating after linking" $
    property $ \(xs :: SampleGroupTree ()) -> do
      let whileLoading :: IO Sync.Shared
          whileLoading = runTerseDB (commit go) (loadSampleTree xs)
            where
              go :: m Sync.Shared
              go = do
                genSyncShared
          afterLoading :: IO Sync.Shared
          afterLoading = runTerseDB (commit go) (loadSampleTreeNoTab xs)
            where
              go :: m Sync.Shared
              go = do
                resetTabulation
                genSyncShared
      x <- whileLoading
      y <- afterLoading
      x `shouldBe` y
  it "resetting tabulation is idempotent" $
    property $ \(xs :: SampleGroupTree ()) -> do
      let resetOnce :: IO Sync.Shared
          resetOnce = runTerseDB (commit go) (loadSampleTreeNoTab xs)
            where
              go :: m Sync.Shared
              go = do
                resetTabulation
                genSyncShared
          resetTwice :: IO Sync.Shared
          resetTwice = runTerseDB (commit go) (loadSampleTreeNoTab xs)
            where
              go :: m Sync.Shared
              go = do
                resetTabulation
                resetTabulation
                genSyncShared
      x <- resetOnce
      y <- resetTwice
      x `shouldBe` y
  it "unlinking causes disjoint trees" $
    let gen =
          let go :: SampleGroupTree () -> Maybe Sync.Shared
              go xs
                | null $ s ^. Sync.store . Sync.toGroups . edges = Nothing
                | otherwise = Just s
                where
                  s = loadSampleTree xs
          in  suchThatMap arbitrary go

    in  forAll gen $ \(s) ->
          forAll (elements (HS.toList (s ^. Sync.store . Sync.toGroups . edges))) $ \(from, to) -> do
            let go :: m Sync.Shared
                go = do
                  unsafeUnlinkGroups from to
                  genSyncShared
            newS <- runTerseDB (commit go) s
            let newGroups = newS ^. Sync.store . Sync.toGroups
                rootsWithoutTo = HS.delete to (newGroups ^. roots)
                descendants :: GroupId -> HashSet GroupId
                descendants gId =
                  let children = fromJust (HM.lookup gId (newGroups ^. nodes)) ^. next
                  in  HS.insert gId (HS.unions (map descendants (HS.toList children)))
            shouldSatisfy (newS, from, to) $ \_ ->
              to `HS.member` (newS ^. Sync.store . Sync.toGroups . roots)
            shouldSatisfy (newS, from, to) $ \_ ->
              all
                ( \descendantOfTo ->
                    not . HS.member descendantOfTo . HS.unions . map descendants $
                      HS.toList rootsWithoutTo
                )
                (HS.toList (descendants to))
