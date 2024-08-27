module Spec.Generic.Group where

import Test.Syd (Spec, it, shouldBe)
import Lib.Class (TerseDB (resetTabulation, genSyncShared, commit), TerseDBGen (runTerseDB))
import Test.QuickCheck (Testable(property))
import Spec.Sync.Sample.Tree (SampleGroupTree, loadSampleTreeNoTab, loadSampleTree)
import qualified Lib.Sync.Types.Store as Sync
import Data.Data (Proxy (Proxy))


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
