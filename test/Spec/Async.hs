module Spec.Async where

import Control.Concurrent.STM (atomically)
import Control.Lens ((^.))
import Control.Monad.Reader (runReader)
import qualified Lib.Async.Types.Store as Async
import qualified Lib.Async.Types.Store.Iso as Async
import qualified Lib.Sync.Types.Store as Sync
import Spec.Sync.Sample.Store (arbitraryShared)
import Test.QuickCheck (forAll)
import Test.Syd (Spec, describe, it, shouldBe)

asyncTests :: Spec
asyncTests = do
  describe "Isomorphisms" $ do
    it "Sync.Store -> Async.Store -> Sync.Store = Sync.Store" $
      forAll arbitraryShared $ \(syncShared, _, _) -> do
        let syncStore = syncShared ^. Sync.store
        asyncShared <- atomically Async.newShared
        let load = runReader (Async.loadSyncStore syncStore) asyncShared
            gen = runReader Async.genSyncStore asyncShared
        generatedSyncStore <- atomically (load >> gen)
        syncStore `shouldBe` generatedSyncStore
    it "Sync.Temp -> Async.Temp -> Sync.Temp = Sync.Temp" $
      forAll arbitraryShared $ \(syncShared, _, _) -> do
        let syncTemp = syncShared ^. Sync.temp
        asyncShared <- atomically Async.newShared
        let load = runReader (Async.loadSyncTemp syncTemp) asyncShared
            gen = runReader Async.genSyncTemp asyncShared
        generatedSyncTemp <- atomically (load >> gen)
        syncTemp `shouldBe` generatedSyncTemp
