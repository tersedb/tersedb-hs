module Spec.Async where

import Control.Concurrent.STM (STM, atomically)
import Control.Lens ((^.))
import Control.Monad.Reader (runReaderT, ReaderT)
import qualified Lib.Async.Types.Store as Async
import qualified Lib.Async.Types.Store.Iso as Async
import qualified Lib.Async.Actions.Tabulation as Async
import qualified Lib.Sync.Types.Store as Sync
import qualified Lib.Sync.Actions.Tabulation as Sync
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
        generatedSyncStore <- atomically $ flip runReaderT asyncShared $ do
          Async.loadSyncStore syncStore
          Async.genSyncStore
        syncStore `shouldBe` generatedSyncStore
    it "Sync.Temp -> Async.Temp -> Sync.Temp = Sync.Temp" $
      forAll arbitraryShared $ \(syncShared, _, _) -> do
        let syncTemp = syncShared ^. Sync.temp
        asyncShared <- atomically Async.newShared
        generatedSyncTemp <- atomically $ flip runReaderT asyncShared $ do
          Async.loadSyncTemp syncTemp
          Async.genSyncTemp
        syncTemp `shouldBe` generatedSyncTemp
    it "Sync.Store -> Async.Store -> Async.Temp -> Sync.Temp = Sync.Store -> Sync.Temp" $
      forAll arbitraryShared $ \(syncShared, _, _) -> do
        let syncStore = syncShared ^. Sync.store
        asyncShared <- atomically Async.newShared
        generatedSyncTemp <- atomically $ flip runReaderT asyncShared $ do
          Async.loadSyncStore syncStore
          Async.loadTempFromStore
          Async.genSyncTemp
        Sync.tempFromStore syncStore `shouldBe` generatedSyncTemp
