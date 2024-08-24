module Spec.Async where

import Test.Syd (Spec, describe, it, shouldBe)
import Test.QuickCheck (forAll)
import Spec.Sync.Sample.Store (arbitraryShared)
import qualified Lib.Sync.Types.Store as Sync
import qualified Lib.Async.Types.Store as Async
import Control.Concurrent.STM (atomically)
import Control.Monad.Reader (runReader)
import qualified Lib.Async.Types.Store.Iso as Async
import Control.Lens ((^.))


asyncTests :: Spec
asyncTests = do
  describe "Commutes" $ do
    it "Sync.Store -> Async.Store -> Sync.Store = Sync.Store" $
      forAll arbitraryShared $ \(syncShared,_,_) -> do
        let syncStore = syncShared ^. Sync.store
        asyncShared <- atomically Async.newShared
        let load = runReader (Async.loadSyncStore syncStore) asyncShared
            gen = runReader Async.genSyncStore asyncShared
        generatedSyncStore <- atomically (load >> gen)
        syncStore `shouldBe` generatedSyncStore
