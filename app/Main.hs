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

module Main (main) where

import Lib.Api (Action, MutableAction)
import Lib.Async.Types.Store (newShared)
import Lib.Async.Types.Store.Iso (genSyncStore)
import Lib.Async.Types.Monad (TerseM)
import Lib.Sync.Actions.Safe (emptyShared)
import qualified Lib.Sync.Types.Store as Sync
import Lib.Class (commit, runTerseDB, loadSyncShared)
import System.Random.Stateful (uniformM, globalStdGen)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, STM, TMVar, TVar, newTVarIO, newTMVarIO, takeTMVar, putTMVar, readTVar)
import Control.Monad.Reader (runReaderT)
import Control.Monad (forever, when, void)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai (responseLBS)
import Network.HTTP.Types (ok200)

main :: IO ()
main = do
  (adminActor, adminGroup) <- uniformM globalStdGen
  s <- atomically newShared
  let init :: TerseM STM ()
      init = loadSyncShared $ emptyShared adminActor adminGroup
  runReaderT (commit init) s

  putStrLn $ "Admin Actor: " <> show adminActor
  putStrLn $ "Admin Group: " <> show adminGroup

  (creatingCheckpointVar :: TMVar ()) <- newTMVarIO () -- prevents mutations while checkout is happening

  let createCheckpoint :: IO ()
      createCheckpoint = do
        state <- atomically $ do        
          takeTMVar creatingCheckpointVar
          runReaderT genSyncStore s
        storeState state
        atomically (putTMVar creatingCheckpointVar ())

  (needsCheckpointVar :: TVar Bool) <- newTVarIO False

  void . forkIO . forever $ do
    threadDelay (1_000_000 * 60 * 60) -- every hour?
    needsCheckpoint <- atomically (readTVar needsCheckpointVar)
    when needsCheckpoint $ do
      putStrLn "Writing checkpoint"
      createCheckpoint

  runEnv 8000 $ \req respond ->
    respond $ responseLBS ok200 [] "test"

storeState :: Sync.Store -> IO ()
storeState state = pure ()
