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

import Lib.Api (Action, MutableAction, actMany, actManyStrict)
import Lib.Async.Types.Store (newShared)
import Lib.Async.Types.Store.Iso (genSyncStore)
import Lib.Async.Types.Monad (TerseM)
import Lib.Sync.Actions.Safe (emptyShared)
import Lib.Types.Id (ActorId, actorIdParser)
import qualified Lib.Sync.Types.Store as Sync
import Lib.Class (commit, runTerseDB, loadSyncShared)
import System.Random.Stateful (uniformM, globalStdGen)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, STM, TMVar, TVar, newTVarIO, newTMVarIO
  , takeTMVar, putTMVar, readTVar, readTMVar, writeTVar)
import Control.Monad.Reader (runReaderT)
import Control.Monad (forever, when, void)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai (responseLBS, pathInfo, RequestBodyLength (KnownLength), consumeRequestBodyStrict, requestMethod, requestHeaders, requestBodyLength, pathInfo, ResponseReceived)
import Network.HTTP.Types (ok200, notFound404, badRequest400, lengthRequired411, unsupportedMediaType415, methodNotAllowed405, methodPost, unauthorized401)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Data (Proxy (..))
import qualified Data.List.NonEmpty as NE
import Control.Monad.Reader (lift)

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
        storeCheckpoint state
        atomically (putTMVar creatingCheckpointVar ())

  (needsCheckpointVar :: TVar Bool) <- newTVarIO False

  void . forkIO . forever $ do
    threadDelay (1_000_000 * 60 * 60) -- every hour?
    needsCheckpoint <- atomically (readTVar needsCheckpointVar)
    when needsCheckpoint $ do
      putStrLn "Writing checkpoint"
      createCheckpoint

  runEnv 8000 $ \req respond ->
    let headers = requestHeaders req
        getActions :: (NonEmpty ActorId -> [Action] -> IO ResponseReceived) -> IO ResponseReceived
        getActions onActions = case getActors =<< lookup "Authorization" headers of
          Nothing -> respond $ responseLBS unauthorized401 [] ""
          Just actors ->
            if not (requestMethod req == methodPost)
            then respond $ responseLBS methodNotAllowed405 [] ""
            else if not (legitRequestBody (requestBodyLength req))
            then respond $ responseLBS lengthRequired411 [] ""
            else do
              body <- consumeRequestBodyStrict req
              case lookup "Content-Type" headers of
                Just "application/json" -> case Aeson.decode body of
                  Nothing -> respond $ responseLBS badRequest400 [] ""
                  Just (actions :: [Action]) -> onActions actors actions
                _ -> respond $ responseLBS unsupportedMediaType415 [] ""
        whenStoringDiff :: IO () -> TerseM IO ()
        whenStoringDiff x = lift $ do
          () <- atomically $ readTMVar creatingCheckpointVar
          x
          atomically $ writeTVar needsCheckpointVar True
    in  case pathInfo req of
          ["act"] -> getActions $ \actors actions -> do
            -- FIXME catch HasCycle and/or Unauthorized
            responses <- flip runReaderT s $ actMany
              (Proxy @(TerseM STM))
              (whenStoringDiff . storeSingleDiff)
              actors
              actions
            respond . responseLBS ok200 [] $ Aeson.encode responses
          ["actStrict"] -> getActions $ \actors actions -> do
            -- FIXME catch HasCycle and/or Unauthorized
            responses <- flip runReaderT s $ actManyStrict
              (Proxy @(TerseM STM))
              (whenStoringDiff . storeMultipleDiffs)
              actors
              actions
            respond . responseLBS ok200 [] $ Aeson.encode responses
          _ -> respond $ responseLBS notFound404 [] ""

storeCheckpoint :: Sync.Store -> IO ()
storeCheckpoint state = pure ()

storeSingleDiff :: MutableAction -> IO ()
storeSingleDiff mutableAction = pure ()

storeMultipleDiffs :: [MutableAction] -> IO ()
storeMultipleDiffs mutableActions = pure ()

legitRequestBody :: RequestBodyLength -> Bool
legitRequestBody (KnownLength l) = l < 1024
legitRequestBody _ = False

getActors :: BS.ByteString -> Maybe (NonEmpty ActorId)
getActors t = case Atto.parseOnly (actorIdParser `Atto.sepBy1` (Atto.char ',')) (T.decodeUtf8 t) of
  Left _e -> Nothing
  Right x -> Just (NE.fromList x)
