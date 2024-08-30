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

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (
  STM,
  TMVar,
  TVar,
  atomically,
  newTMVarIO,
  newTVarIO,
  putTMVar,
  readTMVar,
  readTVar,
  takeTMVar,
  writeTVar,
 )
import Control.Monad (forever, void, when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (lift, runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString as BS
import Data.Data (Proxy (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as T
import Lib.Api (Action, MutableAction, actMany, actManyStrict)
import Lib.Async.Types.Monad (TerseM)
import Lib.Async.Types.Store (newShared)
import Lib.Async.Types.Store.Iso (genSyncStore)
import Lib.Class (TerseDB, commit, loadSyncShared, runTerseDB)
import Lib.Sync.Actions.Safe (emptyShared)
import qualified Lib.Sync.Types.Store as Sync
import Lib.Types.Id (ActorId, actorIdParser)
import Network.HTTP.Types (
  badRequest400,
  lengthRequired411,
  methodNotAllowed405,
  methodPost,
  notFound404,
  ok200,
  unauthorized401,
  unsupportedMediaType415,
 )
import Network.Wai (
  RequestBodyLength (KnownLength),
  ResponseReceived,
  consumeRequestBodyStrict,
  pathInfo,
  requestBodyLength,
  requestHeaders,
  requestMethod,
  responseLBS,
 )
import Network.Wai.Handler.Warp (runEnv)
import System.Random.Stateful (globalStdGen, uniformM)

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
        route
          :: forall x r
           . (Aeson.ToJSON r)
          => ( forall n m
                . ( TerseDB n m
                  , MonadIO n
                  , MonadThrow m
                  )
               => Proxy m
               -> (x -> n ())
               -> NonEmpty ActorId
               -> [Action]
               -> n [r]
             )
          -> (x -> IO ())
          -> IO ResponseReceived
        route act storeDiff = getActions $ \actors actions -> do
          -- FIXME catch HasCycle and/or Unauthorized
          responses <-
            flip runReaderT s $
              act
                (Proxy @(TerseM STM))
                (whenStoringDiff . storeDiff)
                actors
                actions
          respond . responseLBS ok200 [] $ Aeson.encode responses
         where
          getActions
            :: (NonEmpty ActorId -> [Action] -> IO ResponseReceived) -> IO ResponseReceived
          getActions onActions = case getActors =<< lookup "Authorization" headers of
            Nothing -> respond $ responseLBS unauthorized401 [] ""
            Just actors ->
              if not (requestMethod req == methodPost)
                then respond $ responseLBS methodNotAllowed405 [] ""
                else
                  if not (legitRequestBody (requestBodyLength req))
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
     in case pathInfo req of
          ["act"] -> route actMany storeSingleDiff
          ["actStrict"] -> route actManyStrict storeMultipleDiffs
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
