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

import Main.Options (programOptions)
import qualified Options.Applicative as OptParse
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
import Control.Monad.Catch (MonadThrow, catch, catches, Handler (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (lift, runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString as BS
import Data.Data (Proxy (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as T
import Lib.Api (Action, MutableAction, actMany, actManyStrict, act, actStrict, isCreate, Authorize (..))
import Lib.Types.Errors (CycleDetected (..), UnauthorizedAction (..))
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
  created201,
  noContent204,
  unauthorized401,
  unsupportedMediaType415,
  conflict409
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
import Network.Wai.Handler.Warp (run)
import System.Random.Stateful (globalStdGen, uniformM)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Network.Wai.Middleware.RequestLogger (mkRequestLogger, defaultRequestLoggerSettings)
import Control.Logging (withStderrLogging, log', warn')
import qualified Data.Text as T

main :: IO ()
main = withStderrLogging $ do
  cliOptions <- OptParse.execParser programOptions

  (adminActor, adminGroup) <- uniformM globalStdGen
  s <- atomically newShared
  let init :: TerseM STM ()
      init = loadSyncShared $ emptyShared adminActor adminGroup
  runReaderT (commit init) s

  log' . T.pack $ "Admin Actor: " <> show adminActor
  log' . T.pack $ "Admin Group: " <> show adminGroup

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
      log' "Writing checkpoint"
      createCheckpoint

  let port = 8000
  log' $ "Running on port " <> T.pack (show port)
  loggerMiddleware <- mkRequestLogger defaultRequestLoggerSettings
  run port . loggerMiddleware $ \req respond ->
    let headers = requestHeaders req
        route
          :: forall mutMany respMany mutSingle respSingle
           . (Aeson.ToJSON respMany)
          => ( forall n m
                . ( TerseDB n m
                  , MonadIO n
                  , MonadThrow m
                  )
               => Proxy m
               -> (m ())
               -> (mutMany -> n ())
               -> NonEmpty ActorId
               -> [Action]
               -> n [respMany]
             )
          -> (mutMany -> IO ())
          -> ( forall n m
                . ( TerseDB n m
                  , MonadIO n
                  , MonadThrow m
                  )
               => Proxy m
               -> (m ())
               -> (mutSingle -> n ())
               -> NonEmpty ActorId
               -> Action
               -> n respSingle
             )
          -> (mutSingle -> IO ())
          -> (respSingle -> Action -> IO ResponseReceived)
          -> IO ResponseReceived
        route actMany storeDiffMany actSingle storeDiffSingle respondSingle =
          let onActions actors actions = do
                responses <-
                  flip runReaderT s $
                    actMany
                      (Proxy @(TerseM STM))
                      (lift (readTMVar creatingCheckpointVar)) -- Ensures that a checkpoint isn't currently happening
                      (whenStoringDiff . storeDiffMany)
                      actors
                      actions
                respond . responseLBS ok200 [] $ Aeson.encode responses
              onAction actors action = do
                response <-
                  flip runReaderT s $
                    actSingle
                      (Proxy @(TerseM STM))
                      (lift (readTMVar creatingCheckpointVar)) -- Ensures that a checkpoint isn't currently happening
                      (whenStoringDiff . storeDiffSingle)
                      actors
                      action
                respondSingle response action
          in  getActions onActions onAction
         where
          getActions
            :: (NonEmpty ActorId -> [Action] -> IO ResponseReceived)
            -> (NonEmpty ActorId -> Action -> IO ResponseReceived)
            -> IO ResponseReceived
          getActions onActions onAction = case getActors =<< lookup "Authorization" headers of
            Nothing -> respond $ responseLBS unauthorized401 [] ""
            Just actors ->
              if requestMethod req /= methodPost
                then respond $ responseLBS methodNotAllowed405 [] ""
                else
                  if not (legitRequestBody (requestBodyLength req))
                    then respond $ responseLBS lengthRequired411 [] ""
                    else do
                      body <- consumeRequestBodyStrict req
                      case lookup "Content-Type" headers of
                        Just "application/json" -> case Aeson.eitherDecode body of
                          Left e -> case Aeson.eitherDecode body of
                            Left e' -> do
                              warn' $ T.pack e <> ", " <> T.pack e'
                              respond $ responseLBS badRequest400 [] ""
                            Right (action :: Action) -> onAction actors action
                          Right (actions :: [Action]) -> onActions actors actions
                        _ -> respond $ responseLBS unsupportedMediaType415 [] ""
          whenStoringDiff :: IO () -> TerseM IO ()
          whenStoringDiff x = lift $ do
            x
            atomically $ writeTVar needsCheckpointVar True
        handleCycle (CycleDetected cs) =
          respond $ responseLBS conflict409 [] ""
     in case pathInfo req of
          ["act"] ->
            let go = route actMany storeSingleDiff act storeSingleDiff $ \mAuth action ->
                      case mAuth of
                        Unauthorized -> respond $ responseLBS unauthorized401 [] ""
                        Authorized mResp -> case mResp of
                          Nothing -> respond $ responseLBS noContent204 [] ""
                          Just r -> respond . responseLBS (if isCreate action then created201 else ok200) [] $ Aeson.encode r
            in  catch go handleCycle
          ["actStrict"] ->
            let handleUnauthorized UnauthorizedAction =
                  respond $ responseLBS unauthorized401 [] ""
                go = route actManyStrict storeMultipleDiffs actStrict storeSingleDiff $ \mResp action ->
                      case mResp of
                        Nothing -> respond $ responseLBS noContent204 [] ""
                        Just r -> respond . responseLBS (if isCreate action then created201 else ok200) [] $ Aeson.encode r
            in  catches go
                  [ Handler handleCycle
                  , Handler handleUnauthorized
                  ]
          ["config"] -> undefined -- TODO adjust configuration fields at runtime
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
