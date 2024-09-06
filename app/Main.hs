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
  readTVarIO,
  takeTMVar,
  writeTVar,
 )
import Control.Logging (errorL', log', warn', withStderrLogging)
import Control.Monad (forever, void, when)
import Control.Monad.Catch (Handler (..), MonadThrow, catch, catches)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (lift, runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Proxy (..))
import Data.Default (Default (def))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Word (Word64)
import qualified Data.Yaml as Yaml
import Debug.Trace (traceShow)
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import Lib.Api (
  act,
  actMany,
  actManyStrict,
  actStrict,
 )
import Lib.Api.Action (Action, MutableAction, isCreate)
import Lib.Api.Response (Authorize (..))
import Lib.Async.Types.Monad (TerseM)
import Lib.Async.Types.Store (newShared)
import Lib.Async.Types.Store.Iso (genSyncStore)
import Lib.Class (
  TerseDB (
    unsafeReadGroupsEager,
    unsafeReadMembersLazy,
    unsafeReadOrganizationPermission,
    unsafeReadRecruiterPermission,
    unsafeReadUniversePermission
  ),
  commit,
  loadSyncShared,
  runTerseDB,
 )
import Lib.Sync.Actions.Safe (emptyShared)
import qualified Lib.Sync.Types.Store as Sync
import Lib.Types.Errors (CycleDetected (..), UnauthorizedAction (..))
import Lib.Types.Id (ActorId, GroupId, actorIdParser)
import qualified ListT
import Main.Backends.File (mkCheckpointFunctionsAndLoad)
import Main.Options (
  CLIOptions (..),
  Configuration (..),
  FileBackendConfig (..),
  HttpConfig (..),
  SelectedBackend (..),
  ToSeconds (..),
  programEnv,
  programOptions,
 )
import Network.HTTP.Types (
  badRequest400,
  conflict409,
  created201,
  lengthRequired411,
  methodNotAllowed405,
  methodPost,
  noContent204,
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
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (corsRequestHeaders),
  cors,
  simpleCors,
  simpleCorsResourcePolicy,
 )
import Network.Wai.Middleware.RequestLogger (
  OutputFormat (CustomOutputFormatWithDetailsAndHeaders, Detailed),
  RequestLoggerSettings (outputFormat),
  defaultRequestLoggerSettings,
  mkRequestLogger,
 )
import qualified Options.Applicative as OptParse
import Prettyprinter (indent, line, nest, pretty, viaShow, vsep)
import System.Exit (exitSuccess)
import System.Random.Stateful (globalStdGen, uniformM)

main :: IO ()
main = withStderrLogging $ do
  CLIOptions{..} <- OptParse.execParser programOptions

  when defaultConfig $ do
    BS.putStr (Yaml.encode (def :: Configuration))
    exitSuccess
  baseConfig <- case config of
    Nothing -> pure def
    Just path -> Yaml.decodeFileThrow path
  envConfig <- programEnv

  let cfg@Configuration{..} = baseConfig <> envConfig <> cliConfig

  when currentConfig $ do
    BS.putStr (Yaml.encode cfg)
    exitSuccess

  s <- atomically newShared

  ( storeCheckpoint :: Sync.Store -> IO ()
    , storeSingleDiff :: NonEmpty ActorId -> MutableAction -> IO ()
    , storeMultipleDiffs :: NonEmpty ActorId -> [MutableAction] -> IO ()
    , loadedCheckpoint
    ) <- case backend of
    NoBackend -> do
      warn' "No backend selected - all changes will be erased if the server stops"
      pure (\_ -> pure (), \_ _ -> pure (), \_ _ -> pure (), False)
    File FileBackendConfig{fileBackendPath} ->
      mkCheckpointFunctionsAndLoad fileBackendPath cfg s
    Postgres p -> do
      pure (\_ -> pure (), \_ _ -> pure (), \_ _ -> pure (), False)

  when (verbose && loadedCheckpoint) $ do
    let go :: TerseM STM Sync.Store
        go = genSyncStore
    s' <- runReaderT (commit go) s
    log' $ "Current Sync.Store: " <> T.pack (show s')

  (adminActor, adminGroup) <-
    if loadedCheckpoint
      then do
        log' "Checkpoint loaded"
        let getAdminGroupAndMember :: TerseM STM (ActorId, GroupId)
            getAdminGroupAndMember = do
              gs <- unsafeReadGroupsEager
              let gs' = flip UnfoldlM.filter gs $ \gId -> do
                    u <- unsafeReadUniversePermission gId
                    o <- unsafeReadOrganizationPermission gId
                    r <- unsafeReadRecruiterPermission gId
                    pure (u == maxBound && o == maxBound && r == maxBound)
                  getFirst
                    :: Maybe (ActorId, GroupId) -> GroupId -> TerseM STM (Maybe (ActorId, GroupId))
                  getFirst (Just xs) _ = pure (Just xs)
                  getFirst Nothing gId = do
                    ms <- unsafeReadMembersLazy gId
                    mAId <- ListT.head ms
                    case mAId of
                      Nothing -> pure Nothing
                      Just aId -> pure (Just (aId, gId))
              mAdmin <- UnfoldlM.foldlM' getFirst Nothing gs'
              case mAdmin of
                Nothing -> errorL' "Admin group and actor not found!"
                Just xs -> pure xs
        flip runReaderT s $ commit getAdminGroupAndMember
      else do
        log' "Checkpoint not loaded"
        (aA, aG) <- uniformM globalStdGen
        let init :: TerseM STM ()
            init = loadSyncShared $ emptyShared aA aG
        runReaderT (commit init) s
        pure (aA, aG)

  log' . T.pack $ "Admin Actor: " <> show adminActor
  log' . T.pack $ "Admin Group: " <> show adminGroup

  (creatingCheckpointVar :: TMVar ()) <- newTMVarIO () -- prevents mutations while checkout is happening
  let createCheckpoint :: IO ()
      createCheckpoint = do
        log' "Writing checkpoint"
        state <- atomically $ do
          takeTMVar creatingCheckpointVar
          runReaderT genSyncStore s
        storeCheckpoint state
        atomically (putTMVar creatingCheckpointVar ())
        log' "Checkpoint written"

  (needsCheckpointVar :: TVar Bool) <- newTVarIO True

  void . forkIO $ do
    case checkpointEveryOffset of
      Nothing -> pure ()
      Just offset -> threadDelay (1_000_000 * toSeconds offset)
    forever $ do
      threadDelay (1_000_000 * toSeconds (fromJust checkpointEvery)) -- every hour?
      needsCheckpoint <- readTVarIO needsCheckpointVar
      when needsCheckpoint $ do
        log' "Writing checkpoint"
        createCheckpoint
      atomically $ writeTVar needsCheckpointVar False

  log' $ "Running on port " <> T.pack (show (port http))
  loggerMiddleware <-
    mkRequestLogger $
      if verbose
        then
          defaultRequestLoggerSettings
            { outputFormat = CustomOutputFormatWithDetailsAndHeaders $ \z req stat mSize dur reqBody resp heads ->
                fromString . show $
                  vsep
                    [ "Timestamp: " <> viaShow z
                    , "Request: "
                        <> line
                        <> indent
                          4
                          ( vsep
                              [ "Method: " <> viaShow (requestMethod req)
                              , "Headers: "
                                  <> line
                                  <> indent
                                    4
                                    (vsep $ map (\(k, v) -> viaShow k <> ": " <> viaShow v) $ requestHeaders req)
                              , "Body: " <> viaShow reqBody
                              , "Body Size: " <> viaShow mSize
                              ]
                          )
                    , "Response: "
                        <> line
                        <> indent
                          4
                          ( vsep
                              [ "Status: " <> viaShow stat
                              , "Headers: "
                                  <> line
                                  <> indent 4 (vsep $ map (\(k, v) -> viaShow k <> ": " <> viaShow v) heads)
                              , "Body: " <> viaShow resp
                              ]
                          )
                    , "Duration: " <> viaShow dur
                    , line
                    ]
            }
        else defaultRequestLoggerSettings
  let corsPolicy _req =
        Just
          simpleCorsResourcePolicy
            { corsRequestHeaders = ["Authorization", "Content-Type"]
            }
  run (fromIntegral $ port http) . loggerMiddleware . cors corsPolicy $ \req respond ->
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
               -> m ()
               -> (mutMany -> n ())
               -> NonEmpty ActorId
               -> [Action]
               -> n [respMany]
             )
          -> (NonEmpty ActorId -> mutMany -> IO ())
          -> ( forall n m
                . ( TerseDB n m
                  , MonadIO n
                  , MonadThrow m
                  )
               => Proxy m
               -> m ()
               -> (mutSingle -> n ())
               -> NonEmpty ActorId
               -> Action
               -> n respSingle
             )
          -> (NonEmpty ActorId -> mutSingle -> IO ())
          -> (respSingle -> Action -> IO ResponseReceived)
          -> IO ResponseReceived
        route actMany storeDiffMany actSingle storeDiffSingle respondSingle =
          let onActions actors actions = do
                responses <-
                  flip runReaderT s $
                    actMany
                      (Proxy @(TerseM STM))
                      (lift (readTMVar creatingCheckpointVar)) -- Ensures that a checkpoint isn't currently happening
                      (whenStoringDiff . storeDiffMany actors)
                      actors
                      actions
                respond . responseLBS ok200 [] $ Aeson.encode responses
              onAction actors action = do
                response <-
                  flip runReaderT s $
                    actSingle
                      (Proxy @(TerseM STM))
                      (lift (readTMVar creatingCheckpointVar)) -- Ensures that a checkpoint isn't currently happening
                      (whenStoringDiff . storeDiffSingle actors)
                      actors
                      action
                respondSingle response action
           in getActions onActions onAction
         where
          getActions
            :: (NonEmpty ActorId -> [Action] -> IO ResponseReceived)
            -> (NonEmpty ActorId -> Action -> IO ResponseReceived)
            -> IO ResponseReceived
          getActions onActions onAction = case getActors =<< lookup "Authorization" headers of
            Nothing -> do
              warn' "No authorization header"
              respond $ responseLBS unauthorized401 [] ""
            Just actors ->
              if requestMethod req /= methodPost
                then respond $ responseLBS methodNotAllowed405 [] ""
                else
                  let continue = do
                        body <- consumeRequestBodyStrict req
                        case lookup "Content-Type" headers of
                          Just "application/json" -> case Aeson.eitherDecode body of
                            Left e -> case Aeson.eitherDecode body of
                              Left e' -> do
                                warn' $
                                  "List of Actions Parsing Failure: "
                                    <> T.pack e
                                    <> ", Single Action Parsing Failure: "
                                    <> T.pack e'
                                respond $ responseLBS badRequest400 [] ""
                              Right (action :: Action) -> onAction actors action
                            Right (actions :: [Action]) -> onActions actors actions
                          _ -> respond $ responseLBS unsupportedMediaType415 [] ""
                   in case maxUploadLength http of
                        Nothing -> continue
                        Just maxLength
                          | legitRequestBody (requestBodyLength req) maxLength -> continue
                          | otherwise -> respond $ responseLBS lengthRequired411 [] ""
          whenStoringDiff :: IO () -> TerseM IO ()
          whenStoringDiff x = lift $ do
            x
            atomically $ writeTVar needsCheckpointVar True
        handleCycle (CycleDetected cs) =
          respond . responseLBS conflict409 [] . LT.encodeUtf8 $
            "Cycle Detected: " <> LT.pack (show cs)
     in case pathInfo req of
          ["act"] ->
            let go = route actMany storeSingleDiff act storeSingleDiff $ \mAuth action ->
                  case mAuth of
                    Unauthorized -> do
                      warn' $ "Actors not authorized for action " <> T.pack (show action)
                      respond $ responseLBS unauthorized401 [] ""
                    Authorized mResp -> case mResp of
                      Nothing -> respond $ responseLBS noContent204 [] ""
                      Just r ->
                        respond . responseLBS (if isCreate action then created201 else ok200) [] $
                          Aeson.encode r
             in catch go handleCycle
          ["actStrict"] ->
            let handleUnauthorized UnauthorizedAction =
                  respond $ responseLBS unauthorized401 [] ""
                go = route actManyStrict storeMultipleDiffs actStrict storeSingleDiff $ \mResp action ->
                  case mResp of
                    Nothing -> respond $ responseLBS noContent204 [] ""
                    Just r ->
                      respond . responseLBS (if isCreate action then created201 else ok200) [] $
                        Aeson.encode r
             in catches
                  go
                  [ Handler handleCycle
                  , Handler handleUnauthorized
                  ]
          ["config"] -> undefined -- TODO adjust configuration fields at runtime
          _ -> respond $ responseLBS notFound404 [] ""

legitRequestBody :: RequestBodyLength -> Word64 -> Bool
legitRequestBody (KnownLength l) l' = l <= l'
legitRequestBody _ _ = False

getActors :: BS.ByteString -> Maybe (NonEmpty ActorId)
getActors t = case Atto.parseOnly (actorIdParser `Atto.sepBy1` (Atto.char ',')) (T.decodeUtf8 t) of
  Left _e -> Nothing
  Right x -> Just (NE.fromList x)
