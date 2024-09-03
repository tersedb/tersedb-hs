module Main.Backends.File where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM, newTVarIO, readTVarIO, writeTVar)
import Control.Logging (errorL')
import Control.Monad (forM_)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Reader (runReaderT)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT
import Lib.Api (Authorize (..), MutableAction, mutate)
import Lib.Async.Types.Monad (TerseM)
import qualified Lib.Async.Types.Store as Async
import Lib.Async.Types.Store.Iso (loadSyncStore)
import Lib.Class (commit, resetTabulation)
import qualified Lib.Sync.Types.Store as Sync
import Lib.Types.Id (ActorId)
import Main.Options (
  Configuration (
    Configuration,
    purgeCheckpointsOnCheckpoint,
    purgeDiffsOnCheckpoint
  ),
 )

data CheckpointOrDiff
  = Checkpoint Sync.Store
  | DiffSingle (NonEmpty ActorId) MutableAction
  | DiffMultiple (NonEmpty ActorId) [MutableAction]
  deriving (Eq, Show, Read)
instance ToJSON CheckpointOrDiff where
  toJSON x = case x of
    Checkpoint y -> object ["c" .= y]
    DiffSingle as y -> object ["d" .= y, "a" .= as]
    DiffMultiple as y -> object ["d" .= y, "a" .= as]
instance FromJSON CheckpointOrDiff where
  parseJSON = withObject "CheckpointOrDiff" $ \o ->
    (Checkpoint <$> o .: "c")
      <|> (DiffSingle <$> o .: "a" <*> o .: "d")
      <|> (DiffMultiple <$> o .: "a" <*> o .: "d")

mkCheckpointFunctionsAndLoad
  :: FilePath
  -> Configuration
  -> Async.Shared
  -> IO
      ( Sync.Store -> IO ()
      , NonEmpty ActorId -> MutableAction -> IO ()
      , NonEmpty ActorId -> [MutableAction] -> IO ()
      , Bool
      )
mkCheckpointFunctionsAndLoad fileBackendPath Configuration{purgeDiffsOnCheckpoint, purgeCheckpointsOnCheckpoint} s = do
  eEntries <-
    mapM (Aeson.eitherDecode . LT.encodeUtf8)
      . reverse
      . LT.lines
      <$> LT.readFile fileBackendPath
  case eEntries of
    Left e -> errorL' $ "Couldn't parse file backend: " <> T.pack e
    Right entries' -> do
      loadedCheckpointVar <- newTVarIO False
      let (entries, _) = foldl' go ([], False) entries'
           where
            go (acc, stopped) x
              | stopped = (acc, stopped)
              | otherwise = case x of
                  Checkpoint _ -> (x : acc, True)
                  _ -> (x : acc, False)
          load :: CheckpointOrDiff -> TerseM STM ()
          load x = case x of
            DiffSingle as y -> do
              mAuth <- mutate as y
              case mAuth of
                Authorized () -> pure ()
                Unauthorized -> errorL' $ "Tried to load unauthorized action: " <> T.pack (show y)
            DiffMultiple as ys -> forM_ ys $ \y -> do
              mAuth <- mutate as y
              case mAuth of
                Authorized () -> pure ()
                Unauthorized -> errorL' $ "Tried to load unauthorized action: " <> T.pack (show y)
            Checkpoint y -> do
              loadSyncStore y
              resetTabulation
              liftBase $ writeTVar loadedCheckpointVar True
      flip runReaderT s . commit $ forM_ entries load
      let addCheckpoint :: Sync.Store -> IO ()
          addCheckpoint x
            | purgeCheckpointsOnCheckpoint =
                LBS.writeFile fileBackendPath $ Aeson.encode (Checkpoint x) <> "\n"
            | purgeDiffsOnCheckpoint = do
                eEntries <-
                  mapM (Aeson.eitherDecode . LT.encodeUtf8) . reverse . LT.lines
                    <$> LT.readFile fileBackendPath
                case eEntries of
                  Left e -> errorL' $ "Couldn't parse file backend on checkpoint: " <> T.pack e
                  Right entries -> do
                    let everythingTilLastCheckpoint = dropWhile (not . aCheckpoint) entries
                         where
                          aCheckpoint x = case x of
                            Checkpoint _ -> True
                            _ -> False
                        newEntries = reverse (Checkpoint x : everythingTilLastCheckpoint)
                    LT.writeFile
                      fileBackendPath
                      (LT.unlines (LT.decodeUtf8 . Aeson.encode <$> newEntries))
            | otherwise =
                LBS.appendFile fileBackendPath $ Aeson.encode (Checkpoint x) <> "\n"
      loadedCheckpoint <- readTVarIO loadedCheckpointVar
      pure
        ( addCheckpoint
        , \as x -> LBS.appendFile fileBackendPath $ Aeson.encode (DiffSingle as x) <> "\n"
        , \as x -> LBS.appendFile fileBackendPath $ Aeson.encode (DiffMultiple as x) <> "\n"
        , loadedCheckpoint
        )
