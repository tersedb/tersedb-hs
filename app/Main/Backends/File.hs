module Main.Backends.File where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM)
import Control.Logging (errorL')
import Control.Monad (forM_)
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

data CheckpointOrDiff
  = Checkpoint Sync.Store
  | DiffSingle (NonEmpty ActorId) MutableAction
  | DiffMultiple (NonEmpty ActorId) [MutableAction]
  deriving (Eq, Show, Read)
instance ToJSON CheckpointOrDiff where
  toJSON x = case x of
    Checkpoint y -> object ["c" .= y]
    DiffSingle as y -> object ["d" .= object ["as" .= as, "a" .= y]]
    DiffMultiple as y -> object ["d" .= object ["as" .= as, "a" .= y]]
instance FromJSON CheckpointOrDiff where
  parseJSON = withObject "CheckpointOrDiff" $ \o ->
    (Checkpoint <$> o .: "c")
      <|> (onD =<< o .: "d")
   where
    onD o =
      (DiffMultiple <$> o .: "as" <*> o .: "a")
        <|> (DiffSingle <$> o .: "as" <*> o .: "a")

mkCheckpointFunctionsAndLoad
  :: FilePath
  -> Async.Shared
  -> IO
      ( Sync.Store -> IO ()
      , NonEmpty ActorId -> MutableAction -> IO ()
      , NonEmpty ActorId -> [MutableAction] -> IO ()
      )
mkCheckpointFunctionsAndLoad fileBackendPath s = do
  eEntries <-
    mapM (Aeson.eitherDecode . LT.encodeUtf8)
      . reverse
      . LT.lines
      <$> LT.readFile fileBackendPath
  case eEntries of
    Left e -> errorL' $ "Couldn't parse file backend: " <> T.pack e
    Right entries' -> do
      let (entries, _) = foldl' go ([], False) entries'
           where
            go (acc, stopped) x
              | stopped = (acc, stopped)
              | otherwise = case x of
                  Checkpoint _ -> (x : acc, True)
                  _ -> (x : acc, False)
          go :: CheckpointOrDiff -> TerseM STM ()
          go x = case x of
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
      flip runReaderT s . commit $ forM_ entries go
      pure
        ( \x -> LBS.appendFile fileBackendPath $ Aeson.encode (Checkpoint x) <> "\n"
        , \as x -> LBS.appendFile fileBackendPath $ Aeson.encode (DiffSingle as x) <> "\n"
        , \as x -> LBS.appendFile fileBackendPath $ Aeson.encode (DiffMultiple as x) <> "\n"
        )
