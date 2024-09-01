module Main.Options where

import Control.Applicative ((<|>))
import Control.Lens (ix, (%~), (&))
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  Value (Null, String),
  object,
  withObject,
  withText,
  (.:),
  (.:?),
  (.=),
 )
import qualified Data.Attoparsec.Text as Atto
import Data.Char (toLower, toUpper)
import Data.Default (Default (..))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import Deriving.Aeson.Stock (CustomJSON (..), PrefixedSnake, Snake)
import GHC.Generics (Generic)
import GHC.TypeLits (Nat)
import Options.Applicative (
  ParserInfo,
  auto,
  flag',
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  optional,
  progDesc,
  short,
  showDefault,
  showDefaultWith,
  str,
  strOption,
  switch,
  value,
  (<**>),
 )
import qualified Options.Applicative as OptParse
import Text.Read (readMaybe)

data DurationUnit
  = Hour
  | Day
  | Week
  | Month
  | Year
  deriving (Eq, Show, Read)
instance ToJSON DurationUnit where
  toJSON x = toJSON (map toLower (show x))
instance FromJSON DurationUnit where
  parseJSON = withText "DurationUnit" $ \s ->
    case readMaybe (T.unpack s & ix 0 %~ toUpper) of
      Nothing -> fail "Can't parse"
      Just x -> pure x

-- newtype OverMod a (n :: Nat) = OverMod { getOverMod :: a }
--   deriving (Eq, Show, Read, Real, Integral)
-- instance (Num a, KnownNat n) => Num (OverMod a n) where
--   (OverMod x) + (OverMod y) = OverMod (fromIntegral ((fromIntegral x + fromIntegral y) `mod` natVal (Proxy @n)))

-- newtype WithinHour = WithinHour
--   { getWithinHour :: Word16 -- ^ Seconds since the top of the hour
--   } deriving (Eq, Ord, Show, Read, ToJSON, FromJSON)
--
-- newWithinHour
--   :: Word16 -- ^ Seconds, 0 - 3599
--   -> WithinHour
-- newWithinHour x = WithinHour (x `min` (60 * 60))
--
-- -- | Causes "second" component to be 0
-- newWithinHourByMinute
--   :: Word8 -- ^ Minutes, 0 - 59
--   -> Word8 -- ^ Seconds, 0 - 59
--   -> WithinHour
-- newWithinHourByMinute m s =
--   newWithinHour (fromIntegral (m `min` 60) * fromIntegral (s `min` 60))
--
-- newtype WithinDay = WithinDay
--   { getWithinDay :: Word32 -- ^ Seconds since midnight
--   } deriving (Eq, Show, Read, Ord, ToJSON, FromJSON)
--
-- newWithinDay
--   :: Word32 -- ^ Seconds, 0 - 86399
--   -> WithinDay
-- newWithinDay x = WithinDay (x `min` (60 * 60 * 24))
--
-- newWithinDayByHour
--   :: Word8 -- ^ Hour, 0 - 23
--   -> WithinHour -- ^ Time in Hour
--   -> WithinDay
-- newWithinDayByHour h x =
--   newWithinDay (fromIntegral (h `min` 24) * fromIntegral (getWithinHour x))
--
-- newtype WithinWeek = WithinWeek
--   { getWithinWeek :: Word32 -- ^ Seconds since 00:00 on Sunday
--   } deriving (Eq, Show, Read, Ord, ToJSON, FromJSON)
--
-- newWithinWeek
--   :: Word32 -- ^ Seconds, 0 - 604799
--   -> WithinWeek
-- newWithinWeek x = WithinWeek (x `min` (60 * 60 * 24 * 7))
--
-- newWithinWeekByDay
--   :: Word8 -- ^ Day of Week, 0 - 6
--   -> WithinDay -- ^ Time in Day
--   -> WithinWeek
-- newWithinWeekByDay d x =
--   newWithinWeek (fromIntegral (d `min` 7) * getWithinDay x)
--
-- newtype WithinMonth = WithinMonth
--   { getWithinMonth :: Word32 -- ^ Seconds since 00:00 on the 1st of the month
--   } deriving (Eq, Show, Read, Ord, ToJSON, FromJSON)
--
-- newWithinMonth
--   :: Word32 -- ^ Seconds, 0 - 2419199 (28 days max)
--   -> WithinMonth
-- newWithinMonth x = WithinMonth (x `min` (60 * 60 * 24 * 28))
--
-- newWithinMonthByDay
--   :: Word8 -- ^ Day of the Month, 0 - 27
--   -> WithinDay -- ^ Time in Day
--   -> WithinMonth
-- newWithinMonthByDay d x =
--   newWithinMonth (fromIntegral (d `min` 28) * getWithinDay x)
--
-- newtype WithinYear = WithinYear
--   { getWithinYear :: Word32 -- ^ Seconds since 00:00 on January 1st
--   } deriving (Eq, Show, Read, Ord, ToJSON, FromJSON)
--
-- newWithinYear
--   :: Word32 -- ^ Seconds, 0 - 31535999 (365 days max)
--   -> WithinYear
-- newWithinYear x = WithinYear (x `min` (60 * 60 * 24 * 365))
--
-- newWithinYearByDay
--   :: Word16 -- ^ Day of the Year, 0 - 364
--   -> WithinDay -- ^ Time in Day
--   -> WithinYear
-- newWithinYearByDay d x =
--   newWithinYear (fromIntegral (d `min` 365) * getWithinDay x)
--
-- data DurationUnitOffset
--   = OffsetHour WithinHour
--   | OffsetDay WithinDay
--   | OffsetWeek WithinWeek
--   | OffsetMonth WithinMonth
--   | OffsetYear WithinYear
--   deriving (Eq, Show, Read)
-- instance ToJSON DurationUnitOffset where
--   toJSON x = case x of
--     OffsetHour y -> object ["hour" .= y]
--     OffsetDay y -> object ["day" .= y]
--     OffsetWeek y -> object ["week" .= y]
--     OffsetMonth y -> object ["month" .= y]
--     OffsetYear y -> object ["year" .= y]
-- instance FromJSON DurationUnitOffset where
--   parseJSON = withObject "DurationUnitOffset" $ \o ->
--     (OffsetHour <$> o .: "hour")
--       <|> (OffsetDay <$> o .: "day")
--       <|> (OffsetWeek <$> o .: "week")
--       <|> (OffsetMonth <$> o .: "month")
--       <|> (OffsetYear <$> o .: "year")

data DurationUnitOffsetText
  = OffsetTextSecond Int
  | OffsetTextMinute Int
  | OffsetTextHour Int
  | OffsetTextDay Int
  | OffsetTextWeek Int
  deriving (Eq, Show, Read)

newtype DurationUnitOffset = DurationUnitOffset
  { getDurationUnitOffset :: [DurationUnitOffsetText]
  }
  deriving (Eq, Show, Read)

instance ToJSON DurationUnitOffset where
  toJSON (DurationUnitOffset xs) = String $ T.concat (map go xs)
   where
    go x = case x of
      OffsetTextSecond y -> T.pack (show y) <> "s"
      OffsetTextMinute y -> T.pack (show y) <> "m"
      OffsetTextHour y -> T.pack (show y) <> "h"
      OffsetTextDay y -> T.pack (show y) <> "d"
      OffsetTextWeek y -> T.pack (show y) <> "w"
instance FromJSON DurationUnitOffset where
  parseJSON = withText "DurationUnitOffsetText" $ \s -> case Atto.parseOnly durationOffset s of
    Left e -> fail e
    Right x -> pure x

durationOffset :: Atto.Parser DurationUnitOffset
durationOffset = DurationUnitOffset <$> Atto.many1 go
 where
  go =
    (OffsetTextSecond <$> decimalFollowedBy 's')
      <|> (OffsetTextMinute <$> decimalFollowedBy 'm')
      <|> (OffsetTextHour <$> decimalFollowedBy 'h')
      <|> (OffsetTextDay <$> decimalFollowedBy 'd')
      <|> (OffsetTextWeek <$> decimalFollowedBy 'w')
  decimalFollowedBy c = do
    x <- Atto.decimal
    _ <- Atto.char c
    pure x

durationOffsetToSeconds :: DurationUnitOffset -> Int
durationOffsetToSeconds = foldr go 0 . getDurationUnitOffset
 where
  go x acc = case x of
    OffsetTextSecond y -> acc + y
    OffsetTextMinute y -> acc + (y * 60)
    OffsetTextHour y -> acc + (y * 60 * 60)
    OffsetTextDay y -> acc + (y * 60 * 60 * 24)
    OffsetTextWeek y -> acc + (y * 60 * 60 * 24 * 7)

data PostgresBackendConfig = PostgresBackendConfig
  { postgresHost :: String
  , postgresPort :: Word16
  , postgresUsername :: String
  , postgresPassword :: String
  , postgresDatabase :: String
  , postgresCheckpointTable :: String
  , postgresDiffTable :: String
  }
  deriving (Eq, Show, Read, Generic)
  deriving
    (ToJSON, FromJSON)
    via PrefixedSnake "postgres" PostgresBackendConfig

postgresBackendParser :: OptParse.Parser PostgresBackendConfig
postgresBackendParser =
  flag' () (long "postgres-backend")
    *> ( PostgresBackendConfig
          <$> parseHost
          <*> parsePort
          <*> parseUsername
          <*> parsePassword
          <*> parseDatabase
          <*> parseCheckpointTable
          <*> parseDiffTable
       )
 where
  parseHost =
    strOption $
      long "postgres-host"
        <> metavar "POSTGRES_HOST"
        <> showDefault
        <> value "localhost"
        <> help "Host of the Postgres Backend"
  parsePort =
    option auto $
      long "postgres-port"
        <> metavar "POSTGRES_PORT"
        <> showDefault
        <> value 5678
        <> help "Port of the Postgres Backend"
  parseUsername =
    strOption $
      long "postgres-username"
        <> metavar "POSTGRES_USERNAME"
        <> showDefault
        <> value "postgres"
        <> help "Username for the Postgres Backend"
  parsePassword =
    strOption $
      long "postgres-password"
        <> metavar "POSTGRES_PASSWORD"
        <> help "Password for the Postgres Backend"
  parseDatabase =
    strOption $
      long "postgres-db"
        <> metavar "POSTGRES_DB"
        <> showDefault
        <> value "postgres"
        <> help "Database for the Postgres Backend"
  parseCheckpointTable =
    strOption $
      long "postgres-checkpoint-table"
        <> metavar "POSTGRES_CHECKPOINT_TABLE"
        <> showDefault
        <> value "tersedb_checkpoints"
        <> help "Table for Checkpoints"
  parseDiffTable =
    strOption $
      long "postgres-checkpoint-table"
        <> metavar "POSTGRES_DIFF_TABLE"
        <> showDefault
        <> value "tersedb_diffs"
        <> help "Table for Diffs"

data FileBackendConfig = FileBackendConfig
  { fileBackendPath :: FilePath
  }
  deriving (Eq, Show, Read, Generic)
  deriving
    (ToJSON, FromJSON)
    via PrefixedSnake "fileBackend" FileBackendConfig

fileBackendParser :: OptParse.Parser FileBackendConfig
fileBackendParser =
  flag' () (long "file-backend")
    *> (FileBackendConfig <$> parsePath)
 where
  parsePath =
    strOption $
      long "file-backend-path"
        <> metavar "FILE_BACKEND_PATH"
        <> showDefault
        <> value "./backend"
        <> help "Path for file-based Backend"

data SelectedBackend
  = -- | Don't persist any changes made - *Warning* this ensures nothing is stored!
    NoBackend
  | -- | Use a flat file as the backend
    File FileBackendConfig
  | -- | Use a PostgreSQL database as the backend
    Postgres PostgresBackendConfig
  deriving (Eq, Show, Read)
instance ToJSON SelectedBackend where
  toJSON x = case x of
    NoBackend -> toJSON (Nothing :: Maybe ())
    File y -> object ["file" .= y]
    Postgres y -> object ["postgres" .= y]
instance FromJSON SelectedBackend where
  parseJSON Null = pure NoBackend
  parseJSON json = flip (withObject "SelectedBackend") json $ \o ->
    (File <$> o .: "file")
      <|> (Postgres <$> o .: "postgres")
instance Default SelectedBackend where
  def = NoBackend

selectedBackendParser :: OptParse.Parser SelectedBackend
selectedBackendParser =
  (NoBackend <$ optional (flag' () (long "no-backend")))
    <|> (File <$> fileBackendParser)
    <|> (Postgres <$> postgresBackendParser)

data HttpConfig = HttpConfig
  { maxUploadLength :: Maybe Word64
  -- ^ Maximum upload size for a set of actions
  , port :: Word16
  }
  deriving (Eq, Show, Read, Generic)
  deriving
    (ToJSON, FromJSON)
    via Snake HttpConfig
instance Default HttpConfig where
  def =
    HttpConfig
      { maxUploadLength = Just 1024
      , port = 8000
      }

httpConfigParser :: OptParse.Parser HttpConfig
httpConfigParser =
  HttpConfig <$> maxUploadParser <*> portParser
 where
  maxUploadParser =
    optional . option auto $
      long "max-upload-length"
        <> metavar "MAX_UPLOAD_LENGTH"
        <> showDefault
        <> value (fromJust $ maxUploadLength def)
        <> help "Maximum file size for uploads"
  portParser =
    option auto $
      long "port"
        <> metavar "PORT"
        <> showDefault
        <> value (port def)
        <> help "Port to bind the http server to"

-- | Configuration should be "flat" - to support configuration via a separate file, like YAML, or via environment variables, or command-line arguments.
data Configuration = Configuration
  { http :: HttpConfig
  , purgeDiffsOnCheckpoint :: Bool
  -- ^ Whether or not to purge all "diff" records when storing a new checkpoint
  , purgeCheckpointsOnCheckpoint :: Bool
  -- ^ Whether or not to purge all "checkpoint" records when storing a new checkpoint
  , checkpointEvery :: Maybe DurationUnit
  -- ^ When, and how routinely, a checkpoint should be performed
  , checkpointEveryOffset :: Maybe DurationUnitOffset
  -- ^ When `checkpointEvery` is set, this defaults to the "0" of the unit - i.e., 00:00 for a day, or 0 seconds for a minute
  , backend :: SelectedBackend
  -- ^ Chosen backend
  }
  deriving (Eq, Show, Read, Generic)
  deriving
    (ToJSON)
    via Snake Configuration

instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \o ->
    Configuration
      <$> (fromMaybe def <$> o .:? "http")
      <*> (fromMaybe (purgeDiffsOnCheckpoint def) <$> o .:? "purge_diffs_on_checkpoint")
      <*> ( fromMaybe (purgeCheckpointsOnCheckpoint def)
              <$> o .:? "purge_checkpoints_on_checkpoint"
          )
      <*> (fromMaybe (checkpointEvery def) <$> o .:? "checkpoint_every")
      <*> (fromMaybe (checkpointEveryOffset def) <$> o .:? "checkpoint_every_offset")
      <*> (fromMaybe def <$> o .:? "backend")
instance Default Configuration where
  def =
    Configuration
      { http = def
      , purgeDiffsOnCheckpoint = False
      , purgeCheckpointsOnCheckpoint = False
      , checkpointEvery = Just Hour
      , checkpointEveryOffset = Nothing
      , backend = def
      }
instance Semigroup Configuration where
  x <> y =
    let applyIfNotDef
          :: forall a
           . (Eq a)
          => (Configuration -> a)
          -> (a -> Configuration -> Configuration)
          -> Configuration
          -> Configuration
        applyIfNotDef f appF z = if f y /= f def then appF (f y) z else z
     in applyIfNotDef http (\a b -> b{http = a})
          . applyIfNotDef purgeDiffsOnCheckpoint (\a b -> b{purgeDiffsOnCheckpoint = a})
          . applyIfNotDef
            purgeCheckpointsOnCheckpoint
            (\a b -> b{purgeCheckpointsOnCheckpoint = a})
          . applyIfNotDef checkpointEvery (\a b -> b{checkpointEvery = a})
          . applyIfNotDef checkpointEveryOffset (\a b -> b{checkpointEveryOffset = a})
          . applyIfNotDef backend (\a b -> b{backend = a})
          $ x

configParser :: OptParse.Parser Configuration
configParser =
  Configuration
    <$> httpConfigParser
    <*> purgeDiffsParser
    <*> purgeCheckpointsParser
    <*> optional checkpointEveryParser
    <*> optional checkpointEveryOffsetParser
    <*> selectedBackendParser
 where
  purgeDiffsParser =
    switch $
      long "purge-diffs-on-checkpoint"
        <> help "Remove all recorded diffs when a checkpoint is made"
  purgeCheckpointsParser =
    switch $
      long "purge-checkpoints-on-checkpoint"
        <> help "Remove all checkpoints when a checkpoint is made"
  checkpointEveryParser =
    option durationUnit $
      long "checkpoint-interval"
        <> metavar "CHECKPOINT_INTERVAL"
        <> showDefaultWith (map toLower . show)
        <> value (fromJust $ checkpointEvery def)
        <> help "How often to perform checkpoints"
   where
    durationUnit = do
      s <- str
      case s of
        "hour" -> pure Hour
        "day" -> pure Day
        "week" -> pure Week
        "month" -> pure Month
        "year" -> pure Year
        _ -> fail "Not a valid DurationUnit"
  checkpointEveryOffsetParser =
    option durationUnitOffset $
      long "checkpoint-interval-offset"
        <> metavar "CHECKPOINT_INTERVAL_OFFSET"
        <> showDefaultWith showDurationUnitOffset
        <> value (DurationUnitOffset [OffsetTextSecond 0])
        <> help
          "Offset within interval - 's' is \"seconds\", 'm' is \"minutes\", _h_our, _d_ay, _w_eek"
   where
    durationUnitOffset = do
      s <- str
      case Atto.parseOnly durationOffset (T.pack s) of
        Left e -> fail e
        Right xs -> pure xs
    showDurationUnitOffset = concatMap go . getDurationUnitOffset
     where
      go x = case x of
        OffsetTextSecond y -> show y <> "s"
        OffsetTextMinute y -> show y <> "m"
        OffsetTextHour y -> show y <> "h"
        OffsetTextDay y -> show y <> "d"
        OffsetTextWeek y -> show y <> "w"

data CLIOptions = CLIOptions
  { config :: FilePath
  , defaultConfig :: Bool
  , cliConfig :: Configuration
  }

cliOptions :: OptParse.Parser CLIOptions
cliOptions = CLIOptions <$> configPath <*> showDefaultConfig <*> configParser
 where
  configPath =
    strOption $
      long "config"
        <> short 'c'
        <> metavar "CONFIG_PATH"
        <> showDefault
        <> value "./tersedb.yml"
        <> help "Location of configuration file"
  showDefaultConfig =
    switch $
      long "default-config"
        <> help "Print the default configuration to stdout"

programOptions :: ParserInfo CLIOptions
programOptions =
  info (cliOptions <**> helper) $
    fullDesc
      <> progDesc "Start a TerseDB server"
      <> header "tersedb - entity management system"
