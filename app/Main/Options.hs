module Main.Options where

import Data.Word (Word16, Word64, Word8, Word32)
import GHC.TypeLits (Nat)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (..), FromJSON (..), withText, Value (Null, String), (.:), (.=), withObject, withText, object)
import Deriving.Aeson.Stock (Vanilla)
import Text.Read (readMaybe) 
import Control.Applicative ((<|>))
import qualified Data.Text as T


data DurationUnit
  = Hour
  | Day
  | Week
  | Month
  | Year
  deriving (Eq, Show, Read)
instance ToJSON DurationUnit where
  toJSON x = toJSON (show x)
instance FromJSON DurationUnit where
  parseJSON = withText "DurationUnit" $ \s ->
    case readMaybe (T.unpack s) of
      Nothing -> fail "Can't parse"
      Just x -> pure x


-- newtype OverMod a (n :: Nat) = OverMod { getOverMod :: a }
--   deriving (Eq, Show, Read, Real, Integral)
-- instance (Num a, KnownNat n) => Num (OverMod a n) where
--   (OverMod x) + (OverMod y) = OverMod (fromIntegral ((fromIntegral x + fromIntegral y) `mod` natVal (Proxy @n)))

newtype WithinHour = WithinHour
  { getWithinHour :: Word16 -- ^ Seconds since the top of the hour
  } deriving (Eq, Ord, Show, Read, ToJSON, FromJSON)

newWithinHour
  :: Word16 -- ^ Seconds, 0 - 3599
  -> WithinHour
newWithinHour x = WithinHour (x `mod` (60 * 60))

-- | Causes "second" component to be 0
newWithinHourByMinute
  :: Word8 -- ^ Minutes, 0 - 59
  -> Word8 -- ^ Seconds, 0 - 59
  -> WithinHour
newWithinHourByMinute m s =
  newWithinHour (fromIntegral (m `mod` 60) * fromIntegral (s `mod` 60))

newtype WithinDay = WithinDay
  { getWithinDay :: Word32 -- ^ Seconds since midnight
  } deriving (Eq, Show, Read, Ord, ToJSON, FromJSON)

newWithinDay
  :: Word32 -- ^ Seconds, 0 - 86399
  -> WithinDay
newWithinDay x = WithinDay (x `mod` (60 * 60 * 24))

newWithinDayByHour
  :: Word8 -- ^ Hour, 0 - 23
  -> WithinHour -- ^ Time in Hour
  -> WithinDay
newWithinDayByHour h x =
  newWithinDay (fromIntegral (h `mod` 24) * fromIntegral (getWithinHour x))

newtype WithinWeek = WithinWeek
  { getWithinWeek :: Word32 -- ^ Seconds since 00:00 on Sunday
  } deriving (Eq, Show, Read, Ord, ToJSON, FromJSON)

newWithinWeek
  :: Word32 -- ^ Seconds, 0 - 604799
  -> WithinWeek
newWithinWeek x = WithinWeek (x `mod` (60 * 60 * 24 * 7))

newWithinWeekByDay
  :: Word8 -- ^ Day of Week, 0 - 6
  -> WithinDay -- ^ Time in Day
  -> WithinWeek
newWithinWeekByDay d x =
  newWithinWeek (fromIntegral (d `mod` 7) * getWithinDay x)

newtype WithinMonth = WithinMonth
  { getWithinMonth :: Word32 -- ^ Seconds since 00:00 on the 1st of the month
  } deriving (Eq, Show, Read, Ord, ToJSON, FromJSON)

newWithinMonth
  :: Word32 -- ^ Seconds, 0 - 2419199 (28 days max)
  -> WithinMonth
newWithinMonth x = WithinMonth (x `mod` (60 * 60 * 24 * 28))

newWithinMonthByDay
  :: Word8 -- ^ Day of the Month, 0 - 27
  -> WithinDay -- ^ Time in Day
  -> WithinMonth
newWithinMonthByDay d x =
  newWithinMonth (fromIntegral (d `mod` 28) * getWithinDay x)

newtype WithinYear = WithinYear
  { getWithinYear :: Word32 -- ^ Seconds since 00:00 on January 1st
  } deriving (Eq, Show, Read, Ord, ToJSON, FromJSON)

newWithinYear
  :: Word32 -- ^ Seconds, 0 - 31535999 (365 days max)
  -> WithinYear
newWithinYear x = WithinYear (x `mod` (60 * 60 * 24 * 365))

newWithinYearByDay
  :: Word16 -- ^ Day of the Year, 0 - 364
  -> WithinDay -- ^ Time in Day
  -> WithinYear
newWithinYearByDay d x =
  newWithinYear (fromIntegral (d `mod` 365) * getWithinDay x)

data DurationUnitOffset
  = OffsetHour WithinHour
  | OffsetDay WithinDay
  | OffsetWeek WithinWeek
  | OffsetMonth WithinMonth
  | OffsetYear WithinYear
  deriving (Eq, Show, Read)
instance ToJSON DurationUnitOffset where
  toJSON x = case x of
    OffsetHour y -> object ["hour" .= y]
    OffsetDay y -> object ["day" .= y]
    OffsetWeek y -> object ["week" .= y]
    OffsetMonth y -> object ["month" .= y]
    OffsetYear y -> object ["year" .= y]
instance FromJSON DurationUnitOffset where
  parseJSON = withObject "DurationUnitOffset" $ \o -> 
    (OffsetHour <$> o .: "hour")
      <|> (OffsetDay <$> o .: "day")
      <|> (OffsetWeek <$> o .: "week")
      <|> (OffsetMonth <$> o .: "month")
      <|> (OffsetYear <$> o .: "year")


data SelectedBackend
  = NoBackend -- ^ Don't persist any changes made - *Warning* this ensures nothing is stored!
  | File -- ^ Use a flat file as the backend
  | Postgres -- ^ Use a PostgreSQL database as the backend
  deriving (Eq, Show, Read)
instance ToJSON SelectedBackend where
  toJSON x = case x of
    NoBackend -> toJSON (Nothing :: Maybe ())
    File -> String "file"
    Postgres -> String "postgres"
instance FromJSON SelectedBackend where
  parseJSON Null = pure NoBackend
  parseJSON json = flip (withText "SelectedBackend") json $ \s -> case s of
    "file" -> pure File
    "postgres" -> pure Postgres

-- | Configuration should be "flat" - to support configuration via a separate file, like YAML, or via environment variables, or command-line arguments.
data Configuration = Configuration
  { maxUploadLength :: Maybe Word64 -- ^ Maximum upload size for a set of actions
  , port :: Word16
  , purgeDiffOnCheckpoint :: Bool -- ^ Whether or not to purge all "diff" records when storing a new checkpoint
  , purgeCheckpointOnCheckpoint :: Bool -- ^ Whether or not to purge all "checkpoint" records when storing a new checkpoint
  , checkpointEvery :: Maybe DurationUnit -- ^ When, and how routinely, a checkpoint should be performed
  , checkpointEveryOffset :: Maybe DurationUnitOffset -- ^ When `checkpointEvery` is set, this defaults to the "0" of the unit - i.e., 00:00 for a day, or 0 seconds for a minute
  , backend :: SelectedBackend -- ^ Chosen backend
  , fileBackendPath :: FilePath
  , postgresHost :: String
  , postgresPort :: Word16
  , postgresUsername :: String
  , postgresPassword :: String
  , postgresDatabase :: String
  , postgresCheckpointTable :: String
  , postgresDiffTable :: String
  } deriving (Eq, Show, Read, Generic)
    -- deriving (ToJSON, FromJSON)
    --      via Vanilla Configuration


data CLIOptions = CLIOptions
  { config :: FilePath
  , defaultConfig :: Bool
  }
