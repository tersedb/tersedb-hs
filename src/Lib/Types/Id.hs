{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , DeriveGeneric
  #-}

module Lib.Types.Id (GroupId, ActorId, SpaceId, EntityId, VersionId) where

import Data.Hashable (Hashable)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Text.Encoding as T
import Data.Word (Word8)
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), Value (String))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Control.Monad (replicateM)
import Text.Read (Read (readPrec))
import Test.QuickCheck (Arbitrary (arbitrary), vectorOf)
import Test.QuickCheck.Gen (chooseUpTo)
import Test.QuickCheck.Instances ()
import System.Random.Stateful (Uniform (uniformM))


-- | 12 byte random number
newtype Id = Id
  { getId :: BS.ByteString
  } deriving (Eq, Ord, Hashable, Generic)

instance Show Id where
  show (Id x) = show (BS16.encode x)

instance Read Id where
  readPrec = do
    bs <- readPrec
    case BS16.decode bs of
      Left e -> fail e
      Right x -> pure (Id x)

instance ToJSON Id where
  toJSON (Id x) = String (T.decodeUtf8 (BS16.encode x))

instance FromJSON Id where
  parseJSON (String x) = case BS16.decode (T.encodeUtf8 x) of
    Left e -> fail e
    Right x -> pure (Id (x))
  parseJSON json = typeMismatch "Id" json

instance Uniform Id where
  uniformM gen = do
    xs <- replicateM 12 (uniformM gen)
    pure (Id (BS.pack xs))

instance Arbitrary Id where
  arbitrary = do
    xs :: [Word8] <- vectorOf 12 (fmap (fromIntegral . (`mod` 256)) (chooseUpTo maxBound))
    pure (Id (BS.pack xs))


newtype GroupId = GroupId
  { getGroupId :: Id
  } deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform GroupId where
  uniformM gen = GroupId <$> uniformM gen

newtype ActorId = ActorId
  { getActorId :: Id
  } deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform ActorId where
  uniformM gen = ActorId <$> uniformM gen

newtype SpaceId = SpaceId
  { getSpaceId :: Id
  } deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform SpaceId where
  uniformM gen = SpaceId <$> uniformM gen

newtype EntityId = EntityId
  { getEntityId :: Id
  } deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform EntityId where
  uniformM gen = EntityId <$> uniformM gen

newtype VersionId = VersionId
  { getVersionId :: Id
  } deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform VersionId where
  uniformM gen = VersionId <$> uniformM gen
