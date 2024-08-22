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


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Types.Id (GroupId, ActorId, SpaceId, EntityId, VersionId) where

import Control.Monad (replicateM)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String))
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import Data.Hashable (Hashable)
import qualified Data.Text.Encoding as T
import Data.Word (Word8)
import GHC.Generics (Generic)
import System.Random.Stateful (Uniform (uniformM))
import Test.QuickCheck (Arbitrary (arbitrary), vectorOf)
import Test.QuickCheck.Gen (chooseUpTo)
import Test.QuickCheck.Instances ()
import Text.Read (Read (readPrec))

-- | 12 byte random number
newtype Id = Id
  { getId :: BS.ByteString
  }
  deriving (Eq, Ord, Hashable, Generic)

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
    xs :: [Word8] <-
      vectorOf 12 (fmap (fromIntegral . (`mod` 256)) (chooseUpTo maxBound))
    pure (Id (BS.pack xs))

newtype GroupId = GroupId
  { getGroupId :: Id
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform GroupId where
  uniformM gen = GroupId <$> uniformM gen

newtype ActorId = ActorId
  { getActorId :: Id
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform ActorId where
  uniformM gen = ActorId <$> uniformM gen

newtype SpaceId = SpaceId
  { getSpaceId :: Id
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform SpaceId where
  uniformM gen = SpaceId <$> uniformM gen

newtype EntityId = EntityId
  { getEntityId :: Id
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform EntityId where
  uniformM gen = EntityId <$> uniformM gen

newtype VersionId = VersionId
  { getVersionId :: Id
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Show, Read, Arbitrary)

instance Uniform VersionId where
  uniformM gen = VersionId <$> uniformM gen
