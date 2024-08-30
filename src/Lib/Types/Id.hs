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

module Lib.Types.Id (GroupId, ActorId, SpaceId, EntityId, VersionId, AnyId (..), IdWithPfx) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String), object, (.:), (.=))
import Data.Aeson.Types (typeMismatch)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import System.Random.Stateful (Uniform (uniformM))
import Test.QuickCheck (Arbitrary (arbitrary), oneof)
import Test.QuickCheck.Gen (chooseInt)
import Test.QuickCheck.Instances ()
import Data.Serialize (Serialize)
import qualified Data.Serialize as Cereal
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Read (Read (readPrec), look, get, pfail, lift)
import Text.ParserCombinators.ReadP (skipSpaces)
import GHC.TypeLits (KnownSymbol (symbolSing), fromSSymbol, Symbol)
import Control.Monad (replicateM)
import Control.Applicative ((<|>))

-- | 12 byte random number
newtype Id = Id
  { getId :: Int
  }
  deriving (Eq, Ord, Hashable, Generic, Serialize)

idFromText :: T.Text -> Either String Id
idFromText t = BS16.decode (T.encodeUtf8 t) >>= Cereal.decode

idToText :: Id -> T.Text
idToText = T.decodeUtf8 . BS16.encode . Cereal.encode

instance Show Id where
  show = T.unpack . idToText
instance Uniform Id where
  uniformM gen = Id <$> uniformM gen
instance Arbitrary Id where
  arbitrary = Id <$> chooseInt (minBound, maxBound)

newtype IdWithPfx (k :: Symbol) = IdWithPfx
  { getIdWithPfx :: Id
  }
  deriving (Generic, Eq, Ord, Hashable, Arbitrary)
instance Uniform (IdWithPfx k) where
  uniformM gen = IdWithPfx <$> uniformM gen
instance KnownSymbol k => Show (IdWithPfx k) where
  show (IdWithPfx x) = fromSSymbol (symbolSing @k) <> show x
instance KnownSymbol k => Read (IdWithPfx k) where
  readPrec = do
    let pfx = fromSSymbol (symbolSing @k)
    lift skipSpaces
    t <- replicateM (16 + length pfx) get
    case T.splitAt (length pfx) (T.pack t) of
      (p, t) | p == T.pack pfx -> case idFromText t of
        Left _e -> pfail
        Right x -> pure (IdWithPfx x)
      _ -> fail "Incorrect format"
instance KnownSymbol k => ToJSON (IdWithPfx k) where
  toJSON (IdWithPfx x) = String (T.pack (fromSSymbol (symbolSing @k)) <> idToText x)
instance KnownSymbol k => FromJSON (IdWithPfx k) where
  parseJSON (String t) = case T.splitAt (length pfx) t of
    (p, t) | p == T.pack pfx -> case idFromText t of
      Left e -> fail e
      Right x -> pure (IdWithPfx x)
    _ -> fail "Incorrect format"
    where
      pfx = fromSSymbol (symbolSing @k)
  parseJSON json = typeMismatch "IdWithPfx" json

newtype GroupId = GroupId
  { getGroupId :: IdWithPfx "g_"
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Arbitrary)
  deriving (Show, Read)
       via (IdWithPfx "g_")
instance Uniform GroupId where
  uniformM gen = GroupId <$> uniformM gen

newtype ActorId = ActorId
  { getActorId :: IdWithPfx "a_"
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Arbitrary)
  deriving (Show, Read)
       via (IdWithPfx "a_")
instance Uniform ActorId where
  uniformM gen = ActorId <$> uniformM gen

newtype SpaceId = SpaceId
  { getSpaceId :: IdWithPfx "s_"
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Arbitrary)
  deriving (Show, Read)
       via (IdWithPfx "s_")
instance Uniform SpaceId where
  uniformM gen = SpaceId <$> uniformM gen

newtype EntityId = EntityId
  { getEntityId :: IdWithPfx "e_"
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Arbitrary)
  deriving (Show, Read)
       via (IdWithPfx "e_")
instance Uniform EntityId where
  uniformM gen = EntityId <$> uniformM gen

newtype VersionId = VersionId
  { getVersionId :: IdWithPfx "v_"
  }
  deriving (Generic, Eq, Ord, Hashable, ToJSON, FromJSON, Arbitrary)
  deriving (Show, Read)
       via (IdWithPfx "v_")
instance Uniform VersionId where
  uniformM gen = VersionId <$> uniformM gen

data AnyId
  = AnyIdActor ActorId
  | AnyIdGroup GroupId
  | AnyIdSpace SpaceId
  | AnyIdEntity EntityId VersionId
  | AnyIdVersion VersionId
  deriving (Eq, Ord, Show, Read)
instance Arbitrary AnyId where
  arbitrary = oneof
    [ AnyIdActor <$> arbitrary
    , AnyIdGroup <$> arbitrary
    , AnyIdSpace <$> arbitrary
    , AnyIdEntity <$> arbitrary <*> arbitrary
    , AnyIdVersion <$> arbitrary
    ]
instance ToJSON AnyId where
  toJSON x = case x of
    AnyIdActor x -> toJSON x
    AnyIdGroup x -> toJSON x
    AnyIdSpace x -> toJSON x
    AnyIdEntity x y -> object ["e" .= x, "v" .= y]
    AnyIdVersion x -> toJSON x
instance FromJSON AnyId where
  parseJSON json =
    (AnyIdActor <$> parseJSON json)
      <|> (AnyIdGroup <$> parseJSON json)
      <|> (AnyIdSpace <$> parseJSON json)
      <|> ((\o -> AnyIdEntity <$> o .: "e" <*> o .: "v") =<< parseJSON json)
      <|> (AnyIdVersion <$> parseJSON json)
