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

module Lib.Types.Permission where

import Control.Lens.TH (makeLensesFor)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (String),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson.Types (typeMismatch)
import Data.Hashable (Hashable)
import Data.Semigroup (Max (..))
import Deriving.Aeson.Stock (CustomJSON (..), Generic, PrefixedSnake)
import Test.QuickCheck (Arbitrary (arbitrary), elements)

class HasMinimumPermission a where
  hasMinimumPermission :: a -> a -> Bool

data CollectionPermission
  = Blind
  | Read
  | Create
  | Update
  | Delete
  deriving (Eq, Ord, Show, Read, Generic)
instance HasMinimumPermission CollectionPermission where
  hasMinimumPermission = (>=)
instance Hashable CollectionPermission
instance ToJSON CollectionPermission where
  toJSON x = String $ case x of
    Blind -> "b"
    Read -> "r"
    Create -> "c"
    Update -> "u"
    Delete -> "d"
instance FromJSON CollectionPermission where
  parseJSON json@(String x) = case x of
    "b" -> pure Blind
    "r" -> pure Read
    "c" -> pure Create
    "u" -> pure Update
    "d" -> pure Delete
    _ -> typeMismatch "CollectionPermission" json
  parseJSON json = typeMismatch "CollectionPermission" json
instance Semigroup CollectionPermission where
  x <> y = getMax $ Max x <> Max y
instance Monoid CollectionPermission where
  mempty = Blind
instance Arbitrary CollectionPermission where
  arbitrary = elements [Blind, Read, Create, Update, Delete]
instance Bounded CollectionPermission where
  minBound = Blind
  maxBound = Delete

data CollectionPermissionWithExemption = CollectionPermissionWithExemption
  { collectionPermissionPermission :: CollectionPermission
  , collectionPermissionExemption :: Bool
  }
  deriving (Eq, Show, Read, Generic)
instance ToJSON CollectionPermissionWithExemption where
  toJSON (CollectionPermissionWithExemption p e) = object ["p" .= p, "e" .= e]
instance FromJSON CollectionPermissionWithExemption where
  parseJSON = withObject "CollectionPermissionWithExemption" $ \o ->
    CollectionPermissionWithExemption <$> o .: "p" <*> o .: "e"
instance HasMinimumPermission CollectionPermissionWithExemption where
  hasMinimumPermission
    (CollectionPermissionWithExemption xp _)
    (CollectionPermissionWithExemption yp False) = xp >= yp
  hasMinimumPermission
    (CollectionPermissionWithExemption xp xe)
    (CollectionPermissionWithExemption yp True) = xe && (xp >= yp)
instance Hashable CollectionPermissionWithExemption
instance Semigroup CollectionPermissionWithExemption where
  (CollectionPermissionWithExemption xp xe)
    <> (CollectionPermissionWithExemption yp ye) =
      CollectionPermissionWithExemption (xp <> yp) (xe || ye)
instance Monoid CollectionPermissionWithExemption where
  mempty = CollectionPermissionWithExemption mempty False
instance Arbitrary CollectionPermissionWithExemption where
  arbitrary = CollectionPermissionWithExemption <$> arbitrary <*> arbitrary
instance Bounded CollectionPermissionWithExemption where
  minBound = CollectionPermissionWithExemption minBound False
  maxBound = CollectionPermissionWithExemption maxBound True
makeLensesFor
  [ ("collectionPermissionPermission", "collectionPermission")
  , ("collectionPermissionExemption", "exemption")
  ]
  ''CollectionPermissionWithExemption

data SinglePermission
  = NonExistent
  | Exists
  | Adjust
  | Obliterate
  deriving (Eq, Ord, Show, Read, Generic)
instance ToJSON SinglePermission where
  toJSON x = String $ case x of
    NonExistent -> "n"
    Exists -> "e"
    Adjust -> "a"
    Obliterate -> "o"
instance FromJSON SinglePermission where
  parseJSON json@(String x) = case x of
    "n" -> pure NonExistent
    "e" -> pure Exists
    "a" -> pure Adjust
    "o" -> pure Obliterate
    _ -> typeMismatch "SinglePermission" json
  parseJSON json = typeMismatch "SinglePermission" json
instance Semigroup SinglePermission where
  x <> y = getMax $ Max x <> Max y
instance Arbitrary SinglePermission where
  arbitrary = elements [NonExistent, Exists, Adjust, Obliterate]
instance Bounded SinglePermission where
  minBound = NonExistent
  maxBound = Obliterate

escalate
  :: CollectionPermissionWithExemption
  -> SinglePermission
  -> CollectionPermission
escalate CollectionPermissionWithExemption{..} s = case s of
  NonExistent
    | collectionPermissionExemption -> collectionPermissionPermission
    | otherwise -> Blind
  s' -> collectionPermissionPermission <> escalateWithoutExemption s'

escalateWithoutExemption
  :: SinglePermission
  -> CollectionPermission
escalateWithoutExemption s = case s of
  NonExistent -> Blind
  Exists -> Read
  Adjust -> Update
  Obliterate -> Delete

--
-- escalateOverMap
-- :: HashMap k CollectionPermissionWithExemption
-- -> HashMap k SinglePermission
-- -> HashMap k CollectionPermission
-- escalateOverMap collections singles =
-- let onlyCollections = fmap collectionPermissionPermission $ collections `HM.difference` singles
-- onlySingles = fmap singleToCollection $ singles `HM.difference` collections
-- both = HM.intersectionWith (\c s -> escalate c (Just s)) collections singles
-- in  HM.unions [onlyCollections, onlySingles, both]
