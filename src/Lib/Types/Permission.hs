{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Types.Permission where

import Control.Lens.TH (makeLensesFor)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Hashable (Hashable)
import Data.Semigroup (Max (..))
import Deriving.Aeson.Stock (CustomJSON (..), Generic, PrefixedSnake)
import Test.QuickCheck (Arbitrary (arbitrary), elements)

data CollectionPermission
  = Blind
  | Read
  | Create
  | Update
  | Delete
  deriving (Eq, Ord, Show, Read, Generic)
instance Hashable CollectionPermission
instance ToJSON CollectionPermission where
  toJSON x = String $ case x of
    Blind -> "blind"
    Read -> "read"
    Create -> "create"
    Update -> "update"
    Delete -> "delete"
instance FromJSON CollectionPermission where
  parseJSON json@(String x) = case x of
    "blind" -> pure Blind
    "read" -> pure Read
    "create" -> pure Create
    "update" -> pure Update
    "delete" -> pure Delete
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
  deriving
    (ToJSON, FromJSON)
    via PrefixedSnake "collectionPermission" CollectionPermissionWithExemption
instance Ord CollectionPermissionWithExemption where
  compare (CollectionPermissionWithExemption xp xe) (CollectionPermissionWithExemption yp ye) =
    case compare xp yp of
      EQ -> case (xe, ye) of
        (False, True) -> LT
        (True, False) -> GT
        _ -> EQ
      c -> c
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
    NonExistent -> "nonexistent"
    Exists -> "exists"
    Adjust -> "adjust"
    Obliterate -> "obliterate"
instance FromJSON SinglePermission where
  parseJSON json@(String x) = case x of
    "nonexistent" -> pure NonExistent
    "exists" -> pure Exists
    "adjust" -> pure Adjust
    "obliterate" -> pure Obliterate
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
