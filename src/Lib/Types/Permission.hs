{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  #-}

module Lib.Types.Permission where

import Data.Hashable (Hashable)
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Semigroup (Max (..))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary), elements)


data Permission
  = Blind
  | Read
  | Create
  | Update
  | Delete
  deriving (Eq, Ord, Show, Read, Generic)
instance Hashable Permission

instance ToJSON Permission where
  toJSON x = String $ case x of
    Blind -> "blind"
    Read -> "read"
    Create -> "create"
    Update -> "update"
    Delete -> "delete"

instance FromJSON Permission where
  parseJSON json@(String x) = case x of
    "blind" -> pure Blind
    "read" -> pure Read
    "create" -> pure Create
    "update" -> pure Update
    "delete" -> pure Delete
    _ -> typeMismatch "Permission" json
  parseJSON json = typeMismatch "Permission" json

instance Semigroup Permission where
  x <> y = getMax $ Max x <> Max y

instance Monoid Permission where
  mempty = Blind

instance Arbitrary Permission where
  arbitrary = elements [Blind, Read, Create, Update, Delete]
