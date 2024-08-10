{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
  , DerivingVia
  , DataKinds
  , DeriveGeneric
  , RankNTypes
  , TemplateHaskell
  , FlexibleContexts
  #-}

module Lib.Types.Store.Space where

import Lib.Types.Id (EntityId)

import Data.Aeson (ToJSON, FromJSON)
import Deriving.Aeson.Stock (PrefixedSnake, Generic, CustomJSON (..))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Control.Lens.TH (makeLensesFor)


data Space = Space
  { spaceEntities :: HashSet EntityId
  } deriving (Eq, Generic, Show, Read)
  deriving (ToJSON, FromJSON)
  via PrefixedSnake "space" Space
makeLensesFor
  [ ("spaceEntities", "entities")
  ] ''Space

instance Semigroup Space where
  (Space xs) <> (Space ys) = Space (xs `HS.union` ys)
instance Monoid Space where
  mempty = Space mempty
