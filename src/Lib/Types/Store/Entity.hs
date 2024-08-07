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

module Lib.Types.Store.Entity where

import Lib.Types.Id (SpaceId)

import Data.Aeson (ToJSON, FromJSON)
import Deriving.Aeson.Stock (PrefixedSnake, Generic, CustomJSON (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Monoid (First (..))
import Data.List (uncons)
import Control.Lens ((^.))
import Control.Lens.TH (makeLensesFor)
import Control.Monad.State (MonadState (get, put), modify, evalState)
import Control.Monad.Extra (mconcatMapM)


data Entity = Entity
  { entitySpace :: SpaceId
  } deriving (Eq, Generic, Show, Read)
  deriving (ToJSON, FromJSON)
  via PrefixedSnake "entity" Entity
makeLensesFor
  [ ("entitySpace", "space")
  ] ''Entity
