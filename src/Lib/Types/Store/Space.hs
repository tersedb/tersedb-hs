{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Types.Store.Space where

import Lib.Types.Id (EntityId)

import Control.Lens.TH (makeLensesFor)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Deriving.Aeson.Stock (CustomJSON (..), Generic, PrefixedSnake)

data Space = Space
    { spaceEntities :: HashSet EntityId
    }
    deriving (Eq, Generic, Show, Read)
    deriving
        (ToJSON, FromJSON)
        via PrefixedSnake "space" Space
makeLensesFor
    [ ("spaceEntities", "entities")
    ]
    ''Space

instance Semigroup Space where
    (Space xs) <> (Space ys) = Space (xs `HS.union` ys)
instance Monoid Space where
    mempty = Space mempty
