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

module Lib.Types.Store.Version 
  ( Version
  , genesisVersion
  , forkVersion
  , entity
  , prevVersion
  ) where

import Lib.Types.Id (EntityId, VersionId)

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


data Version = Version
  { versionEntity :: EntityId
  , versionPrev :: Maybe VersionId -- Only allow for back-linking to facilitate forks
  } deriving (Eq, Generic, Show, Read)
  deriving (ToJSON, FromJSON)
  via PrefixedSnake "version" Version
makeLensesFor
  [ ("versionEntity", "entity")
  , ("versionPrev", "prevVersion")
  ] ''Version

genesisVersion :: EntityId -> Version
genesisVersion eId = Version
  { versionEntity = eId
  , versionPrev = Nothing
  }

forkVersion :: EntityId -> VersionId -> Version
forkVersion eId vId = Version
  { versionEntity = eId
  , versionPrev = Just vId
  }
