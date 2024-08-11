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
  , references
  , subscriptions
  ) where

import Lib.Types.Id (EntityId, VersionId)

import Data.Aeson (ToJSON, FromJSON)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Deriving.Aeson.Stock (PrefixedSnake, Generic, CustomJSON (..))
import Control.Lens.TH (makeLensesFor)


data Version = Version
  { versionEntity :: EntityId
  , versionPrev :: Maybe VersionId -- Only allow for back-linking to facilitate forks
  , versionReferences :: HashSet VersionId
  , versionSubscriptions :: HashSet EntityId
  } deriving (Eq, Generic, Show, Read)
  deriving (ToJSON, FromJSON)
  via PrefixedSnake "version" Version
makeLensesFor
  [ ("versionEntity", "entity")
  , ("versionPrev", "prevVersion")
  , ("versionReferences", "references")
  , ("versionSubscriptions", "subscriptions")
  ] ''Version

genesisVersion :: EntityId -> Version
genesisVersion eId = Version
  { versionEntity = eId
  , versionPrev = Nothing
  , versionReferences = mempty
  , versionSubscriptions = mempty
  }

forkVersion :: EntityId -> VersionId -> Version
forkVersion eId vId = Version
  { versionEntity = eId
  , versionPrev = Just vId
  , versionReferences = HS.singleton vId
  , versionSubscriptions = mempty
  }
