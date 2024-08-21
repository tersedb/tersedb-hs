{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Types.Store.Version (
  Version,
  initVersion,
  entity,
  references,
  subscriptions,
) where

import Lib.Types.Id (EntityId, VersionId)

import Control.Lens.TH (makeLensesFor)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Deriving.Aeson.Stock (CustomJSON (..), Generic, PrefixedSnake)

data Version = Version
  { versionEntity :: EntityId
  , versionReferences :: HashSet VersionId
  , versionSubscriptions :: HashSet EntityId
  }
  deriving (Eq, Generic, Show, Read)
  deriving
    (ToJSON, FromJSON)
    via PrefixedSnake "version" Version
makeLensesFor
  [ ("versionEntity", "entity")
  , ("versionReferences", "references")
  , ("versionSubscriptions", "subscriptions")
  ]
  ''Version

initVersion :: EntityId -> Version
initVersion eId =
  Version
    { versionEntity = eId
    , versionReferences = mempty
    , versionSubscriptions = mempty
    }
