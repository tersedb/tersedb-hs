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

module Lib.Types.Store.Entity (Entity, space, versions, fork, initEntity) where

import Lib.Types.Id (SpaceId, VersionId)

import Data.Aeson (ToJSON, FromJSON)
import Deriving.Aeson.Stock (PrefixedSnake, Generic, CustomJSON (..))
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Control.Lens.TH (makeLensesFor)


data Entity = Entity
  { entitySpace :: SpaceId
  , entityVersions :: NonEmpty VersionId
  , entityFork :: Maybe VersionId
  } deriving (Eq, Generic, Show, Read)
  deriving (ToJSON, FromJSON)
  via PrefixedSnake "entity" Entity
makeLensesFor
  [ ("entitySpace", "space")
  , ("entityVersions", "versions")
  , ("entityFork", "fork")
  ] ''Entity

initEntity :: SpaceId -> VersionId -> Maybe VersionId -> Entity
initEntity sId vId fork = Entity
  { entitySpace = sId
  , entityVersions = vId :| []
  , entityFork = fork
  }

-- addVersion :: Entity -> VersionId -> Entity
-- addVersion x vId = x { entityVersions = vId <| entityVersions x }
