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

module Lib.Types.Store.Entity (Entity, space, versions, initEntity, addVersion) where

import Lib.Types.Id (SpaceId, VersionId)

import Data.Aeson (ToJSON, FromJSON)
import Deriving.Aeson.Stock (PrefixedSnake, Generic, CustomJSON (..))
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Control.Lens.TH (makeLensesFor)


data Entity = Entity
  { entitySpace :: SpaceId
  , entityVersions :: NonEmpty VersionId
  } deriving (Eq, Generic, Show, Read)
  deriving (ToJSON, FromJSON)
  via PrefixedSnake "entity" Entity
makeLensesFor
  [ ("entitySpace", "space")
  , ("entityVersions", "versions")
  ] ''Entity

initEntity :: SpaceId -> VersionId -> Entity
initEntity sId vId = Entity
  { entitySpace = sId
  , entityVersions = vId :| []
  }

addVersion :: Entity -> VersionId -> Entity
addVersion x vId = x { entityVersions = vId <| entityVersions x }
