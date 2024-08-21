{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Types.Store.Entity (Entity, space, versions, fork, initEntity) where

import Lib.Types.Id (SpaceId, VersionId)

import Control.Lens.TH (makeLensesFor)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Deriving.Aeson.Stock (CustomJSON (..), Generic, PrefixedSnake)

data Entity = Entity
    { entitySpace :: SpaceId
    , entityVersions :: NonEmpty VersionId
    , entityFork :: Maybe VersionId
    }
    deriving (Eq, Generic, Show, Read)
    deriving
        (ToJSON, FromJSON)
        via PrefixedSnake "entity" Entity
makeLensesFor
    [ ("entitySpace", "space")
    , ("entityVersions", "versions")
    , ("entityFork", "fork")
    ]
    ''Entity

initEntity :: SpaceId -> VersionId -> Maybe VersionId -> Entity
initEntity sId vId fork =
    Entity
        { entitySpace = sId
        , entityVersions = vId :| []
        , entityFork = fork
        }

-- addVersion :: Entity -> VersionId -> Entity
-- addVersion x vId = x { entityVersions = vId <| entityVersions x }
