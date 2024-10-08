{-
TerseDB - Entity Management System
Copyright (C) 2024  Athan Clark

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

You can reach me at athan.clark@gmail.com.
-}

module Lib.Sync.Types.Store.Entity (Entity, versions, fork, initEntity) where

import Lib.Types.Id (VersionId)

import Control.Lens.TH (makeLensesFor)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Deriving.Aeson.Stock (CustomJSON (..), Generic, PrefixedSnake)

data Entity = Entity
  { entityVersions :: NonEmpty VersionId
  , entityFork :: Maybe VersionId
  }
  deriving (Eq, Generic, Show, Read)
  deriving
    (ToJSON, FromJSON)
    via PrefixedSnake "entity" Entity
makeLensesFor
  [ ("entityVersions", "versions")
  , ("entityFork", "fork")
  ]
  ''Entity

initEntity :: VersionId -> Maybe VersionId -> Entity
initEntity vId fork =
  Entity
    { entityVersions = vId :| []
    , entityFork = fork
    }
