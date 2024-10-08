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

module Lib.Sync.Types.Store.Version (
  Version,
  initVersion,
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
  { versionReferences :: HashSet VersionId
  , versionSubscriptions :: HashSet EntityId
  }
  deriving (Eq, Generic, Show, Read)
  deriving
    (ToJSON, FromJSON)
    via PrefixedSnake "version" Version
makeLensesFor
  [ ("versionReferences", "references")
  , ("versionSubscriptions", "subscriptions")
  ]
  ''Version

initVersion :: Version
initVersion =
  Version
    { versionReferences = mempty
    , versionSubscriptions = mempty
    }
