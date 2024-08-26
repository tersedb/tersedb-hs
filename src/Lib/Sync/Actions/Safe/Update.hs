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

module Lib.Sync.Actions.Safe.Update where

import Control.Monad.Extra (allM, andM)
import Control.Monad.State (MonadState)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty)
import Lib.Sync.Actions.Safe.Verify (
  anyCanReadEntity,
  anyCanReadVersion,
  anyCanUpdateVersion,
  conditionally,
 )
import Lib.Sync.Actions.Unsafe.Update (
  unsafeUpdateVersionReferences,
  unsafeUpdateVersionSubscriptions,
 )
import Lib.Sync.Types.Store (
  Shared,
 )
import Lib.Types.Id (ActorId, EntityId, SpaceId, VersionId)

updateVersionReferences
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> VersionId
  -> HashSet VersionId
  -> m Bool
updateVersionReferences updater vId refIds = do
  canAdjust <-
    andM
      [ allM (anyCanReadVersion updater) (HS.toList refIds)
      , anyCanUpdateVersion updater vId
      ]
  conditionally (unsafeUpdateVersionReferences vId refIds) canAdjust

updateVersionSubscriptions
  :: (MonadState Shared m)
  => NonEmpty ActorId
  -> VersionId
  -> HashSet EntityId
  -> m Bool
updateVersionSubscriptions updater vId subIds = do
  canAdjust <-
    andM
      [ allM (anyCanReadEntity updater) (HS.toList subIds)
      , anyCanUpdateVersion updater vId
      ]
  conditionally (unsafeUpdateVersionSubscriptions vId subIds) canAdjust
