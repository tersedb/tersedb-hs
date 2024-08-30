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

module Lib.Sync.Actions.Safe where

import Lib.Sync.Actions.Tabulation (updateTabulationStartingAt)
import Lib.Sync.Actions.Unsafe (unsafeEmptyShared)
import Lib.Sync.Actions.Unsafe.Store (
  unsafeAddMember,
  unsafeStoreActor,
  unsafeStoreGroup,
 )
import Lib.Sync.Actions.Unsafe.Update.Group (
  unsafeAdjustOrganizationPermission,
  unsafeAdjustRecruiterPermission,
  unsafeAdjustUniversePermission,
 )
import Lib.Sync.Types.Store (Shared)
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption (..),
 )

import Control.Monad.State (execState)

emptyShared :: ActorId -> GroupId -> Shared
emptyShared adminActor adminGroup = flip execState unsafeEmptyShared $ do
  unsafeStoreGroup adminGroup
  unsafeAdjustUniversePermission
    (const $ CollectionPermissionWithExemption Delete True)
    adminGroup
  unsafeAdjustOrganizationPermission
    (const $ CollectionPermissionWithExemption Delete True)
    adminGroup
  unsafeAdjustRecruiterPermission (const Delete) adminGroup
  unsafeStoreActor adminActor
  unsafeAddMember adminGroup adminActor
  updateTabulationStartingAt adminGroup
