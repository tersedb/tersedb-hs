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

module Lib.Sync.Actions.Safe.Verify.Group (
  anyCanReadGroup,
  anyCanCreateGroup,
  anyCanUpdateGroup,
  anyCanDeleteGroup,
  canReadGroup,
  hasGroupPermission,
) where

import Control.Lens ((^.))
import Control.Monad.Extra (anyM)
import Control.Monad.State (MonadState)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Lib.Sync.Actions.Safe.Verify.Utils (
  canDo,
  canDoWithTab,
  withCollectionPermission,
 )
import Lib.Sync.Types.Store (Shared)
import Lib.Sync.Types.Store.Tabulation.Group (forGroups, forOrganization)
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (
  CollectionPermission (..),
  SinglePermission,
  collectionPermission,
  escalate,
 )

canReadGroup :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canReadGroup reader gId = do
  canDo
    (withCollectionPermission gId forOrganization forGroups)
    reader
    Read

anyCanReadGroup
  :: (MonadState Shared m) => NonEmpty ActorId -> GroupId -> m Bool
anyCanReadGroup readers gId =
  anyM (`canReadGroup` gId) (NE.toList readers)

-- visibleGroups :: MonadState Shared m => ActorId -> m (HashSet GroupId)
--
-- hiddenGroups :: MonadState Shared m => ActorId -> m (HashSet GroupId)

canCreateGroup :: (MonadState Shared m) => ActorId -> m Bool
canCreateGroup creater =
  canDo
    (\t -> t ^. forOrganization . collectionPermission)
    creater
    Create

anyCanCreateGroup :: (MonadState Shared m) => NonEmpty ActorId -> m Bool
anyCanCreateGroup = anyM canCreateGroup . NE.toList

canUpdateGroup :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canUpdateGroup updater gId =
  canDo
    (withCollectionPermission gId forOrganization forGroups)
    updater
    Update

anyCanUpdateGroup
  :: (MonadState Shared m) => NonEmpty ActorId -> GroupId -> m Bool
anyCanUpdateGroup updaters gId =
  anyM (`canUpdateGroup` gId) (NE.toList updaters)

canDeleteGroup :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canDeleteGroup deleter gId =
  canDo
    (withCollectionPermission gId forOrganization forGroups)
    deleter
    Delete

anyCanDeleteGroup
  :: (MonadState Shared m) => NonEmpty ActorId -> GroupId -> m Bool
anyCanDeleteGroup deleters gId =
  anyM (`canDeleteGroup` gId) (NE.toList deleters)

hasGroupPermission
  :: (MonadState Shared m) => ActorId -> GroupId -> SinglePermission -> m Bool
hasGroupPermission aId gId p =
  canDoWithTab
    (withCollectionPermission gId forOrganization forGroups)
    aId
    (\t -> escalate (t ^. forOrganization) p)
