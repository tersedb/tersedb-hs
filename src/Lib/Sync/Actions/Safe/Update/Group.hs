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

module Lib.Sync.Actions.Safe.Update.Group where

import Control.Lens (ix, (^?), _Just)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Extra (andM)
import Control.Monad.State (MonadState, get)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty)
import Lib.Sync.Actions.Safe.Verify (anyCanUpdateGroup, conditionally)
import Lib.Sync.Actions.Unsafe.Update.Group (
  unsafeUpdateGroupChildren,
  unsafeUpdateGroupParent,
 )
import Lib.Sync.Types.Store (Shared, store, toGroups)
import Lib.Sync.Types.Store.Groups (next, nodes, prev)
import Lib.Types.Id (ActorId, GroupId)

updateGroupChildren
  :: (MonadState Shared m, MonadThrow m)
  => NonEmpty ActorId
  -> GroupId
  -> HashSet GroupId
  -> m Bool
updateGroupChildren updater gId newChildren = do
  s <- get
  case s ^? store . toGroups . nodes . ix gId . next of
    Nothing -> pure False
    Just oldChildren -> do
      canAdjust <-
        andM $
          anyCanUpdateGroup updater gId
            : map (anyCanUpdateGroup updater) (HS.toList (newChildren <> oldChildren))
      conditionally (unsafeUpdateGroupChildren gId newChildren) canAdjust
