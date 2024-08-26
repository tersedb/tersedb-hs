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

module Lib.Sync.Actions.Safe.Verify.Actor
  ( anyCanReadActor
  , anyCanCreateActor
  , anyCanUpdateActor
  , anyCanDeleteActor
  , hasRecruiterPermission
  ) where

import Control.Lens ((^.))
import Control.Monad.Extra (anyM, orM)
import Control.Monad.State (MonadState)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Lib.Sync.Actions.Safe.Verify.Utils (canDo)
import Lib.Sync.Types.Store (Shared)
import Lib.Sync.Types.Store.Tabulation.Group (forRecruiter)
import Lib.Types.Id (ActorId)
import Lib.Types.Permission (
  CollectionPermission (..),
 )

hasRecruiterPermission
  :: (MonadState Shared m) => ActorId -> CollectionPermission -> m Bool
hasRecruiterPermission aId p =
  canDo
    (^. forRecruiter)
    aId
    p

canReadActor :: (MonadState Shared m) => ActorId -> m Bool
canReadActor reader =
  canDo (^. forRecruiter) reader Read

anyCanReadActor :: (MonadState Shared m) => NonEmpty ActorId -> m Bool
anyCanReadActor = anyM canReadActor . NE.toList

canCreateActor :: (MonadState Shared m) => ActorId -> m Bool
canCreateActor creater =
  canDo (^. forRecruiter) creater Create

anyCanCreateActor :: (MonadState Shared m) => NonEmpty ActorId -> m Bool
anyCanCreateActor = anyM canCreateActor . NE.toList

canUpdateActor :: (MonadState Shared m) => ActorId -> ActorId -> m Bool
canUpdateActor updater aId =
  orM
    [ pure (updater == aId)
    , canDo (^. forRecruiter) updater Update
    ]

anyCanUpdateActor
  :: (MonadState Shared m) => NonEmpty ActorId -> ActorId -> m Bool
anyCanUpdateActor updaters aId =
  anyM (`canUpdateActor` aId) (NE.toList updaters)

canDeleteActor :: (MonadState Shared m) => ActorId -> ActorId -> m Bool
canDeleteActor deleter aId =
  orM
    [ pure (deleter == aId)
    , canDo (^. forRecruiter) deleter Delete
    ]

anyCanDeleteActor
  :: (MonadState Shared m) => NonEmpty ActorId -> ActorId -> m Bool
anyCanDeleteActor deleters aId =
  anyM (`canDeleteActor` aId) (NE.toList deleters)
