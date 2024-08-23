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

module Lib.Sync.Actions.Safe.Verify.Utils where

import Lib.Types.Id (ActorId)
import Lib.Types.Permission (
  CollectionPermission,
  CollectionPermissionWithExemption,
  collectionPermission,
  HasMinimumPermission (..),
 )
import Lib.Sync.Types.Store (
  Shared,
  temp,
  toMemberOf,
  toTabulatedGroups,
 )
import Lib.Sync.Types.Store.Tabulation.Group (
  TabulatedPermissionsForGroup (..),
 )

import Control.Lens (Lens', at, non, (^.))
import Control.Monad.Extra (when)
import Control.Monad.State (MonadState (get))
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)

{- | Looks first for the groups the user is in, then sees if any of the groups
can do the action, depicted by the Lens
-}
canDoWithTab
  :: ( MonadState Shared m
     , HasMinimumPermission a
     )
  => (TabulatedPermissionsForGroup -> a)
  -- ^ Specific permission being checked
  -> ActorId
  -- ^ Actor requesting permission
  -> (TabulatedPermissionsForGroup -> a)
  -- ^ Minimum permission actor needs
  -> m Bool
canDoWithTab proj creator getP = do
  s <- get
  pure $ case s ^. temp . toMemberOf . at creator of
    Just groups ->
      let perGroup gId =
            let tab = s ^. temp . toTabulatedGroups . at gId . non mempty
             in proj tab `hasMinimumPermission` getP tab
       in any perGroup groups
    _ -> False

canDo
  :: ( MonadState Shared m
     , HasMinimumPermission a
     )
  => (TabulatedPermissionsForGroup -> a)
  -- ^ Specific permission being checked
  -> ActorId
  -- ^ Actor requesting permission
  -> a
  -- ^ Minimum permission actor needs
  -> m Bool
canDo a b c = canDoWithTab a b (const c)

conditionally :: (Applicative m) => m () -> Bool -> m Bool
conditionally f t = t <$ when t f

withCollectionPermission
  :: (Hashable a)
  => a
  -> Lens' TabulatedPermissionsForGroup CollectionPermissionWithExemption
  -> Lens' TabulatedPermissionsForGroup (HashMap a CollectionPermission)
  -> TabulatedPermissionsForGroup
  -> CollectionPermission
withCollectionPermission xId projMajor projMinor t =
  let maj = t ^. projMajor . collectionPermission
   in maybe maj (\p -> p `min` maj) (t ^. projMinor . at xId)
