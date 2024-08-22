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


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Actions.Unsafe where

import Lib.Types.Store (
  Shared (..),
  Store (..),
  emptyTemp,
 )
import Lib.Types.Store.Groups (emptyGroups)

-- Note that this doesn't grant any initial "admin" actor
unsafeEmptyShared :: Shared
unsafeEmptyShared =
  Shared
    { sharedStore =
        Store
          { storeGroups = emptyGroups
          , storeActors = mempty
          , storeSpaces = mempty
          , storeEntities = mempty
          , storeVersions = mempty
          , storeSpacePermissions = mempty
          , storeEntityPermissions = mempty
          , storeGroupPermissions = mempty
          , storeMemberPermissions = mempty
          }
    , sharedTemp = emptyTemp
    }
