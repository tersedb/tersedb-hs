{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Actions.Safe where

import Lib.Actions.Tabulation (updateTabulationStartingAt)
import Lib.Actions.Unsafe (unsafeEmptyShared)
import Lib.Actions.Unsafe.Store (
    unsafeAddMember,
    unsafeStoreActor,
    unsafeStoreGroup,
 )
import Lib.Actions.Unsafe.Update.Group (
    unsafeAdjustOrganizationPermission,
    unsafeAdjustRecruiterPermission,
    unsafeAdjustUniversePermission,
 )
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (
    CollectionPermission (..),
    CollectionPermissionWithExemption (..),
 )
import Lib.Types.Store (Shared)

import Control.Monad.State (execState)

emptyShared :: ActorId -> GroupId -> Shared
emptyShared adminActor adminGroup = flip execState unsafeEmptyShared $ do
    unsafeStoreGroup adminGroup
    unsafeAdjustUniversePermission (const $ CollectionPermissionWithExemption Delete True) adminGroup
    unsafeAdjustOrganizationPermission (const $ CollectionPermissionWithExemption Delete True) adminGroup
    unsafeAdjustRecruiterPermission (const Delete) adminGroup
    unsafeStoreActor adminActor
    unsafeAddMember adminGroup adminActor
    updateTabulationStartingAt adminGroup
