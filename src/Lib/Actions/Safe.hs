{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
  , DerivingVia
  , DataKinds
  , DeriveGeneric
  , RankNTypes
  , TemplateHaskell
  , FlexibleContexts
  #-}

module Lib.Actions.Safe where

import Lib.Actions.Tabulation (updateTabulationStartingAt)
import Lib.Actions.Unsafe (unsafeEmptyShared)
import Lib.Actions.Unsafe.Store
  ( unsafeStoreGroup
  , unsafeStoreActor
  , unsafeAddMember
  )
import Lib.Actions.Unsafe.Update.Group
  ( unsafeAdjustUniversePermission
  , unsafeAdjustOrganizationPermission
  , unsafeAdjustRecruiterPermission
  )
import Lib.Types.Id (GroupId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption (..)
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

