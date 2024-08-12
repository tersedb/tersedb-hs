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

module Lib.Actions.Safe.Update.Group where

import Lib.Actions.Safe.Verify (conditionally, canUpdateGroup)
import Lib.Actions.Safe.Verify.SpaceAndEntity (hasSpacePermission, hasEntityPermission)
import Lib.Actions.Safe.Verify.Group (hasGroupPermission)
import Lib.Actions.Safe.Verify.Member (hasMemberPermission)
import Lib.Actions.Safe.Verify.Utils (canDo)
import Lib.Actions.Unsafe.Update.Group
  ( unsafeAdjustUniversePermission
  , unsafeAdjustOrganizationPermission
  , unsafeAdjustRecruiterPermission
  , unsafeAdjustGroupPermission
  , unsafeAdjustSpacePermission
  , unsafeAdjustEntityPermission
  , unsafeAdjustMemberPermission
  )
import Lib.Types.Id (GroupId, SpaceId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption (..)
  , SinglePermission (..)
  , escalate
  , collectionPermission
  )
import Lib.Types.Store (Shared)
import Lib.Types.Store.Tabulation.Group
  ( forUniverse
  , forOrganization
  , forRecruiter
  , forSpaces
  , forEntities
  , forGroups
  , forMembers
  )

import Data.Maybe (fromMaybe)
import Control.Lens ((^.), at, non)
import Control.Monad.State (MonadState)
import Control.Monad.Extra (andM, orM)

-- | Will only update the group if the actor has same or greater permission
setUniversePermission
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermissionWithExemption -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> m Bool
setUniversePermission creator p gId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , canDo (\t -> t ^. forUniverse) creator p
    ]
  conditionally
    (unsafeAdjustUniversePermission (const p) gId)
    canAdjust

setOrganizationPermission 
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermissionWithExemption -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> m Bool
setOrganizationPermission creator p gId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , canDo (\t -> t ^. forOrganization) creator p
    ]
  conditionally
    (unsafeAdjustOrganizationPermission (const p) gId)
    canAdjust

setRecruiterPermission 
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermission -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> m Bool
setRecruiterPermission creator p gId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , canDo (\t -> t ^. forRecruiter) creator p
    ]
  conditionally
    (unsafeAdjustRecruiterPermission (const p) gId)
    canAdjust

setSpacePermission 
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> Maybe SinglePermission -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> SpaceId -- ^ relevant to this space
  -> m Bool
setSpacePermission creator p gId sId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , hasSpacePermission creator sId (fromMaybe Exists p)
    ]
  conditionally
    (unsafeAdjustSpacePermission (const p) gId sId)
    canAdjust

setEntityPermission 
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermission -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> SpaceId -- ^ relevant to this space
  -> m Bool
setEntityPermission creator p gId sId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , hasEntityPermission creator sId p
    ]
  conditionally
    (unsafeAdjustEntityPermission (const p) gId sId)
    canAdjust

setGroupPermission 
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> Maybe SinglePermission -- ^ permission being set
  -> GroupId -- ^ group subject to new permission
  -> GroupId -- ^ relevant to this group (grants one group to NEAO other groups)
  -> m Bool
setGroupPermission creator p gId towardGId = do
  canAdjust <- andM
    [ canUpdateGroup creator gId
    , hasGroupPermission creator towardGId (fromMaybe Exists p)
    ]
  conditionally
    (unsafeAdjustGroupPermission (const p) gId towardGId)
    canAdjust

setMemberPermission 
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to set permission
  -> CollectionPermission -- ^ the permission being granted
  -> GroupId -- ^ the group gaining the permission
  -> GroupId -- ^ the group that can have their members manipulated
  -> m Bool
setMemberPermission creator p manipulatorGId manipulatedGId = do
  canAdjust <- andM
    [ canUpdateGroup creator manipulatorGId
    , hasMemberPermission creator manipulatedGId p
    ]
  conditionally
    (unsafeAdjustMemberPermission (const p) manipulatorGId manipulatedGId)
    canAdjust
