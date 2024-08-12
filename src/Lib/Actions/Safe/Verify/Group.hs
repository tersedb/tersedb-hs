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

module Lib.Actions.Safe.Verify.Group where

import Lib.Actions.Safe.Verify.Utils (canDo, canDoWithTab)
import Lib.Types.Store (Shared)
import Lib.Types.Store.Tabulation.Group (forOrganization, forGroups)
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , SinglePermission (Exists)
  , escalate
  , collectionPermission
  )

import Data.Maybe (fromMaybe)
import Control.Lens ((^.), at)
import Control.Monad.State (MonadState)

canReadGroup :: MonadState Shared m => ActorId -> GroupId -> m Bool
canReadGroup reader gId =
  canDo
    (\t -> fromMaybe (t ^. forOrganization . collectionPermission) (t ^. forGroups . at gId))
    reader
    Read

-- visibleGroups :: MonadState Shared m => ActorId -> m (HashSet GroupId)
--
-- hiddenGroups :: MonadState Shared m => ActorId -> m (HashSet GroupId)

canCreateGroup :: MonadState Shared m => ActorId -> m Bool
canCreateGroup creater =
  canDo
    (\t -> t ^. forOrganization . collectionPermission)
    creater
    Create

canUpdateGroup :: MonadState Shared m => ActorId -> GroupId -> m Bool
canUpdateGroup updater gId =
  canDo
    (\t -> fromMaybe (t ^. forOrganization . collectionPermission) (t ^. forGroups . at gId))
    updater
    Update

canDeleteGroup :: MonadState Shared m => ActorId -> GroupId -> m Bool
canDeleteGroup deleter gId =
  canDo
    (\t -> fromMaybe (t ^. forOrganization . collectionPermission) (t ^. forGroups . at gId))
    deleter
    Delete

hasGroupPermission :: MonadState Shared m => ActorId -> GroupId -> SinglePermission -> m Bool
hasGroupPermission aId gId p =
  canDoWithTab
    (\t -> fromMaybe (t ^. forOrganization . collectionPermission) (t ^. forGroups . at gId))
    aId
    (\t -> escalate (t ^. forOrganization) p)
