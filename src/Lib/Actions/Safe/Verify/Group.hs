module Lib.Actions.Safe.Verify.Group where

import Lib.Actions.Safe.Verify.Utils (canDo, canDoWithTab)
import Lib.Types.Store (Shared, toActors, toGroupsHiddenTo, store, temp)
import Lib.Types.Store.Tabulation.Group (forOrganization, forGroups)
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , SinglePermission
  , escalate
  , collectionPermission
  )

import Data.Maybe (fromMaybe, isNothing)
import Control.Lens ((^.), at, ix)
import Control.Monad.State (MonadState, get)

canReadGroup :: MonadState Shared m => ActorId -> GroupId -> m Bool
canReadGroup reader gId = do
  s <- get
  pure $ case s ^. store . toActors . at reader of
    Nothing -> False
    Just gs -> any (\gId' -> isNothing $ s ^. temp . toGroupsHiddenTo . ix gId . at gId') gs

canReadGroupOld :: MonadState Shared m => ActorId -> GroupId -> m Bool
canReadGroupOld reader gId = do
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
