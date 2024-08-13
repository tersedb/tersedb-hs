module Lib.Actions.Safe.Verify.Group where

import Lib.Actions.Safe.Verify.Utils (canDo, canDoWithTab)
import Lib.Types.Store (Shared, toActors, store, temp)
import Lib.Types.Store.Tabulation.Group (forOrganization, forGroups)
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption (..)
  , SinglePermission
  , escalate
  , collectionPermission
  )

import Data.Maybe (fromMaybe, isNothing, isJust)
import qualified Data.HashSet as HS
import Control.Lens ((^.), at, ix)
import Control.Monad.State (MonadState, get)
import Control.Monad.Extra (anyM)

-- canReadGroup :: MonadState Shared m => ActorId -> GroupId -> m Bool
-- canReadGroup reader gId = do
--   s <- get
--   case s ^. store . toActors . at reader of
--     Nothing -> pure False
--     Just gs -> flip anyM (HS.toList gs) $ \gId' ->
--       if isJust (s ^. temp . toGroupsHiddenTo . ix gId . at gId')
--       then canDo
--             (\t -> t ^. forOrganization)
--             reader
--             (CollectionPermissionWithExemption Read True)
--       else pure True

canReadGroup :: MonadState Shared m => ActorId -> GroupId -> m Bool
canReadGroup reader gId = do
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
