module Lib.Actions.Safe.Verify.Group where

import Lib.Actions.Safe.Verify.Utils (canDo, canDoWithTab, withCollectionPermission)
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (
    CollectionPermission (..),
    CollectionPermissionWithExemption (..),
    SinglePermission,
    collectionPermission,
    escalate,
 )
import Lib.Types.Store (Shared, store, temp, toActors)
import Lib.Types.Store.Tabulation.Group (forGroups, forOrganization)

import Control.Lens (at, ix, (^.))
import Control.Monad.Extra (anyM)
import Control.Monad.State (MonadState, get)
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe, isJust, isNothing)

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

canReadGroup :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canReadGroup reader gId = do
    canDo
        (withCollectionPermission gId forOrganization forGroups)
        reader
        Read

-- visibleGroups :: MonadState Shared m => ActorId -> m (HashSet GroupId)
--
-- hiddenGroups :: MonadState Shared m => ActorId -> m (HashSet GroupId)

canCreateGroup :: (MonadState Shared m) => ActorId -> m Bool
canCreateGroup creater =
    canDo
        (\t -> t ^. forOrganization . collectionPermission)
        creater
        Create

canUpdateGroup :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canUpdateGroup updater gId =
    canDo
        (withCollectionPermission gId forOrganization forGroups)
        updater
        Update

canDeleteGroup :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canDeleteGroup deleter gId =
    canDo
        (withCollectionPermission gId forOrganization forGroups)
        deleter
        Delete

hasGroupPermission :: (MonadState Shared m) => ActorId -> GroupId -> SinglePermission -> m Bool
hasGroupPermission aId gId p =
    canDoWithTab
        (withCollectionPermission gId forOrganization forGroups)
        aId
        (\t -> escalate (t ^. forOrganization) p)
