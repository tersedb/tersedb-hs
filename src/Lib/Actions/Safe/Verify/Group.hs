module Lib.Actions.Safe.Verify.Group (
  anyCanReadGroup,
  anyCanCreateGroup,
  anyCanUpdateGroup,
  anyCanDeleteGroup,
  canReadGroup,
  hasGroupPermission,
) where

import Control.Lens ((^.))
import Control.Monad.Extra (anyM)
import Control.Monad.State (MonadState)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Lib.Actions.Safe.Verify.Utils (
  canDo,
  canDoWithTab,
  withCollectionPermission,
 )
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (
  CollectionPermission (..),
  SinglePermission,
  collectionPermission,
  escalate,
 )
import Lib.Types.Store (Shared)
import Lib.Types.Store.Tabulation.Group (forGroups, forOrganization)

canReadGroup :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canReadGroup reader gId = do
  canDo
    (withCollectionPermission gId forOrganization forGroups)
    reader
    Read

anyCanReadGroup :: (MonadState Shared m) => NonEmpty ActorId -> GroupId -> m Bool
anyCanReadGroup readers gId =
  anyM (`canReadGroup` gId) (NE.toList readers)

-- visibleGroups :: MonadState Shared m => ActorId -> m (HashSet GroupId)
--
-- hiddenGroups :: MonadState Shared m => ActorId -> m (HashSet GroupId)

canCreateGroup :: (MonadState Shared m) => ActorId -> m Bool
canCreateGroup creater =
  canDo
    (\t -> t ^. forOrganization . collectionPermission)
    creater
    Create

anyCanCreateGroup :: (MonadState Shared m) => NonEmpty ActorId -> m Bool
anyCanCreateGroup = anyM canCreateGroup . NE.toList

canUpdateGroup :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canUpdateGroup updater gId =
  canDo
    (withCollectionPermission gId forOrganization forGroups)
    updater
    Update

anyCanUpdateGroup
  :: (MonadState Shared m) => NonEmpty ActorId -> GroupId -> m Bool
anyCanUpdateGroup updaters gId =
  anyM (`canUpdateGroup` gId) (NE.toList updaters)

canDeleteGroup :: (MonadState Shared m) => ActorId -> GroupId -> m Bool
canDeleteGroup deleter gId =
  canDo
    (withCollectionPermission gId forOrganization forGroups)
    deleter
    Delete

anyCanDeleteGroup
  :: (MonadState Shared m) => NonEmpty ActorId -> GroupId -> m Bool
anyCanDeleteGroup deleters gId =
  anyM (`canDeleteGroup` gId) (NE.toList deleters)

hasGroupPermission
  :: (MonadState Shared m) => ActorId -> GroupId -> SinglePermission -> m Bool
hasGroupPermission aId gId p =
  canDoWithTab
    (withCollectionPermission gId forOrganization forGroups)
    aId
    (\t -> escalate (t ^. forOrganization) p)
