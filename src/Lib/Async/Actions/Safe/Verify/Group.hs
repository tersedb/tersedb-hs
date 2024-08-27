module Lib.Async.Actions.Safe.Verify.Group (
  anyCanReadGroup,
  canReadGroup,
  anyCanCreateGroup,
  anyCanUpdateGroup,
  anyCanDeleteGroup,
  hasOrganizationPermission,
  hasGroupPermission,
) where

import Control.Concurrent.STM (STM)
import Control.Lens ((^.))
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Extra (anyM)
import Control.Monad.Reader (MonadReader (ask))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Lib.Actions.Safe.Utils (deriveCollectionPermission)
import Lib.Async.Actions.Safe.Verify.Utils (canDo, canDoWithTab)
import Lib.Async.Types.Monad (TerseM)
import Lib.Async.Types.Store (temp, toTabOrganization, toTabOther)
import Lib.Async.Types.Tabulation (forGroups)
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption,
  SinglePermission,
  collectionPermission,
  escalate,
 )
import qualified StmContainers.Map as Map

hasOrganizationPermission
  :: ActorId -> CollectionPermissionWithExemption -> TerseM STM Bool
hasOrganizationPermission aId p =
  canDo getCheckedPerm aId p
 where
  getCheckedPerm gId = do
    s <- ask
    liftBase $ fromMaybe minBound <$> Map.lookup gId (s ^. temp . toTabOrganization)

canReadGroup :: ActorId -> GroupId -> TerseM STM Bool
canReadGroup reader gId =
  canDo getPerm reader Read
 where
  getPerm readerGId = do
    s <- ask
    liftBase $ do
      major <-
        fromMaybe minBound <$> Map.lookup readerGId (s ^. temp . toTabOrganization)
      minor <- do
        mTabOther <- Map.lookup readerGId (s ^. temp . toTabOther)
        case mTabOther of
          Nothing -> pure Nothing
          Just tabOther -> Map.lookup gId (tabOther ^. forGroups)
      pure (deriveCollectionPermission major minor)

anyCanReadGroup :: NonEmpty ActorId -> GroupId -> TerseM STM Bool
anyCanReadGroup readers gId = anyM (`canReadGroup` gId) (NE.toList readers)

canCreateGroup :: ActorId -> TerseM STM Bool
canCreateGroup creater =
  canDo getPerm creater Create
 where
  getPerm createrGId = do
    s <- ask
    liftBase $
      maybe Blind (^. collectionPermission)
        <$> Map.lookup createrGId (s ^. temp . toTabOrganization)

anyCanCreateGroup :: NonEmpty ActorId -> TerseM STM Bool
anyCanCreateGroup = anyM canCreateGroup . NE.toList

canUpdateGroup :: ActorId -> GroupId -> TerseM STM Bool
canUpdateGroup updater gId =
  canDo getPerm updater Update
 where
  getPerm updaterGId = do
    s <- ask
    liftBase $ do
      major <-
        fromMaybe minBound <$> Map.lookup updaterGId (s ^. temp . toTabOrganization)
      minor <- do
        mTabOther <- Map.lookup updaterGId (s ^. temp . toTabOther)
        case mTabOther of
          Nothing -> pure Nothing
          Just tabOther -> Map.lookup gId (tabOther ^. forGroups)
      pure (deriveCollectionPermission major minor)

anyCanUpdateGroup :: NonEmpty ActorId -> GroupId -> TerseM STM Bool
anyCanUpdateGroup updaters gId = anyM (`canUpdateGroup` gId) (NE.toList updaters)

canDeleteGroup :: ActorId -> GroupId -> TerseM STM Bool
canDeleteGroup deleter gId =
  canDo getPerm deleter Delete
 where
  getPerm deleterGId = do
    s <- ask
    liftBase $ do
      major <-
        fromMaybe minBound <$> Map.lookup deleterGId (s ^. temp . toTabOrganization)
      minor <- do
        mTabOther <- Map.lookup deleterGId (s ^. temp . toTabOther)
        case mTabOther of
          Nothing -> pure Nothing
          Just tabOther -> Map.lookup gId (tabOther ^. forGroups)
      pure (deriveCollectionPermission major minor)

anyCanDeleteGroup :: NonEmpty ActorId -> GroupId -> TerseM STM Bool
anyCanDeleteGroup deleters gId = anyM (`canDeleteGroup` gId) (NE.toList deleters)

hasGroupPermission :: ActorId -> GroupId -> SinglePermission -> TerseM STM Bool
hasGroupPermission aId sId p =
  canDoWithTab getCheckedPerm aId getRefPerm
 where
  getCheckedPerm gId = do
    s <- ask
    liftBase $ do
      major <- fromMaybe minBound <$> Map.lookup gId (s ^. temp . toTabOrganization)
      minor <- do
        mTabOther <- Map.lookup gId (s ^. temp . toTabOther)
        case mTabOther of
          Nothing -> pure Nothing
          Just tabOther -> Map.lookup sId (tabOther ^. forGroups)
      pure (deriveCollectionPermission major minor)

  getRefPerm gId = do
    s <- ask
    liftBase $ do
      major <- fromMaybe minBound <$> Map.lookup gId (s ^. temp . toTabOrganization)
      pure (escalate major p)
