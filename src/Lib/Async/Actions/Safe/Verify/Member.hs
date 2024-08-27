module Lib.Async.Actions.Safe.Verify.Member (
  anyCanReadMember,
  anyCanCreateMember,
  anyCanUpdateMember,
  anyCanDeleteMember,
  hasMemberPermission,
) where

import Control.Concurrent.STM (STM)
import Control.Lens ((^.))
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Extra (andM, anyM, orM)
import Control.Monad.Reader (MonadReader (ask))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Lib.Async.Actions.Safe.Verify.Group (canReadGroup)
import Lib.Async.Actions.Safe.Verify.Utils (canDo)
import Lib.Async.Types.Monad (TerseM)
import Lib.Async.Types.Store (temp, toTabOrganization, toTabOther)
import Lib.Async.Types.Tabulation (forMembers)
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (CollectionPermission (..), collectionPermission)
import qualified StmContainers.Map as Map

canReadMember :: ActorId -> GroupId -> TerseM STM Bool
canReadMember reader gId =
  andM
    [ canDo getPerm reader Read
    , canReadGroup reader gId
    ]
 where
  getPerm readerGId = do
    s <- ask
    liftBase $ do
      mTabOther <- Map.lookup readerGId (s ^. temp . toTabOther)
      case mTabOther of
        Nothing -> pure Blind
        Just tabOther -> fromMaybe Blind <$> Map.lookup gId (tabOther ^. forMembers)

anyCanReadMember :: NonEmpty ActorId -> GroupId -> TerseM STM Bool
anyCanReadMember readers gId =
  anyM (`canReadMember` gId) (NE.toList readers)

canCreateMember :: ActorId -> GroupId -> TerseM STM Bool
canCreateMember creater gId =
  andM
    [ canDo getPerm creater Create
    , canReadGroup creater gId
    ]
 where
  getPerm createrGId = do
    s <- ask
    liftBase $ do
      mTabOther <- Map.lookup createrGId (s ^. temp . toTabOther)
      case mTabOther of
        Nothing -> pure Blind
        Just tabOther -> fromMaybe Blind <$> Map.lookup gId (tabOther ^. forMembers)

anyCanCreateMember :: NonEmpty ActorId -> GroupId -> TerseM STM Bool
anyCanCreateMember creaters gId =
  anyM (`canCreateMember` gId) (NE.toList creaters)

canUpdateMember :: ActorId -> GroupId -> TerseM STM Bool
canUpdateMember updater gId =
  andM
    [ canDo getPerm updater Update
    , canReadGroup updater gId
    ]
 where
  getPerm updaterGId = do
    s <- ask
    liftBase $ do
      mTabOther <- Map.lookup updaterGId (s ^. temp . toTabOther)
      case mTabOther of
        Nothing -> pure Blind
        Just tabOther -> fromMaybe Blind <$> Map.lookup gId (tabOther ^. forMembers)

anyCanUpdateMember :: NonEmpty ActorId -> GroupId -> TerseM STM Bool
anyCanUpdateMember updaters gId =
  anyM (`canUpdateMember` gId) (NE.toList updaters)

canDeleteMember :: ActorId -> GroupId -> TerseM STM Bool
canDeleteMember deleter gId =
  andM
    [ canDo getPerm deleter Delete
    , canReadGroup deleter gId
    ]
 where
  getPerm deleterGId = do
    s <- ask
    liftBase $ do
      mTabOther <- Map.lookup deleterGId (s ^. temp . toTabOther)
      case mTabOther of
        Nothing -> pure Blind
        Just tabOther -> fromMaybe Blind <$> Map.lookup gId (tabOther ^. forMembers)

anyCanDeleteMember :: NonEmpty ActorId -> GroupId -> TerseM STM Bool
anyCanDeleteMember deleters gId =
  anyM (`canDeleteMember` gId) (NE.toList deleters)

hasMemberPermission
  :: ActorId -> GroupId -> CollectionPermission -> TerseM STM Bool
hasMemberPermission aId gId p =
  andM
    [ canReadGroup aId gId
    , orM
        [ let getPerm gId = do
                s <- ask
                liftBase $ do
                  mTabOther <- Map.lookup gId (s ^. temp . toTabOther)
                  case mTabOther of
                    Nothing -> pure Blind
                    Just tabOther -> fromMaybe Blind <$> Map.lookup gId (tabOther ^. forMembers)
           in canDo getPerm aId p
        , let getPerm gId = do
                s <- ask
                liftBase $
                  maybe Blind (^. collectionPermission)
                    <$> Map.lookup gId (s ^. temp . toTabOrganization)
           in canDo getPerm aId Update
        ]
    ]
