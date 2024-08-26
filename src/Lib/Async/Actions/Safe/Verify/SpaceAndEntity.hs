module Lib.Async.Actions.Safe.Verify.SpaceAndEntity
  ( anyCanReadSpace
  , anyCanReadSpaceOld
  , anyCanCreateSpace
  , anyCanUpdateSpace
  , anyCanDeleteSpace
  , hasSpacePermission
  , anyCanReadAllEntities
  , anyCanReadEntity
  , anyCanCreateEntity
  , anyCanUpdateAllEntities
  , anyCanUpdateEntity
  , anyCanDeleteEntity
  , hasEntityPermission
  , anyCanReadVersion
  , anyCanCreateVersion
  , anyCanUpdateVersion
  , anyCanDeleteVersion
  , anyCanDeleteVersionAndEntity
  ) where

import Lib.Types.Id (ActorId, SpaceId, GroupId, EntityId, VersionId)
import Lib.Async.Types.Monad (TerseM)
import Control.Concurrent.STM (STM)
import ListT (foldMaybe)
import qualified StmContainers.Multimap as Multimap
import Lib.Async.Types.Store (toSpacesHiddenTo, temp, toMemberOf, toTabUniverse, toTabOther, toSpaceEntities, store, toForksFrom, toEntities, toReferencesFrom, toSubscriptionsFrom, toSpaceOf, toEntityOf)
import Lib.Types.Permission (CollectionPermissionWithExemption (..), CollectionPermission (..), collectionPermission, SinglePermission, escalate)
import Lib.Async.Actions.Safe.Verify.Utils (canDo, canDoWithTab)
import Control.Lens ((^.))
import Control.Monad.Base (MonadBase(liftBase))
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad.Trans.Control (MonadBaseControl(liftBaseWith))
import qualified StmContainers.Map as Map
import Control.Monad.Reader (MonadReader(ask))
import Data.List.NonEmpty (NonEmpty)
import Control.Monad.Extra (anyM, andM, allM, orM)
import qualified Data.List.NonEmpty as NE
import Lib.Async.Types.Tabulation (forSpaces, forEntities)
import Lib.Actions.Safe.Utils (deriveCollectionPermission)
import DeferredFolds.UnfoldlM (forM_)

canReadSpace :: ActorId -> SpaceId -> TerseM STM Bool
canReadSpace reader sId = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    let go :: Bool -> GroupId -> STM (Maybe Bool)
        go True _ = pure Nothing
        go False gId = do
          isHidden <- Multimap.lookup gId sId (s ^. temp . toSpacesHiddenTo)
          if isHidden
            then Just <$> runInBase (canDo getPerm reader (CollectionPermissionWithExemption Read True))
            else pure (Just True)
          where
            getPerm readerGId =
              liftBase $ fromMaybe minBound <$> Map.lookup readerGId (s ^. temp . toTabUniverse)
    foldMaybe go False (Multimap.listTByKey reader (s ^. temp . toMemberOf))

anyCanReadSpace :: NonEmpty ActorId -> SpaceId -> TerseM STM Bool
anyCanReadSpace readers sId = anyM (`canReadSpace` sId) (NE.toList readers)

canReadSpaceOld :: ActorId -> SpaceId -> TerseM STM Bool
canReadSpaceOld reader sId =
  canDo getPerm reader Read
  where
    getPerm readerGId = do
      s <- ask
      liftBase $ do
        mTabOther <- Map.lookup readerGId (s ^. temp . toTabOther)
        case mTabOther of
          Nothing -> pure Blind
          Just tabOther -> do
            major <- fromMaybe minBound <$> Map.lookup readerGId (s ^. temp . toTabUniverse)
            minor <- Map.lookup sId (tabOther ^. forSpaces)
            pure (deriveCollectionPermission major minor)

anyCanReadSpaceOld :: NonEmpty ActorId -> SpaceId -> TerseM STM Bool
anyCanReadSpaceOld readers sId = anyM (`canReadSpaceOld` sId) (NE.toList readers)

canCreateSpace :: ActorId -> TerseM STM Bool
canCreateSpace creater = 
  canDo getPerm creater Create
  where
    getPerm createrGId = do
      s <- ask
      liftBase $ maybe minBound (^. collectionPermission)
        <$> Map.lookup createrGId (s ^. temp . toTabUniverse)

anyCanCreateSpace :: NonEmpty ActorId -> TerseM STM Bool
anyCanCreateSpace = anyM canCreateSpace . NE.toList

canUpdateSpace :: ActorId -> SpaceId -> TerseM STM Bool
canUpdateSpace updater sId =
  canDo getPerm updater Update
  where
    getPerm updaterGId = do
      s <- ask
      liftBase $ do
        mTabOther <- Map.lookup updaterGId (s ^. temp . toTabOther)
        case mTabOther of
          Nothing -> pure Blind
          Just tabOther -> do
            major <- fromMaybe minBound <$> Map.lookup updaterGId (s ^. temp . toTabUniverse)
            minor <- Map.lookup sId (tabOther ^. forSpaces)
            pure (deriveCollectionPermission major minor)

anyCanUpdateSpace :: NonEmpty ActorId -> SpaceId -> TerseM STM Bool
anyCanUpdateSpace updaters sId = anyM (`canUpdateSpace` sId) (NE.toList updaters)

canDeleteSpace :: ActorId -> SpaceId -> TerseM STM Bool
canDeleteSpace deleter sId = do
  s <- ask
  andM
    [ canDo getPerm deleter Delete
    , liftBaseWith $ \runInBase ->
        let checkEntity :: Bool -> EntityId -> STM (Maybe Bool)
            checkEntity False _ = pure Nothing
            checkEntity True eId = do
              Just <$> andM
                [ let checkSubscriber :: Bool -> VersionId -> STM (Maybe Bool)
                      checkSubscriber False _ = pure Nothing
                      checkSubscriber True subscriberId =
                        Just <$> runInBase (canUpdateVersion deleter subscriberId)
                  in  foldMaybe checkSubscriber True (Multimap.listTByKey eId (s ^. temp . toSubscriptionsFrom))
                , do  vs <- fromJust <$> Map.lookup eId (s ^. store . toEntities)
                      let checkVersion :: VersionId -> STM Bool
                          checkVersion vId = andM
                              [ let checkReferrer :: Bool -> VersionId -> STM (Maybe Bool)
                                    checkReferrer False _ = pure Nothing
                                    checkReferrer True referrerId =
                                      Just <$> runInBase (canUpdateVersion deleter referrerId)
                                in  foldMaybe checkReferrer True (Multimap.listTByKey vId (s ^. temp . toReferencesFrom))
                              , let checkForker :: Bool -> EntityId -> STM (Maybe Bool)
                                    checkForker False _ = pure Nothing
                                    checkForker True forkerId =
                                      Just <$> runInBase (canUpdateEntity deleter forkerId)
                                in  foldMaybe checkForker True (Multimap.listTByKey vId (s ^. temp . toForksFrom))
                              ]
                      allM checkVersion (NE.toList vs)
                ]
        in  foldMaybe checkEntity True (Multimap.listTByKey sId (s ^. store . toSpaceEntities))
    ]
  where
    getPerm deleterGId = do
      s <- ask
      liftBase $ do
        mTabOther <- Map.lookup deleterGId (s ^. temp . toTabOther)
        case mTabOther of
          Nothing -> pure Blind
          Just tabOther -> do
            major <- fromMaybe minBound <$> Map.lookup deleterGId (s ^. temp . toTabUniverse)
            minor <- Map.lookup sId (tabOther ^. forSpaces)
            pure (deriveCollectionPermission major minor)

anyCanDeleteSpace :: NonEmpty ActorId -> SpaceId -> TerseM STM Bool
anyCanDeleteSpace deleters sId = anyM (`canDeleteSpace` sId) (NE.toList deleters)

hasSpacePermission :: ActorId -> SpaceId -> SinglePermission -> TerseM STM Bool
hasSpacePermission aId sId p =
  canDoWithTab getCheckedPerm aId getRefPerm
  where
    getCheckedPerm gId = do
      s <- ask
      liftBase $ do
        mTabOther <- Map.lookup gId (s ^. temp . toTabOther)
        case mTabOther of
          Nothing -> pure Blind
          Just tabOther -> do
            major <- fromMaybe minBound <$> Map.lookup gId (s ^. temp . toTabUniverse)
            minor <- Map.lookup sId (tabOther ^. forSpaces)
            pure (deriveCollectionPermission major minor)

    getRefPerm gId = do
      s <- ask
      liftBase $ do
        major <- fromMaybe minBound <$> Map.lookup gId (s ^. temp . toTabUniverse)
        pure (escalate major p)

canReadAllEntities :: ActorId -> SpaceId -> TerseM STM Bool
canReadAllEntities reader sId = andM
  [ canDo getPerm reader Read
  , canReadSpace reader sId
  ]
  where
    getPerm readerGId = do
      s <- ask
      liftBase $ do
        mTabOther <- Map.lookup readerGId (s ^. temp . toTabOther)
        case mTabOther of
          Nothing -> pure Blind
          Just tabOther -> fromMaybe Blind <$> Map.lookup sId (tabOther ^. forEntities)

anyCanReadAllEntities :: NonEmpty ActorId -> SpaceId -> TerseM STM Bool
anyCanReadAllEntities readers sId =
  anyM (`canReadAllEntities` sId) (NE.toList readers)

canReadEntity :: ActorId -> EntityId -> TerseM STM Bool
canReadEntity reader eId = do
  s <- ask
  mSId <- liftBase $ Map.lookup eId (s ^. temp . toSpaceOf)
  case mSId of
    Nothing -> pure False
    Just sId -> canReadAllEntities reader sId

anyCanReadEntity :: NonEmpty ActorId -> EntityId -> TerseM STM Bool
anyCanReadEntity readers eId =
  anyM (`canReadEntity` eId) (NE.toList readers)

canCreateEntity :: ActorId -> SpaceId -> TerseM STM Bool
canCreateEntity creater sId = andM
  [ canDo getPerm creater Create
  , canReadSpace creater sId
  ]
  where
    getPerm createrGId = do
      s <- ask
      liftBase $ do
        mTabOther <- Map.lookup createrGId (s ^. temp . toTabOther)
        case mTabOther of
          Nothing -> pure Blind
          Just tabOther -> fromMaybe Blind <$> Map.lookup sId (tabOther ^. forEntities)

anyCanCreateEntity :: NonEmpty ActorId -> SpaceId -> TerseM STM Bool
anyCanCreateEntity creaters sId =
  anyM (`canCreateEntity` sId) (NE.toList creaters)

canUpdateAllEntities :: ActorId -> SpaceId -> TerseM STM Bool
canUpdateAllEntities updater sId = andM
  [ canDo getPerm updater Update
  , canUpdateSpace updater sId
  ]
  where
    getPerm updaterGId = do
      s <- ask
      liftBase $ do
        mTabOther <- Map.lookup updaterGId (s ^. temp . toTabOther)
        case mTabOther of
          Nothing -> pure Blind
          Just tabOther -> fromMaybe Blind <$> Map.lookup sId (tabOther ^. forEntities)

anyCanUpdateAllEntities :: NonEmpty ActorId -> SpaceId -> TerseM STM Bool
anyCanUpdateAllEntities updaters sId =
  anyM (`canUpdateAllEntities` sId) (NE.toList updaters)

canUpdateEntity :: ActorId -> EntityId -> TerseM STM Bool
canUpdateEntity updater eId = do
  s <- ask
  mSId <- liftBase $ Map.lookup eId (s ^. temp . toSpaceOf)
  case mSId of
    Nothing -> pure False
    Just sId -> canUpdateAllEntities updater sId

anyCanUpdateEntity :: NonEmpty ActorId -> EntityId -> TerseM STM Bool
anyCanUpdateEntity updaters eId =
  anyM (`canUpdateEntity` eId) (NE.toList updaters)

canDeleteEntity :: ActorId -> EntityId -> TerseM STM Bool
canDeleteEntity deleter eId = do
  s <- ask
  mSId <- liftBase $ Map.lookup eId (s ^. temp . toSpaceOf)
  case mSId of
    Nothing -> pure False
    Just sId -> andM
      [ canReadSpace deleter sId
      , let getPerm deleterGId = do
              s <- ask
              liftBase $ do
                mTabOther <- Map.lookup deleterGId (s ^. temp . toTabOther)
                case mTabOther of
                  Nothing -> pure Blind
                  Just tabOther -> fromMaybe Blind <$> Map.lookup sId (tabOther ^. forEntities)
        in  canDo getPerm deleter Delete
      , liftBaseWith $ \runInBase -> andM
          [ let checkSubscriber :: Bool -> VersionId -> STM (Maybe Bool)
                checkSubscriber False _ = pure Nothing
                checkSubscriber True subscriberId =
                  Just <$> runInBase (canUpdateVersion deleter subscriberId)
            in  foldMaybe checkSubscriber True (Multimap.listTByKey eId (s ^. temp . toSubscriptionsFrom))
          , do  vs <- fromJust <$> Map.lookup eId (s ^. store . toEntities)
                let checkVersion :: VersionId -> STM Bool
                    checkVersion vId = andM
                        [ let checkReferrer :: Bool -> VersionId -> STM (Maybe Bool)
                              checkReferrer False _ = pure Nothing
                              checkReferrer True referrerId =
                                Just <$> runInBase (canUpdateVersion deleter referrerId)
                          in  foldMaybe checkReferrer True (Multimap.listTByKey vId (s ^. temp . toReferencesFrom))
                        , let checkForker :: Bool -> EntityId -> STM (Maybe Bool)
                              checkForker False _ = pure Nothing
                              checkForker True forkerId =
                                Just <$> runInBase (canUpdateEntity deleter forkerId)
                          in  foldMaybe checkForker True (Multimap.listTByKey vId (s ^. temp . toForksFrom))
                        ]
                allM checkVersion (NE.toList vs)
          ]
      ]

anyCanDeleteEntity :: NonEmpty ActorId -> EntityId -> TerseM STM Bool
anyCanDeleteEntity deleters eId = anyM (`canDeleteEntity` eId) (NE.toList deleters)

hasEntityPermission :: ActorId -> SpaceId -> CollectionPermission -> TerseM STM Bool
hasEntityPermission aId sId p = andM
  [ canReadSpace aId sId
  , orM
    [ let getPerm gId = do
            s <- ask
            liftBase $ do
              mTabOther <- Map.lookup gId (s ^. temp . toTabOther)
              case mTabOther of
                Nothing -> pure Blind
                Just tabOther -> fromMaybe Blind <$> Map.lookup sId (tabOther ^. forEntities)
      in  canDo getPerm aId p
    , let getPerm gId = do
            s <- ask
            liftBase $ maybe Blind (^. collectionPermission) <$> Map.lookup gId (s ^. temp . toTabUniverse)
      in  canDo getPerm aId Update
    ]
  ]

canReadVersion :: ActorId -> VersionId -> TerseM STM Bool
canReadVersion reader vId = do
  s <- ask
  mEId <- liftBase $ Map.lookup vId (s ^. temp . toEntityOf)
  case mEId of
    Nothing -> pure False
    Just eId -> canReadEntity reader eId

anyCanReadVersion :: NonEmpty ActorId -> VersionId -> TerseM STM Bool
anyCanReadVersion readers vId =
  anyM (`canReadVersion` vId) (NE.toList readers)

canCreateVersion :: ActorId -> EntityId -> TerseM STM Bool
canCreateVersion = canUpdateEntity

anyCanCreateVersion :: NonEmpty ActorId -> EntityId -> TerseM STM Bool
anyCanCreateVersion = anyCanUpdateEntity

canUpdateVersion :: ActorId -> VersionId -> TerseM STM Bool
canUpdateVersion updater vId = do
  s <- ask
  mEId <- liftBase $ Map.lookup vId (s ^. temp . toEntityOf)
  case mEId of
    Nothing -> pure False
    Just eId -> canUpdateEntity updater eId

anyCanUpdateVersion :: NonEmpty ActorId -> VersionId -> TerseM STM Bool
anyCanUpdateVersion updaters vId = 
  anyM (`canUpdateVersion` vId) (NE.toList updaters)

canDeleteVersion :: ActorId -> VersionId -> TerseM STM Bool
canDeleteVersion = canUpdateVersion

anyCanDeleteVersion :: NonEmpty ActorId -> VersionId -> TerseM STM Bool
anyCanDeleteVersion = anyCanUpdateVersion

canDeleteVersionAndEntity :: ActorId -> VersionId -> TerseM STM Bool
canDeleteVersionAndEntity deleter vId = do
  s <- ask
  mEId <- liftBase $ Map.lookup vId (s ^. temp . toEntityOf)
  case mEId of
    Nothing -> pure False
    Just eId -> canDeleteEntity deleter eId

anyCanDeleteVersionAndEntity :: NonEmpty ActorId -> VersionId -> TerseM STM Bool
anyCanDeleteVersionAndEntity deleters vId =
  anyM (`canDeleteVersionAndEntity` vId) (NE.toList deleters)
