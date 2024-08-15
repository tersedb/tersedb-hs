module Lib.Actions.Safe.Store where

import Lib.Actions.Safe.Verify
  ( canCreateGroup
  , canCreateActor
  , canCreateMember
  , canCreateSpace
  , canCreateEntity
  , canUpdateEntity
  , canReadEntity
  , conditionally
  )
import Lib.Actions.Tabulation (LoadRefsAndSubsError)
import Lib.Actions.Unsafe.Store
  ( unsafeStoreGroup
  , unsafeStoreActor
  , unsafeStoreSpace
  , unsafeStoreEntity
  , unsafeStoreVersion
  , unsafeAddMember
  )
import Lib.Types.Id (GroupId, SpaceId, EntityId, VersionId, ActorId)
import Lib.Types.Store
  ( Shared
  , store
  , toEntities
  , toVersions
  )
import Lib.Types.Store.Entity (versions)
import Lib.Types.Store.Version (genesisVersion, forkVersion, entity)

import qualified Data.List.NonEmpty as NE
import Control.Lens ((^.), at, _Left, (%~), (&))
import Control.Monad.State (MonadState (get))
import Control.Monad.Extra (andM)



storeGroup
  :: MonadState Shared m
  => ActorId -- ^ actor storing the group
  -> GroupId -- ^ group being stored
  -> m Bool
storeGroup creator gId =
  canCreateGroup creator >>=
    conditionally (unsafeStoreGroup gId)

storeActor
  :: MonadState Shared m
  => ActorId -- ^ actor storing the created actor
  -> ActorId -- ^ created actor being stored
  -> m Bool
storeActor creator aId =
  canCreateActor creator >>=
    conditionally (unsafeStoreActor aId)

addMember
  :: MonadState Shared m
  => ActorId -- ^ actor creating membership
  -> GroupId -- ^ group gaining a member
  -> ActorId -- ^ new member
  -> m Bool
addMember creator gId aId = do
  canCreateMember creator gId >>=
    conditionally (unsafeAddMember gId aId)

storeSpace
  :: MonadState Shared m
  => ActorId -- ^ actor storing the space
  -> SpaceId -- ^ space being created
  -> m Bool
storeSpace creator sId =
  canCreateSpace creator >>=
    conditionally (unsafeStoreSpace sId)

storeEntity
  :: MonadState Shared m
  => ActorId -- ^ actor storing the entity
  -> EntityId -- ^ entity being stored
  -> SpaceId -- ^ space in which entity is being stored
  -> VersionId -- ^ initial version
  -> m Bool
storeEntity creator eId sId vId = do
  canAdjust <- canCreateEntity creator sId
  if not canAdjust then pure False else do
    eVErr <- unsafeStoreEntity eId sId vId genesisVersion
    case eVErr of
      Left e -> error $ "Impossible error - genesis version has undefined reference " <> show (e, eId, sId, vId)
      Right () -> pure True

data StoreForkedEntityError
  = PreviousVersionDoesntExist
  | PreviousEntityDoesntExist
  | ForkingSelf
  deriving (Eq, Show, Read)

storeForkedEntity
  :: MonadState Shared m
  => ActorId -- ^ actor forking the entity
  -> EntityId -- ^ entity being forked
  -> SpaceId -- ^ space in which entity is being forked to
  -> VersionId -- ^ initial version of forked entitiy
  -> VersionId -- ^ previous version of entity (possibly in a different space, definitely in a different entity)
  -> m (Maybe (Either (Either StoreForkedEntityError LoadRefsAndSubsError) ()))
storeForkedEntity creator eId sId vId prevVId
  | vId == prevVId = pure . Just . Left $ Left ForkingSelf
  | otherwise = do
  s <- get
  case s ^. store . toVersions . at prevVId of
    Nothing -> pure . Just . Left $ Left PreviousVersionDoesntExist
    Just prevV -> do
      canAdjust <- andM
        [ canCreateEntity creator sId
        , canReadEntity creator (prevV ^. entity)
        ]
      if not canAdjust then pure Nothing else do
        -- FIXME will this forked reference be present in references tabulation?
        eVErr <- unsafeStoreEntity eId sId vId (flip forkVersion prevVId)
        pure . Just $ eVErr & _Left %~ Right

storeNextVersion
  :: MonadState Shared m
  => ActorId -- ^ actor attempting to store a version
  -> EntityId -- ^ entity receiving a new version
  -> VersionId -- ^ version being stored
  -> m (Maybe (Either LoadRefsAndSubsError ()))
storeNextVersion creator eId vId = do
  s <- get
  case s ^. store . toEntities . at eId of
    Nothing -> pure Nothing
    Just e -> do
      canAdjust <- canUpdateEntity creator eId
      if not canAdjust then  pure Nothing else do
        fmap Just . unsafeStoreVersion eId vId . flip forkVersion . NE.head $ e ^. versions
