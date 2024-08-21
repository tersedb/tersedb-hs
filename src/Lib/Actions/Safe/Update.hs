module Lib.Actions.Safe.Update where

import Control.Lens (at, ix, (.~), (^.), (^?))
import Control.Monad.Extra (allM, andM)
import Control.Monad.State (MonadState (get), modify)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Lib.Actions.Safe.Verify (
  canCreateEntity,
  canDeleteEntity,
  canDeleteVersion,
  canReadEntity,
  canReadVersion,
  canUpdateEntity,
  canUpdateVersion,
  conditionally,
 )
import Lib.Actions.Unsafe.Update (
  unsafeAddReference,
  unsafeAddSubscription,
  unsafeMoveEntity,
  unsafeOffsetVersionIndex,
  unsafeRemoveReference,
  unsafeRemoveSubscription,
  unsafeSetVersionIndex,
  unsafeUpdateFork,
  unsafeUpdateVersionReferences,
  unsafeUpdateVersionSubscriptions,
 )
import Lib.Types.Id (ActorId, EntityId, SpaceId, VersionId)
import Lib.Types.Store (
  Shared,
  store,
  temp,
  toEntities,
  toReferencesFrom,
  toSpaces,
  toVersions,
 )
import Lib.Types.Store.Entity (space)
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Version (entity, references, subscriptions)

-- | Moving an entity between spaces requires delete authority on the current space, and create authority on the destination space
updateEntitySpace
  :: (MonadState Shared m) => ActorId -> EntityId -> SpaceId -> m Bool
updateEntitySpace updater eId newSId = do
  s <- get
  case s ^. store . toEntities . at eId of
    Nothing -> pure False -- FIXME
    Just e -> do
      canAdjust <-
        andM
          [ canDeleteEntity updater (e ^. space) eId
          , canCreateEntity updater newSId
          ]
      flip conditionally canAdjust $ do
        modify $ store . toEntities . ix eId . space .~ newSId
        modify $ store . toSpaces . ix (e ^. space) . entities . at eId .~ Nothing
        modify $ store . toSpaces . ix newSId . entities . at eId .~ Just ()

updateVersionReferences
  :: (MonadState Shared m)
  => ActorId
  -> VersionId
  -> HashSet VersionId
  -> m (Maybe (Either VersionId ()))
updateVersionReferences updater vId refIds = do
  canAdjust <-
    andM
      [ allM (canReadVersion updater) (HS.toList refIds)
      , canUpdateVersion updater vId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeUpdateVersionReferences vId refIds

addReference
  :: (MonadState Shared m)
  => ActorId
  -> VersionId
  -> VersionId
  -> m (Maybe (Either VersionId ()))
addReference updater vId refId = do
  canAdjust <-
    andM
      [ canReadVersion updater refId
      , canUpdateVersion updater vId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeAddReference vId refId

removeReference
  :: (MonadState Shared m)
  => ActorId
  -> VersionId
  -> VersionId
  -> m (Maybe (Either VersionId ()))
removeReference updater vId refId = do
  canAdjust <-
    andM
      [ canReadVersion updater refId
      , canUpdateVersion updater vId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeRemoveReference vId refId

updateVersionSubscriptions
  :: (MonadState Shared m)
  => ActorId
  -> VersionId
  -> HashSet EntityId
  -> m (Maybe (Either VersionId ()))
updateVersionSubscriptions updater vId subIds = do
  canAdjust <-
    andM
      [ allM (canReadEntity updater) (HS.toList subIds)
      , canUpdateVersion updater vId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeUpdateVersionSubscriptions vId subIds

addSubscription
  :: (MonadState Shared m)
  => ActorId
  -> VersionId
  -> EntityId
  -> m (Maybe (Either VersionId ()))
addSubscription updater vId subId = do
  canAdjust <-
    andM
      [ canReadEntity updater subId
      , canUpdateVersion updater vId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeAddSubscription vId subId

removeSubscription
  :: (MonadState Shared m)
  => ActorId
  -> VersionId
  -> EntityId
  -> m (Maybe (Either VersionId ()))
removeSubscription updater vId subId = do
  canAdjust <-
    andM
      [ canReadEntity updater subId
      , canUpdateVersion updater vId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeRemoveSubscription vId subId

-- removeVersion
--   :: MonadState Shared m
--   => ActorId
--   -> VersionId
--   -> m (Maybe (Either VersionId ()))
-- removeVersion remover vId = do
--   s <- get
--   canAdjust <- andM
--     [ canDeleteVersion remover vId
--     , case s ^? temp . toReferencesFrom . ix vId of
--         Nothing -> pure True
--         Just refs -> andM
--           [ allM (canUpdateVersion remover) . HS.toList $ refs
--           -- NOTE we don't need to check for `canReadVersion` or `canReadEntity` for
--           -- this version's references and subscriptions, because we're essentially
--           -- making this version blind to them by deleting it
--           ]
--     ]
--   if not canAdjust then pure Nothing else
--     Just <$> unsafeRemoveVersion vId

updateFork
  :: (MonadState Shared m)
  => ActorId
  -> EntityId
  -> Maybe VersionId
  -> m (Maybe (Either EntityId ()))
updateFork updater eId mFork = do
  s <- get
  canAdjust <-
    andM
      [ canUpdateEntity updater eId
      , maybe (pure True) (canReadVersion updater) mFork
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeUpdateFork eId mFork

moveEntity
  :: (MonadState Shared m)
  => ActorId
  -> EntityId
  -> SpaceId
  -> m (Maybe (Either EntityId ()))
moveEntity updater eId newSId = do
  s <- get
  canAdjust <-
    andM
      [ canUpdateEntity updater eId
      , canCreateEntity updater newSId
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeMoveEntity eId newSId

offsetVersionIndex
  :: (MonadState Shared m)
  => ActorId
  -> VersionId
  -> Int
  -> m (Maybe (Either VersionId ()))
offsetVersionIndex updater vId offset = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure . Just $ Left vId
    Just v -> do
      canAdjust <- canUpdateEntity updater (v ^. entity)
      if not canAdjust
        then pure Nothing
        else Just <$> unsafeOffsetVersionIndex vId offset

setVersionIndex
  :: (MonadState Shared m)
  => ActorId
  -> VersionId
  -> Int
  -> m (Maybe (Either VersionId ()))
setVersionIndex updater vId idx = do
  s <- get
  case s ^. store . toVersions . at vId of
    Nothing -> pure . Just $ Left vId
    Just v -> do
      canAdjust <- canUpdateEntity updater (v ^. entity)
      if not canAdjust
        then pure Nothing
        else Just <$> unsafeSetVersionIndex vId idx
