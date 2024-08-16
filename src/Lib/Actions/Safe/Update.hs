module Lib.Actions.Safe.Update where

import Lib.Actions.Safe.Verify
  ( canDeleteEntity
  , canCreateEntity
  , canReadEntity
  , canReadVersion
  , canUpdateVersion
  , conditionally
  )
import Lib.Actions.Unsafe.Update
  ( unsafeUpdateVersionReferences
  , unsafeAddReference
  , unsafeRemoveReference
  , unsafeUpdateVersionSubscriptions
  , unsafeAddSubscription
  , unsafeRemoveSubscription
  )
import Lib.Types.Id (SpaceId, EntityId, ActorId, VersionId)
import Lib.Types.Store
  ( Shared
  , store
  , toEntities
  , toSpaces
  )
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Entity (space)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Control.Lens ((^.), at, ix, (.~))
import Control.Monad.State (MonadState (get), modify)
import Control.Monad.Extra (andM, allM)


-- | Moving an entity between spaces requires delete authority on the current space, and create authority on the destination space
updateEntitySpace :: MonadState Shared m => ActorId -> EntityId -> SpaceId -> m Bool
updateEntitySpace updater eId newSId = do
  s <- get
  case s ^. store . toEntities . at eId of
    Nothing -> pure False -- FIXME
    Just e -> do
      canAdjust <- andM
        [ canDeleteEntity updater (e ^. space) eId
        , canCreateEntity updater newSId
        ]
      flip conditionally canAdjust $ do
        modify $ store . toEntities . ix eId . space .~ newSId
        modify $ store . toSpaces . ix (e ^. space) . entities . at eId .~ Nothing
        modify $ store . toSpaces . ix newSId . entities . at eId .~ Just ()


updateVersionReferences
  :: MonadState Shared m
  => ActorId
  -> VersionId
  -> HashSet VersionId
  -> m (Maybe (Either (Either VersionId EntityId) ()))
updateVersionReferences updater vId refIds = do
  canAdjust <- andM
    [ allM (canReadVersion updater) (HS.toList refIds)
    , canUpdateVersion updater vId
    ]
  if not canAdjust then pure Nothing else
    Just <$> unsafeUpdateVersionReferences vId refIds

addReference
  :: MonadState Shared m
  => ActorId
  -> VersionId
  -> VersionId
  -> m (Maybe (Either (Either VersionId EntityId) ()))
addReference updater vId refId = do
  canAdjust <- andM
    [ canReadVersion updater refId
    , canUpdateVersion updater vId
    ]
  if not canAdjust then pure Nothing else
    Just <$> unsafeAddReference vId refId

removeReference
  :: MonadState Shared m
  => ActorId
  -> VersionId
  -> VersionId
  -> m (Maybe (Either (Either VersionId EntityId) ()))
removeReference updater vId refId = do
  canAdjust <- andM
    [ canReadVersion updater refId
    , canUpdateVersion updater vId
    ]
  if not canAdjust then pure Nothing else
    Just <$> unsafeRemoveReference vId refId


updateVersionSubscriptions
  :: MonadState Shared m
  => ActorId
  -> VersionId
  -> HashSet EntityId
  -> m (Maybe (Either EntityId ()))
updateVersionSubscriptions updater vId subIds = do
  canAdjust <- andM
    [ allM (canReadEntity updater) (HS.toList subIds)
    , canUpdateVersion updater vId
    ]
  if not canAdjust then pure Nothing else
    Just <$> unsafeUpdateVersionSubscriptions vId subIds

addSubscription
  :: MonadState Shared m
  => ActorId
  -> VersionId
  -> EntityId
  -> m (Maybe (Either (Either VersionId EntityId) ()))
addSubscription updater vId subId = do
  canAdjust <- andM
    [ canReadEntity updater subId
    , canUpdateVersion updater vId
    ]
  if not canAdjust then pure Nothing else
    Just <$> unsafeAddSubscription vId subId

removeSubscription
  :: MonadState Shared m
  => ActorId
  -> VersionId
  -> EntityId
  -> m (Maybe (Either (Either VersionId EntityId) ()))
removeSubscription updater vId subId = do
  canAdjust <- andM
    [ canReadEntity updater subId
    , canUpdateVersion updater vId
    ]
  if not canAdjust then pure Nothing else
    Just <$> unsafeRemoveSubscription vId subId
