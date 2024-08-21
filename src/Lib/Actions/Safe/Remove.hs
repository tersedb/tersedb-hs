module Lib.Actions.Safe.Remove where

import Control.Lens (at, ix, (.~), (^.), (^?))
import Control.Monad.Extra (allM, andM)
import Control.Monad.State (MonadState (get), modify)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Lib.Actions.Safe.Verify
  ( canCreateEntity,
    canDeleteEntity,
    canDeleteVersion,
    canReadEntity,
    canReadVersion,
    canUpdateEntity,
    canUpdateVersion,
    conditionally,
  )
import Lib.Actions.Unsafe.Remove
  ( unsafeRemoveEntity,
    unsafeRemoveVersion,
  )
import Lib.Types.Id (ActorId, EntityId, SpaceId, VersionId)
import Lib.Types.Store
  ( Shared,
    store,
    temp,
    toEntities,
    toReferencesFrom,
    toSpaces,
    toSubscriptionsFrom,
    toVersions,
  )
import Lib.Types.Store.Entity (space)
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Version (entity, references, subscriptions)

removeVersion ::
  (MonadState Shared m) =>
  ActorId ->
  VersionId ->
  m (Maybe (Either (Either VersionId EntityId) ()))
removeVersion remover vId = do
  s <- get
  canAdjust <-
    andM
      [ canDeleteVersion remover vId,
        case s ^? temp . toReferencesFrom . ix vId of
          Nothing -> pure True
          Just refs -> allM (canUpdateVersion remover) (HS.toList refs)
          -- NOTE we don't need to check for `canReadVersion` or `canReadEntity` for
          -- this version's references and subscriptions, because we're essentially
          -- making this version blind to them by deleting it
      ]
  if not canAdjust
    then pure Nothing
    else
      Just <$> unsafeRemoveVersion vId

removeEntity ::
  (MonadState Shared m) =>
  ActorId ->
  EntityId ->
  m (Maybe (Either (Either VersionId EntityId) ()))
removeEntity remover eId = do
  s <- get
  canAdjust <-
    andM
      [ case s ^? store . toEntities . ix eId of
          Nothing -> pure False
          Just e -> canDeleteEntity remover (e ^. space) eId,
        case s ^? temp . toSubscriptionsFrom . ix eId of
          Nothing -> pure True
          Just subs -> allM (canUpdateVersion remover) (HS.toList subs)
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeRemoveEntity eId
