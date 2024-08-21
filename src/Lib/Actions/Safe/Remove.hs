module Lib.Actions.Safe.Remove where

import Control.Lens (ix, (^.), (^?))
import Control.Monad.Extra (allM, andM)
import Control.Monad.State (MonadState (get))
import qualified Data.HashSet as HS
import Lib.Actions.Safe.Verify (
  canDeleteEntity,
  canDeleteSpace,
  canDeleteVersion,
  canDeleteMember,
  canReadVersion,
  canUpdateEntity,
  canUpdateVersion,
  conditionally,
 )
import Lib.Actions.Unsafe.Remove (
  unsafeRemoveEntity,
  unsafeRemoveSpace,
  unsafeRemoveVersion,
  unsafeRemoveMember,
 )
import Lib.Types.Id (ActorId, EntityId, SpaceId, VersionId, GroupId)
import Lib.Types.Store (
  Shared,
  store,
  temp,
  toEntities,
  toReferencesFrom,
  toSpaces,
  toSubscriptionsFrom,
 )
import Lib.Types.Store.Entity (space)
import Lib.Types.Store.Space (entities)

removeVersion
  :: (MonadState Shared m)
  => ActorId
  -> VersionId
  -> m (Maybe (Either (Either VersionId EntityId) ()))
removeVersion remover vId = do
  s <- get
  canAdjust <-
    andM
      [ canDeleteVersion remover vId
      , case s ^? temp . toReferencesFrom . ix vId of
          Nothing -> pure True
          Just refs -> allM (canUpdateVersion remover) (HS.toList refs)
          -- NOTE we don't need to check for `canReadVersion` or `canReadEntity` for
          -- this version's references and subscriptions, because we're essentially
          -- making this version blind to them by deleting it
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeRemoveVersion vId

removeEntity
  :: (MonadState Shared m)
  => ActorId
  -> EntityId
  -> m (Maybe (Either (Either VersionId EntityId) ()))
removeEntity remover eId = do
  s <- get
  canAdjust <-
    andM
      [ case s ^? store . toEntities . ix eId of
          Nothing -> pure False
          Just e -> canDeleteEntity remover (e ^. space) eId
      , case s ^? temp . toSubscriptionsFrom . ix eId of
          Nothing -> pure True
          Just subs -> allM (canUpdateVersion remover) (HS.toList subs)
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeRemoveEntity eId

removeSpace
  :: (MonadState Shared m)
  => ActorId
  -> SpaceId
  -> m (Maybe (Either (Either (Either VersionId EntityId) SpaceId) ()))
removeSpace remover sId = do
  s <- get
  canAdjust <- case s ^? store . toSpaces . ix sId of
    Nothing -> pure False
    Just s ->
      andM
        [ canDeleteSpace remover sId
        , allM (canDeleteEntity remover sId) (HS.toList (s ^. entities))
        ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeRemoveSpace sId


removeMember
  :: MonadState Shared m
  => ActorId
  -> GroupId
  -> ActorId
  -> m Bool
removeMember remover gId aId =
  canDeleteMember remover gId >>= conditionally (unsafeRemoveMember gId aId)
