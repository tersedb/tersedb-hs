module Lib.Actions.Safe.Update where

import Lib.Actions.Safe.Verify
  ( canDeleteEntity
  , canCreateEntity
  , canReadEntity
  , canReadVersion
  , canUpdateVersion
  , conditionally
  , canUpdateGroup
  )
import Lib.Actions.Unsafe.Update
  ( unsafeUpdateVersionReferences
  , unsafeUpdateVersionSubscriptions
  , unsafeUpdateGroupParent
  , unsafeAddGroupChild
  , unsafeRemoveGroupChild
  , unsafeUpdateGroupChildren
  )
import Lib.Types.Id (SpaceId, EntityId, ActorId, VersionId, GroupId)
import Lib.Types.Store
  ( Shared
  , store
  , toEntities
  , toSpaces
  , toGroups
  )
import Lib.Types.Store.Groups (prev, nodes, next)
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Entity (space)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Control.Lens ((^.), at, ix, (.~), (^?), _Just)
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


updateGroupChildren
  :: MonadState Shared m
  => ActorId
  -> GroupId
  -> HashSet GroupId
  -> m Bool
updateGroupChildren updater gId newChildren = do
  s <- get
  case s ^? store . toGroups . nodes . ix gId . next of
    Nothing -> pure False
    Just oldChildren -> do
      canAdjust <- andM $
        (canUpdateGroup updater gId) :
        (map (canUpdateGroup updater) (HS.toList (newChildren <> oldChildren)))
      conditionally (unsafeUpdateGroupChildren gId newChildren) canAdjust


addGroupChild
  :: MonadState Shared m
  => ActorId
  -> GroupId
  -> GroupId
  -> m Bool
addGroupChild updater gId childId = do
  canAdjust <- andM
    [ canUpdateGroup updater gId
    , canUpdateGroup updater childId
    ]
  conditionally (unsafeAddGroupChild gId childId) canAdjust


removeGroupChild
  :: MonadState Shared m
  => ActorId
  -> GroupId
  -> GroupId
  -> m Bool
removeGroupChild updater gId childId = do
  canAdjust <- andM
    [ canUpdateGroup updater gId
    , canUpdateGroup updater childId
    ]
  conditionally (unsafeRemoveGroupChild gId childId) canAdjust


updateGroupParent
  :: MonadState Shared m
  => ActorId
  -> GroupId
  -> Maybe GroupId
  -> m Bool
updateGroupParent updater gId mParent = do
  canAdjust <- andM
    [ canUpdateGroup updater gId
    , do  s <- get
          case s ^? store . toGroups . nodes . ix gId . prev . _Just of
            Nothing -> pure True
            Just oldParent -> canUpdateGroup updater oldParent
    , case mParent of
        Nothing -> pure True
        Just newParent -> canUpdateGroup updater newParent
    ]
  conditionally (unsafeUpdateGroupParent gId mParent) canAdjust
