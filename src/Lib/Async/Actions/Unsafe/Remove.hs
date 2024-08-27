module Lib.Async.Actions.Unsafe.Remove where

import Control.Concurrent.STM (STM)
import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Trans.Control (MonadBaseControl (liftBaseWith))
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import DeferredFolds.UnfoldlM (forM_)
import qualified Focus
import Lib.Async.Actions.Unsafe.Update (
  unsafeRemoveSubscription,
  unsafeUpdateFork,
 )
import Lib.Async.Actions.Unsafe.Update.Group (unsafeUnlinkGroups)
import Lib.Async.Types.Monad (TerseM)
import Lib.Async.Types.Store (
  store,
  temp,
  toActors,
  toEntities,
  toEntityOf,
  toForks,
  toForksFrom,
  toGroupsNext,
  toGroupsPrev,
  toMemberOf,
  toMembers,
  toOuts,
  toReferences,
  toReferencesFrom,
  toRoots,
  toSpaceEntities,
  toSpaceOf,
  toSpaces,
  toSubscriptions,
  toSubscriptionsFrom,
  toVersions,
 )
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import qualified StmContainers.Map as Map
import qualified StmContainers.Multimap as Multimap
import qualified StmContainers.Set as Set

unsafeRemoveVersion :: VersionId -> TerseM STM ()
unsafeRemoveVersion vId = do
  s <- ask
  liftBase $ do
    mEId <- Map.lookup vId (s ^. temp . toEntityOf)
    case mEId of
      Nothing -> pure ()
      Just eId -> do
        mVs <- Map.lookup eId (s ^. store . toEntities)
        case mVs of
          Nothing -> pure ()
          Just vs -> unless (length vs == 1) $ do
            Map.focus
              (Focus.adjust (NE.fromList . NE.filter (/= vId)))
              eId
              (s ^. store . toEntities)
            Set.delete vId (s ^. store . toVersions)
            forM_ (Multimap.unfoldlMByKey vId (s ^. store . toReferences)) $ \refId ->
              Multimap.delete vId refId (s ^. temp . toReferencesFrom)
            Multimap.deleteByKey vId (s ^. store . toReferences)
            Multimap.deleteByKey vId (s ^. temp . toReferencesFrom)
            forM_ (Multimap.unfoldlMByKey vId (s ^. store . toSubscriptions)) $ \subId ->
              Multimap.delete vId subId (s ^. temp . toSubscriptionsFrom)
            Multimap.deleteByKey vId (s ^. store . toSubscriptions)
            forM_ (Multimap.unfoldlMByKey vId (s ^. temp . toForksFrom)) $ \forkId ->
              Map.delete forkId (s ^. store . toForks)
            Multimap.deleteByKey vId (s ^. temp . toForksFrom)

unsafeRemoveEntity :: EntityId -> TerseM STM ()
unsafeRemoveEntity eId = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    mVs <- Map.lookup eId (s ^. store . toEntities)
    case mVs of
      Nothing -> pure ()
      Just vs -> do
        runInBase $ for_ vs unsafeRemoveVersion
        forM_ (Multimap.unfoldlMByKey eId (s ^. temp . toSubscriptionsFrom)) $ \subscriberId ->
          runInBase $ unsafeRemoveSubscription subscriberId eId
        runInBase $ unsafeUpdateFork eId Nothing
        Map.delete eId (s ^. store . toEntities)

unsafeRemoveSpace :: SpaceId -> TerseM STM ()
unsafeRemoveSpace sId = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    forM_ (Multimap.unfoldlMByKey sId (s ^. store . toSpaceEntities)) $ \eId ->
      runInBase $ unsafeRemoveEntity eId
    Set.delete sId (s ^. store . toSpaces)

unsafeRemoveMember :: GroupId -> ActorId -> TerseM STM ()
unsafeRemoveMember gId aId = do
  s <- ask
  liftBase $ do
    Multimap.delete aId gId (s ^. store . toMembers)
    Multimap.delete gId aId (s ^. temp . toMemberOf)

unsafeRemoveActor :: ActorId -> TerseM STM ()
unsafeRemoveActor aId = do
  s <- ask
  liftBase $ do
    forM_ (Multimap.unfoldlMByKey aId (s ^. temp . toMemberOf)) $ \gId ->
      Multimap.delete aId gId (s ^. store . toMembers)
    Multimap.deleteByKey aId (s ^. temp . toMemberOf)
    Set.delete aId (s ^. store . toActors)

unsafeRemoveGroup :: GroupId -> TerseM STM ()
unsafeRemoveGroup gId = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    forM_ (Multimap.unfoldlMByKey gId (s ^. store . toMembers)) $ \aId ->
      runInBase $ unsafeRemoveMember gId aId
    mPrev <- Map.lookup gId (s ^. store . toGroupsPrev)
    case mPrev of
      Nothing -> pure ()
      Just prevId -> runInBase $ unsafeUnlinkGroups prevId gId
    forM_ (Multimap.unfoldlMByKey gId (s ^. store . toGroupsNext)) $ \nextId ->
      runInBase $ unsafeUnlinkGroups gId nextId
    Set.delete gId (s ^. store . toRoots)
    Set.delete gId (s ^. store . toOuts)
