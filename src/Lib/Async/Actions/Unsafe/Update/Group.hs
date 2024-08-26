module Lib.Async.Actions.Unsafe.Update.Group where

import Lib.Types.Id (GroupId, SpaceId)
import Lib.Async.Types.Monad (TerseM)
import Control.Concurrent.STM (STM, newTVar, readTVar, writeTVar, modifyTVar)
import Control.Monad.Reader (MonadReader(ask))
import Control.Monad.Base (MonadBase(liftBase))
import Lib.Async.Types.Store (store, toEdges, toOuts, toRoots, toGroupsNext, toGroupsPrev, toPermUniverse, toPermOrganization, toPermRecruiter, spacePermission, toPermOther, groupPermission, entityPermission, memberPermission)
import Control.Lens ((^.))
import Control.Monad (unless)
import qualified StmContainers.Set as Set
import qualified StmContainers.Map as Map
import qualified StmContainers.Multimap as Multimap
import Data.Monoid (First (..))
import DeferredFolds.UnfoldlM (forM_)
import Data.List (uncons)
import GHC.Generics (Generic)
import Control.Exception (Exception)
import Lib.Async.Actions.Tabulation (updateTabulatedPermissionsStartingAt)
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Control (MonadBaseControl(liftBaseWith))
import Lib.Types.Permission (CollectionPermissionWithExemption, CollectionPermission, SinglePermission)
import qualified Focus

hasCycle :: TerseM STM (Maybe [GroupId])
hasCycle = do
  s <- ask
  liftBase $ do
    trailVar <- newTVar []
    resultVar <- newTVar (First Nothing)
    let dfs :: GroupId -> STM ()
        dfs node = do
          trail <- readTVar trailVar
          if node `elem` trail
            then modifyTVar resultVar (<> First (Just (node : trail)))
            else do
              modifyTVar trailVar (node :)
              forM_ (Multimap.unfoldlMByKey node (s ^. store . toGroupsNext)) dfs
              trail <- readTVar trailVar
              case uncons trail of
                Nothing -> error "Broken hasCycle algorithm - empty stack when attempting uncons"
                Just (_, trail') -> do
                  writeTVar trailVar trail'
    forM_ (Set.unfoldlM (s ^. store . toRoots)) dfs
    getFirst <$> readTVar resultVar


newtype CycleDetected = CycleDetected [GroupId]
  deriving (Generic, Show, Eq)
instance Exception CycleDetected


unsafeLinkGroups :: GroupId -> GroupId -> TerseM STM ()
unsafeLinkGroups from to = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    edgeExists <- Set.lookup (from,to) (s ^. store . toEdges)
    unless edgeExists $ do
      wouldCauseCycle <- Set.lookup to (s ^. store . toOuts)
      unless wouldCauseCycle $ do
        Set.insert (from,to) (s ^. store . toEdges)
        Set.insert to (s ^. store . toOuts)
        Set.delete from (s ^. store . toOuts)
        Set.delete to (s ^. store . toRoots)
        Multimap.insert to from (s ^. store . toGroupsNext)
        Map.insert from to (s ^. store . toGroupsPrev)
        mCycle <- runInBase hasCycle
        case mCycle of
          Just cycle -> throwM $ CycleDetected cycle
          Nothing -> runInBase $ updateTabulatedPermissionsStartingAt to

unsafeUnlinkGroups :: GroupId -> GroupId -> TerseM STM ()
unsafeUnlinkGroups from to = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    Set.delete (from,to) (s ^. store . toEdges)
    Set.delete to (s ^. store . toOuts)
    Set.insert from (s ^. store . toOuts)
    Set.insert to (s ^. store . toRoots)
    Multimap.delete to from (s ^. store . toGroupsNext)
    Map.delete to (s ^. store . toGroupsPrev)
    runInBase $ updateTabulatedPermissionsStartingAt to

unsafeAdjustUniversePermission :: (CollectionPermissionWithExemption -> CollectionPermissionWithExemption) -> GroupId -> TerseM STM ()
unsafeAdjustUniversePermission f gId = do
  s <- ask
  liftBase $ Map.focus (Focus.adjust f) gId (s ^. store . toPermUniverse)
  updateTabulatedPermissionsStartingAt gId

unsafeAdjustOrganizationPermission :: (CollectionPermissionWithExemption -> CollectionPermissionWithExemption) -> GroupId -> TerseM STM ()
unsafeAdjustOrganizationPermission f gId = do
  s <- ask
  liftBase $ Map.focus (Focus.adjust f) gId (s ^. store . toPermOrganization)
  updateTabulatedPermissionsStartingAt gId

unsafeAdjustRecruiterPermission :: (CollectionPermission -> CollectionPermission) -> GroupId -> TerseM STM ()
unsafeAdjustRecruiterPermission f gId = do
  s <- ask
  liftBase $ Map.focus (Focus.adjust f) gId (s ^. store . toPermRecruiter)
  updateTabulatedPermissionsStartingAt gId

unsafeAdjustSpacePermission :: (Maybe SinglePermission -> Maybe SinglePermission) -> GroupId -> SpaceId -> TerseM STM ()
unsafeAdjustSpacePermission f gId sId = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    mPermOther <- Map.lookup gId (s ^. store . toPermOther)
    case mPermOther of
      Nothing -> pure ()
      Just permOther -> do
        Map.focus (Focus.alter f) sId (permOther ^. spacePermission)
        runInBase $ updateTabulatedPermissionsStartingAt gId

unsafeAdjustEntityPermission :: (CollectionPermission -> CollectionPermission) -> GroupId -> SpaceId -> TerseM STM ()
unsafeAdjustEntityPermission f gId sId = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    mPermOther <- Map.lookup gId (s ^. store . toPermOther)
    case mPermOther of
      Nothing -> pure ()
      Just permOther -> do
        Map.focus (Focus.adjust f) sId (permOther ^. entityPermission)
        runInBase $ updateTabulatedPermissionsStartingAt gId

unsafeAdjustGroupPermission :: (Maybe SinglePermission -> Maybe SinglePermission) -> GroupId -> GroupId -> TerseM STM ()
unsafeAdjustGroupPermission f gId sId = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    mPermOther <- Map.lookup gId (s ^. store . toPermOther)
    case mPermOther of
      Nothing -> pure ()
      Just permOther -> do
        Map.focus (Focus.alter f) sId (permOther ^. groupPermission)
        runInBase $ updateTabulatedPermissionsStartingAt gId

unsafeAdjustMemberPermission :: (CollectionPermission -> CollectionPermission) -> GroupId -> GroupId -> TerseM STM ()
unsafeAdjustMemberPermission f gId sId = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    mPermOther <- Map.lookup gId (s ^. store . toPermOther)
    case mPermOther of
      Nothing -> pure ()
      Just permOther -> do
        Map.focus (Focus.adjust f) sId (permOther ^. memberPermission)
        runInBase $ updateTabulatedPermissionsStartingAt gId
