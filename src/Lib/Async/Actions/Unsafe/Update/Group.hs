module Lib.Async.Actions.Unsafe.Update.Group where

import Control.Concurrent.STM (STM, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Exception (Exception)
import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Trans.Control (MonadBaseControl (liftBaseWith))
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..))
import DeferredFolds.UnfoldlM (forM_)
import qualified Focus
import GHC.Generics (Generic)
import Lib.Async.Actions.Tabulation (updateTabulatedPermissionsStartingAt)
import Lib.Async.Types.Monad (TerseM)
import Lib.Async.Types.Store (
  entityPermission,
  groupPermission,
  memberPermission,
  spacePermission,
  store,
  toEdges,
  toGroupsNext,
  toGroupsPrev,
  toOuts,
  toPermOrganization,
  toPermOther,
  toPermRecruiter,
  toPermUniverse,
  toRoots,
 )
import Lib.Types.Id (GroupId, SpaceId)
import Lib.Types.Permission (
  CollectionPermission (Blind),
  CollectionPermissionWithExemption,
  SinglePermission,
 )
import qualified StmContainers.Map as Map
import qualified StmContainers.Multimap as Multimap
import qualified StmContainers.Set as Set

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
    edgeExists <- Set.lookup (from, to) (s ^. store . toEdges)
    unless edgeExists $ do
      wouldCauseCycle <- Set.lookup to (s ^. store . toOuts)
      unless wouldCauseCycle $ do
        Set.insert (from, to) (s ^. store . toEdges)
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
    Set.delete (from, to) (s ^. store . toEdges)
    Set.delete to (s ^. store . toOuts)
    Set.insert from (s ^. store . toOuts)
    Set.insert to (s ^. store . toRoots)
    Multimap.delete to from (s ^. store . toGroupsNext)
    Map.delete to (s ^. store . toGroupsPrev)
    runInBase $ updateTabulatedPermissionsStartingAt to

unsafeAdjustUniversePermission
  :: (CollectionPermissionWithExemption -> CollectionPermissionWithExemption)
  -> GroupId
  -> TerseM STM ()
unsafeAdjustUniversePermission f gId = do
  s <- ask
  liftBase $ Map.focus (Focus.adjust f) gId (s ^. store . toPermUniverse)
  updateTabulatedPermissionsStartingAt gId

unsafeAdjustOrganizationPermission
  :: (CollectionPermissionWithExemption -> CollectionPermissionWithExemption)
  -> GroupId
  -> TerseM STM ()
unsafeAdjustOrganizationPermission f gId = do
  s <- ask
  liftBase $ Map.focus (Focus.adjust f) gId (s ^. store . toPermOrganization)
  updateTabulatedPermissionsStartingAt gId

unsafeAdjustRecruiterPermission
  :: (CollectionPermission -> CollectionPermission) -> GroupId -> TerseM STM ()
unsafeAdjustRecruiterPermission f gId = do
  s <- ask
  liftBase $ Map.focus (Focus.adjust f) gId (s ^. store . toPermRecruiter)
  updateTabulatedPermissionsStartingAt gId

unsafeAdjustSpacePermission
  :: (Maybe SinglePermission -> Maybe SinglePermission)
  -> GroupId
  -> SpaceId
  -> TerseM STM ()
unsafeAdjustSpacePermission f gId sId = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    mPermOther <- Map.lookup gId (s ^. store . toPermOther)
    case mPermOther of
      Nothing -> pure ()
      Just permOther -> do
        Map.focus (Focus.alter f) sId (permOther ^. spacePermission)
        runInBase $ updateTabulatedPermissionsStartingAt gId

unsafeAdjustEntityPermission
  :: (CollectionPermission -> CollectionPermission)
  -> GroupId
  -> SpaceId
  -> TerseM STM ()
unsafeAdjustEntityPermission f gId sId = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    mPermOther <- Map.lookup gId (s ^. store . toPermOther)
    case mPermOther of
      Nothing -> pure ()
      Just permOther -> do
        Map.focus
          (Focus.alter (Just . f . fromMaybe Blind))
          sId
          (permOther ^. entityPermission)
        runInBase $ updateTabulatedPermissionsStartingAt gId

unsafeAdjustGroupPermission
  :: (Maybe SinglePermission -> Maybe SinglePermission)
  -> GroupId
  -> GroupId
  -> TerseM STM ()
unsafeAdjustGroupPermission f gId sId = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    mPermOther <- Map.lookup gId (s ^. store . toPermOther)
    case mPermOther of
      Nothing -> pure ()
      Just permOther -> do
        Map.focus (Focus.alter f) sId (permOther ^. groupPermission)
        runInBase $ updateTabulatedPermissionsStartingAt gId

unsafeAdjustMemberPermission
  :: (CollectionPermission -> CollectionPermission)
  -> GroupId
  -> GroupId
  -> TerseM STM ()
unsafeAdjustMemberPermission f gId sId = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    mPermOther <- Map.lookup gId (s ^. store . toPermOther)
    case mPermOther of
      Nothing -> pure ()
      Just permOther -> do
        Map.focus
          (Focus.alter (Just . f . fromMaybe Blind))
          sId
          (permOther ^. memberPermission)
        runInBase $ updateTabulatedPermissionsStartingAt gId
