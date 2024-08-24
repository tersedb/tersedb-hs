module Lib.Async.Actions.Tabulation where

import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift))
import Lib.Async.Types.Store (Shared, Temp, toPermUniverse, toPermOrganization, toPermRecruiter
  , toPermOther, spacePermission, toTabOther, toTabUniverse, toTabOrganization
  , toTabRecruiter, temp, store, entityPermission, groupPermission, memberPermission, toGroupsPrev, newPermissionsPerGroup)
import Lib.Async.Types.Tabulation (forSpaces, forEntities, forGroups, forMembers, TabulatedPermissions)
import qualified Lib.Async.Types.Tabulation as Tab
import Lib.Types.Id (GroupId)
import Control.Concurrent.STM (STM)
import DeferredFolds.UnfoldlM (forM_)
import StmContainers.Map (Map)
import qualified StmContainers.Map as Map
import Data.Maybe (fromMaybe)
import Control.Lens (Lens', (^.))
import Data.Hashable (Hashable)
import Lib.Types.Permission (escalate, CollectionPermission)
import Focus (Focus)
import qualified Focus
import Control.Monad (join)


mkSetInitTabulatedPermissions
  :: MonadReader Shared m => m (GroupId -> STM ())
mkSetInitTabulatedPermissions = do
  s <- ask
  pure $ \gId -> do
    univ <- fromMaybe minBound <$> Map.lookup gId (s ^. store . toPermUniverse)
    Map.insert univ gId (s ^. temp . toTabUniverse)
    org <- fromMaybe minBound <$> Map.lookup gId (s ^. store . toPermOrganization)
    Map.insert org gId (s ^. temp . toTabOrganization)
    recr <- fromMaybe minBound <$> Map.lookup gId (s ^. store . toPermRecruiter)
    Map.insert recr gId (s ^. temp . toTabRecruiter)
    permOther <- maybe newPermissionsPerGroup pure =<< Map.lookup gId (s ^. store . toPermOther)
    tabOther <- Tab.new
    forM_ (Map.unfoldlM (permOther ^. spacePermission)) $ \(sId, p) ->
      Map.insert (escalate univ p) sId (tabOther ^. forSpaces)
    forM_ (Map.unfoldlM (permOther ^. entityPermission)) $ \(sId, p) ->
      Map.insert p sId (tabOther ^. forEntities)
    forM_ (Map.unfoldlM (permOther ^. groupPermission)) $ \(gId, p) ->
      Map.insert (escalate univ p) gId (tabOther ^. forGroups)
    forM_ (Map.unfoldlM (permOther ^. memberPermission)) $ \(gId, p) ->
      Map.insert p gId (tabOther ^. forMembers)
    Map.insert tabOther gId (s ^. temp . toTabOther) 


mkUpdateTabulatedPermissionsStartingAt
  :: MonadReader Shared m => m (GroupId -> STM ())
mkUpdateTabulatedPermissionsStartingAt = do
  setInitTab <- mkSetInitTabulatedPermissions
  s <- ask
  pure $ \gId -> do
    setInitTab gId
    mParent <- Map.lookup gId (s ^. store . toGroupsPrev)
    let overProj :: (Bounded a, Semigroup a) => Lens' Temp (Map GroupId a) -> STM ()
        overProj proj = Map.focus go gId (s ^. temp . proj)
          where
            go = do
              initUniv <- fromMaybe minBound <$> Focus.lookup
              newUniv <- case mParent of
                Nothing -> pure initUniv
                Just parent -> do
                  parentUniv <- fromMaybe minBound <$> lift (Map.lookup parent (s ^. temp . proj))
                  pure (parentUniv <> initUniv)
              Focus.insert newUniv
    overProj toTabUniverse
    overProj toTabOrganization
    overProj toTabRecruiter
    tabOther <- maybe Tab.new pure =<< Map.lookup gId (s ^. temp . toTabOther)
    case mParent of
      Nothing -> pure ()
      Just parent -> do
        parentTabOther <- maybe Tab.new pure =<< Map.lookup parent (s ^. temp . toTabOther)
        let overProjOther :: Hashable k => Lens' TabulatedPermissions (Map k CollectionPermission) -> STM ()
            overProjOther proj = do
              forM_ (Map.unfoldlM (parentTabOther ^. proj)) $ \(sId, p) ->
                Map.focus (Focus.alter (Just . maybe p (<> p))) sId (tabOther ^. proj)
        overProjOther forSpaces
        overProjOther forEntities
        overProjOther forGroups
        overProjOther forMembers
