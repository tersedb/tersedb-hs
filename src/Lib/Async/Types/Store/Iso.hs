module Lib.Async.Types.Store.Iso where

import Control.Concurrent.STM (STM, modifyTVar, newTVar, readTVar)
import Control.Lens (at, ix, non, (&), (.~), (?~), (^.))
import Control.Monad.Reader (MonadReader (ask))
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import DeferredFolds.UnfoldlM (forM_)
import qualified Lib.Sync.Types.Store.Tabulation.Group as Sync
import Lib.Async.Types.Store (
  Shared,
  entityPermission,
  groupPermission,
  memberPermission,
  newPermissionsPerGroup,
  spacePermission,
  store,
  toActors,
  toEdges,
  toEntities,
  toForks,
  toGroupsNext,
  toGroupsPrev,
  toMembers,
  toOuts,
  toPermOrganization,
  toPermOther,
  toPermRecruiter,
  toPermUniverse,
  toReferences,
  toRoots,
  toSpaceEntities,
  toSpaces,
  toSubscriptions,
  toVersions, temp, toReferencesFrom, toSubscriptionsFrom, toForksFrom, toSpacesHiddenTo, toMemberOf, toSpaceOf, toEntityOf, toTabUniverse, toTabOrganization, toTabRecruiter, toTabOther,
 )
import Lib.Sync.Actions.Unsafe (unsafeEmptyShared)
import qualified Lib.Sync.Types.Store as Sync
import qualified Lib.Sync.Types.Store.Groups as Sync
import qualified Lib.Sync.Types.Store.Version as Sync
import qualified Lib.Types.Store.Entity as Sync
import qualified StmContainers.Map as Map
import qualified StmContainers.Multimap as Multimap
import qualified StmContainers.Set as Set
import qualified Lib.Async.Types.Tabulation as Tab
import Lib.Async.Types.Tabulation (forSpaces, forEntities, forGroups, forMembers)

loadSyncStore :: (MonadReader Shared m) => Sync.Store -> m (STM ())
loadSyncStore syncStore = do
  s <- ask
  pure $ do
    for_ (HM.toList $ syncStore ^. Sync.toGroups . Sync.nodes) $ \(gId, g) -> do
      case g ^. Sync.prev of
        Nothing -> pure ()
        Just prev -> Map.insert prev gId (s ^. store . toGroupsPrev)
      for_ (g ^. Sync.next) $ \nextId ->
        Multimap.insert nextId gId (s ^. store . toGroupsNext)
      Map.insert (g ^. Sync.universePermission) gId (s ^. store . toPermUniverse)
      Map.insert
        (g ^. Sync.organizationPermission)
        gId
        (s ^. store . toPermOrganization)
      Map.insert (g ^. Sync.recruiterPermission) gId (s ^. store . toPermRecruiter)
      permOther <- newPermissionsPerGroup
      Map.insert permOther gId (s ^. store . toPermOther)
      for_ (g ^. Sync.members) $ \aId ->
        Multimap.insert aId gId (s ^. store . toMembers)

    for_ (syncStore ^. Sync.toGroups . Sync.roots) $ \gId ->
      Set.insert gId (s ^. store . toRoots)
    for_ (syncStore ^. Sync.toGroups . Sync.edges) $ \edge ->
      Set.insert edge (s ^. store . toEdges)
    for_ (syncStore ^. Sync.toGroups . Sync.outs) $ \gId ->
      Set.insert gId (s ^. store . toOuts)

    for_ (syncStore ^. Sync.toActors) $ \aId ->
      Set.insert aId (s ^. store . toActors)

    for_ (HM.toList $ syncStore ^. Sync.toSpaces) $ \(sId, es) -> do
      Set.insert sId (s ^. store . toSpaces)
      for_ es $ \eId ->
        Multimap.insert eId sId (s ^. store . toSpaceEntities)

    for_ (HM.toList $ syncStore ^. Sync.toEntities) $ \(eId, e) -> do
      Map.insert (e ^. Sync.versions) eId (s ^. store . toEntities)
      case e ^. Sync.fork of
        Nothing -> pure ()
        Just fork -> Map.insert fork eId (s ^. store . toForks)

    for_ (HM.toList $ syncStore ^. Sync.toVersions) $ \(vId, v) -> do
      Set.insert vId (s ^. store . toVersions)
      for_ (v ^. Sync.references) $ \refId ->
        Multimap.insert refId vId (s ^. store . toReferences)
      for_ (v ^. Sync.subscriptions) $ \subId ->
        Multimap.insert subId vId (s ^. store . toSubscriptions)

    for_ (HM.toList (syncStore ^. Sync.toSpacePermissions)) $ \(gId, ps) -> do
      permOther <- fromJust <$> Map.lookup gId (s ^. store . toPermOther)
      for_ (HM.toList ps) $ \(sId, p) ->
        Map.insert p sId (permOther ^. spacePermission)
    for_ (HM.toList (syncStore ^. Sync.toEntityPermissions)) $ \(gId, ps) -> do
      permOther <- fromJust <$> Map.lookup gId (s ^. store . toPermOther)
      for_ (HM.toList ps) $ \(sId, p) ->
        Map.insert p sId (permOther ^. entityPermission)
    for_ (HM.toList (syncStore ^. Sync.toGroupPermissions)) $ \(gId, ps) -> do
      permOther <- fromJust <$> Map.lookup gId (s ^. store . toPermOther)
      for_ (HM.toList ps) $ \(sId, p) ->
        Map.insert p sId (permOther ^. groupPermission)
    for_ (HM.toList (syncStore ^. Sync.toMemberPermissions)) $ \(gId, ps) -> do
      permOther <- fromJust <$> Map.lookup gId (s ^. store . toPermOther)
      for_ (HM.toList ps) $ \(sId, p) ->
        Map.insert p sId (permOther ^. memberPermission)

genSyncStore :: (MonadReader Shared m) => m (STM Sync.Store)
genSyncStore = do
  s <- ask
  pure $ do
    syncStore <- newTVar (unsafeEmptyShared ^. Sync.store)

    forM_ (Set.unfoldlM (s ^. store . toRoots)) $ \gId -> do
      modifyTVar syncStore (Sync.toGroups . Sync.nodes . at gId ?~ Sync.emptyGroup)
      modifyTVar syncStore (Sync.toGroups . Sync.roots . at gId ?~ ())

    forM_ (Set.unfoldlM (s ^. store . toOuts)) $ \gId -> do
      modifyTVar syncStore (Sync.toGroups . Sync.nodes . at gId ?~ Sync.emptyGroup)
      modifyTVar syncStore (Sync.toGroups . Sync.outs . at gId ?~ ())

    forM_ (Set.unfoldlM (s ^. store . toEdges)) $ \(fromId, toId) -> do
      modifyTVar syncStore (Sync.toGroups . Sync.nodes . at fromId ?~ Sync.emptyGroup)
      modifyTVar syncStore (Sync.toGroups . Sync.nodes . at toId ?~ Sync.emptyGroup)
      modifyTVar syncStore (Sync.toGroups . Sync.edges . at (fromId, toId) ?~ ())

    forM_ (Map.unfoldlM (s ^. store . toGroupsPrev)) $ \(gId, forkId) ->
      modifyTVar syncStore (Sync.toGroups . Sync.nodes . ix gId . Sync.prev ?~ forkId)
    forM_ (Multimap.unfoldlM (s ^. store . toGroupsNext)) $ \(gId, nextId) ->
      modifyTVar
        syncStore
        (Sync.toGroups . Sync.nodes . ix gId . Sync.next . at nextId ?~ ())
    forM_ (Multimap.unfoldlM (s ^. store . toMembers)) $ \(gId, aId) ->
      modifyTVar
        syncStore
        (Sync.toGroups . Sync.nodes . ix gId . Sync.members . at aId ?~ ())

    forM_ (Set.unfoldlM (s ^. store . toActors)) $ \aId ->
      modifyTVar syncStore (Sync.toActors . at aId ?~ ())

    forM_ (Set.unfoldlM (s ^. store . toSpaces)) $ \sId ->
      modifyTVar syncStore (Sync.toSpaces . at sId ?~ mempty)
    forM_ (Multimap.unfoldlM (s ^. store . toSpaceEntities)) $ \(sId, eId) ->
      modifyTVar syncStore (Sync.toSpaces . ix sId . at eId ?~ ())

    forM_ (Map.unfoldlM (s ^. store . toEntities)) $ \(eId, vs) ->
      let e = Sync.initEntity (NE.head vs) Nothing & Sync.versions .~ vs
       in modifyTVar syncStore (Sync.toEntities . at eId ?~ e)
    forM_ (Map.unfoldlM (s ^. store . toForks)) $ \(eId, forkId) ->
      modifyTVar syncStore (Sync.toEntities . ix eId . Sync.fork ?~ forkId)

    forM_ (Set.unfoldlM (s ^. store . toVersions)) $ \vId ->
      modifyTVar syncStore (Sync.toVersions . at vId ?~ Sync.initVersion)
    forM_ (Multimap.unfoldlM (s ^. store . toReferences)) $ \(vId, refId) ->
      modifyTVar
        syncStore
        (Sync.toVersions . ix vId . Sync.references . at refId ?~ ())
    forM_ (Multimap.unfoldlM (s ^. store . toSubscriptions)) $ \(vId, subId) ->
      modifyTVar
        syncStore
        (Sync.toVersions . ix vId . Sync.subscriptions . at subId ?~ ())

    forM_ (Map.unfoldlM (s ^. store . toPermUniverse)) $ \(gId, p) ->
      modifyTVar
        syncStore
        (Sync.toGroups . Sync.nodes . ix gId . Sync.universePermission .~ p)
    forM_ (Map.unfoldlM (s ^. store . toPermOrganization)) $ \(gId, p) ->
      modifyTVar
        syncStore
        (Sync.toGroups . Sync.nodes . ix gId . Sync.organizationPermission .~ p)
    forM_ (Map.unfoldlM (s ^. store . toPermRecruiter)) $ \(gId, p) ->
      modifyTVar
        syncStore
        (Sync.toGroups . Sync.nodes . ix gId . Sync.recruiterPermission .~ p)
    forM_ (Map.unfoldlM (s ^. store . toPermOther)) $ \(gId, permOther) -> do
      forM_ (Map.unfoldlM (permOther ^. spacePermission)) $ \(sId, p) ->
        modifyTVar
          syncStore
          (Sync.toSpacePermissions . at gId . non mempty . at sId ?~ p)
      forM_ (Map.unfoldlM (permOther ^. entityPermission)) $ \(sId, p) ->
        modifyTVar
          syncStore
          (Sync.toEntityPermissions . at gId . non mempty . at sId ?~ p)
      forM_ (Map.unfoldlM (permOther ^. groupPermission)) $ \(gId', p) ->
        modifyTVar
          syncStore
          (Sync.toGroupPermissions . at gId . non mempty . at gId' ?~ p)
      forM_ (Map.unfoldlM (permOther ^. memberPermission)) $ \(gId', p) ->
        modifyTVar
          syncStore
          (Sync.toMemberPermissions . at gId . non mempty . at gId' ?~ p)

    readTVar syncStore


loadSyncTemp :: MonadReader Shared m => Sync.Temp -> m (STM ())
loadSyncTemp syncTemp = do
  s <- ask
  pure $ do
    for_ (HM.toList $ syncTemp ^. Sync.toReferencesFrom) $ \(refId, referrers) ->
      for_ referrers $ \vId ->
        Multimap.insert vId refId (s ^. temp . toReferencesFrom)
    for_ (HM.toList $ syncTemp ^. Sync.toSubscriptionsFrom) $ \(subId, subscribers) ->
      for_ subscribers $ \vId ->
        Multimap.insert vId subId (s ^. temp . toSubscriptionsFrom)
    for_ (HM.toList $ syncTemp ^. Sync.toForksFrom) $ \(forkId, forkers) ->
      for_ forkers $ \vId ->
        Multimap.insert vId forkId (s ^. temp . toForksFrom)

    for_ (HM.toList $ syncTemp ^. Sync.toTabulatedGroups) $ \(gId, tab) -> do
      Map.insert (tab ^. Sync.forUniverse) gId (s ^. temp . toTabUniverse)
      Map.insert (tab ^. Sync.forOrganization) gId (s ^. temp . toTabOrganization)
      Map.insert (tab ^. Sync.forRecruiter) gId (s ^. temp . toTabRecruiter)
      asyncTab <- Tab.new
      for_ (HM.toList $ tab ^. Sync.forSpaces) $ \(sId, p) ->
        Map.insert p sId (asyncTab ^. forSpaces)
      for_ (HM.toList $ tab ^. Sync.forEntities) $ \(sId, p) ->
        Map.insert p sId (asyncTab ^. forEntities)
      for_ (HM.toList $ tab ^. Sync.forGroups) $ \(gId, p) ->
        Map.insert p gId (asyncTab ^. forGroups)
      for_ (HM.toList $ tab ^. Sync.forMembers) $ \(gId, p) ->
        Map.insert p gId (asyncTab ^. forMembers)
      Map.insert asyncTab gId (s ^. temp . toTabOther)

    for_ (HM.toList $ syncTemp ^. Sync.toSpacesHiddenTo) $ \(sId, gs) ->
      for_ gs $ \gId ->
        Multimap.insert gId sId (s ^. temp . toSpacesHiddenTo)
    for_ (HM.toList $ syncTemp ^. Sync.toMemberOf) $ \(aId, gs) ->
      for_ gs $ \gId ->
        Multimap.insert gId aId (s ^. temp . toMemberOf)
    for_ (HM.toList $ syncTemp ^. Sync.toSpaceOf) $ \(eId, sId) ->
      Map.insert sId eId (s ^. temp . toSpaceOf)
    for_ (HM.toList $ syncTemp ^. Sync.toEntityOf) $ \(vId, eId) ->
      Map.insert eId vId (s ^. temp . toEntityOf)


genSyncTemp :: MonadReader Shared m => m (STM Sync.Temp)
genSyncTemp = do
  s <- ask
  pure $ do
    syncTemp <- newTVar (unsafeEmptyShared ^. Sync.temp)

    forM_ (Multimap.unfoldlM (s ^. temp . toReferencesFrom)) $ \(refId, vId) ->
      modifyTVar syncTemp $ Sync.toReferencesFrom . at refId . non mempty . at vId ?~ ()
    forM_ (Multimap.unfoldlM (s ^. temp . toSubscriptionsFrom)) $ \(subId, vId) ->
      modifyTVar syncTemp $ Sync.toSubscriptionsFrom . at subId . non mempty . at vId ?~ ()
    forM_ (Multimap.unfoldlM (s ^. temp . toForksFrom)) $ \(forkId, eId) ->
      modifyTVar syncTemp $ Sync.toForksFrom . at forkId . non mempty . at eId ?~ ()
    forM_ (Multimap.unfoldlM (s ^. temp . toSpacesHiddenTo)) $ \(spaceId, gId) ->
      modifyTVar syncTemp $ Sync.toSpacesHiddenTo . at spaceId . non mempty . at gId ?~ ()
    forM_ (Multimap.unfoldlM (s ^. temp . toMemberOf)) $ \(aId, gId) ->
      modifyTVar syncTemp $ Sync.toMemberOf . at aId . non mempty . at gId ?~ ()
    forM_ (Map.unfoldlM (s ^. temp . toSpaceOf)) $ \(eId, sId) ->
      modifyTVar syncTemp $ Sync.toSpaceOf . at eId ?~ sId
    forM_ (Map.unfoldlM (s ^. temp . toEntityOf)) $ \(vId, eId) ->
      modifyTVar syncTemp $ Sync.toEntityOf . at vId ?~ eId

    forM_ (Map.unfoldlM (s ^. temp . toTabUniverse)) $ \(gId, p) ->
      modifyTVar syncTemp $ Sync.toTabulatedGroups . at gId . non mempty . Sync.forUniverse .~ p
    forM_ (Map.unfoldlM (s ^. temp . toTabOrganization)) $ \(gId, p) ->
      modifyTVar syncTemp $ Sync.toTabulatedGroups . at gId . non mempty . Sync.forOrganization .~ p
    forM_ (Map.unfoldlM (s ^. temp . toTabRecruiter)) $ \(gId, p) ->
      modifyTVar syncTemp $ Sync.toTabulatedGroups . at gId . non mempty . Sync.forRecruiter .~ p
    forM_ (Map.unfoldlM (s ^. temp . toTabOther)) $ \(gId, tabOther) -> do
      forM_ (Map.unfoldlM (tabOther ^. forSpaces)) $ \(sId, p) ->
        modifyTVar syncTemp $ Sync.toTabulatedGroups . at gId . non mempty . Sync.forSpaces . at sId ?~ p
      forM_ (Map.unfoldlM (tabOther ^. forEntities)) $ \(sId, p) ->
        modifyTVar syncTemp $ Sync.toTabulatedGroups . at gId . non mempty . Sync.forEntities . at sId ?~ p
      forM_ (Map.unfoldlM (tabOther ^. forGroups)) $ \(gId', p) ->
        modifyTVar syncTemp $ Sync.toTabulatedGroups . at gId . non mempty . Sync.forGroups . at gId' ?~ p
      forM_ (Map.unfoldlM (tabOther ^. forMembers)) $ \(gId', p) ->
        modifyTVar syncTemp $ Sync.toTabulatedGroups . at gId . non mempty . Sync.forMembers . at gId' ?~ p

    readTVar syncTemp
