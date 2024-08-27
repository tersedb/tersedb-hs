module Lib.Async.Actions.Tabulation where

import Control.Concurrent.STM (STM)
import Control.Lens (Lens', (^.))
import Control.Monad (join, void, when)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift))
import Control.Monad.Trans.Control (MonadBaseControl (liftBaseWith))
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import Data.Maybe (fromJust, fromMaybe)
import DeferredFolds.UnfoldlM (forM_)
import Focus (Focus)
import qualified Focus
import Lib.Actions.Safe.Utils (deriveCollectionPermission)
import Lib.Async.Types.Monad (TerseM)
import Lib.Async.Types.Store (
  Shared,
  Temp,
  entityPermission,
  groupPermission,
  memberPermission,
  newPermissionsPerGroup,
  spacePermission,
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
  toPermOrganization,
  toPermOther,
  toPermRecruiter,
  toPermUniverse,
  toReferences,
  toReferencesFrom,
  toRoots,
  toSpaceEntities,
  toSpaceOf,
  toSpaces,
  toSpacesHiddenTo,
  toSubscriptions,
  toSubscriptionsFrom,
  toTabOrganization,
  toTabOther,
  toTabRecruiter,
  toTabUniverse,
  toVersions,
 )
import Lib.Async.Types.Tabulation (
  TabulatedPermissions,
  forEntities,
  forGroups,
  forMembers,
  forSpaces,
 )
import qualified Lib.Async.Types.Tabulation as Tab
import Lib.Types.Id (EntityId, GroupId, VersionId)
import Lib.Types.Permission (
  CollectionPermission (Blind),
  CollectionPermissionWithExemption (CollectionPermissionWithExemption),
  escalate,
 )
import StmContainers.Map (Map)
import qualified StmContainers.Map as Map
import qualified StmContainers.Multimap as Multimap
import qualified StmContainers.Set as Set

setInitTabulatedPermissions
  :: GroupId -> TerseM STM ()
setInitTabulatedPermissions gId = do
  s <- ask
  liftBase $ do
    univ <- fromMaybe minBound <$> Map.lookup gId (s ^. store . toPermUniverse)
    Map.insert univ gId (s ^. temp . toTabUniverse)
    org <- fromMaybe minBound <$> Map.lookup gId (s ^. store . toPermOrganization)
    Map.insert org gId (s ^. temp . toTabOrganization)
    recr <- fromMaybe minBound <$> Map.lookup gId (s ^. store . toPermRecruiter)
    Map.insert recr gId (s ^. temp . toTabRecruiter)
    permOther <-
      maybe newPermissionsPerGroup pure =<< Map.lookup gId (s ^. store . toPermOther)
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

updateTabulatedPermissionsStartingAt
  :: GroupId -> TerseM STM ()
updateTabulatedPermissionsStartingAt gId = do
  s <- ask
  setInitTabulatedPermissions gId
  liftBase $ do
    mParent <- Map.lookup gId (s ^. store . toGroupsPrev)
    let overProj :: (Bounded a, Semigroup a) => Lens' Temp (Map GroupId a) -> STM ()
        overProj proj = Map.focus go gId (s ^. temp . proj)
         where
          go = do
            init <- fromMaybe minBound <$> Focus.lookup
            new <- case mParent of
              Nothing -> pure init
              Just parent -> do
                parent <- fromMaybe minBound <$> lift (Map.lookup parent (s ^. temp . proj))
                pure (parent <> init)
            Focus.insert new
    overProj toTabUniverse
    overProj toTabOrganization
    overProj toTabRecruiter
    tabOther <- fromJust <$> Map.lookup gId (s ^. temp . toTabOther)
    case mParent of
      Nothing -> pure ()
      Just parent -> do
        parentTabOther <-
          maybe Tab.new pure =<< Map.lookup parent (s ^. temp . toTabOther)
        let overProjOther
              :: (Hashable k)
              => Lens' TabulatedPermissions (Map k CollectionPermission)
              -> STM ()
            overProjOther proj = do
              forM_ (Map.unfoldlM (parentTabOther ^. proj)) $ \(sId, p) ->
                Map.focus (Focus.alter (Just . maybe p (<> p))) sId (tabOther ^. proj)
        overProjOther forSpaces
        overProjOther forEntities
        overProjOther forGroups
        overProjOther forMembers
    univPerm <- fromJust <$> Map.lookup gId (s ^. temp . toTabUniverse)
    let hideSpace sId =
          Multimap.insert gId sId (s ^. temp . toSpacesHiddenTo)
        makeSpaceVisible sId =
          Multimap.delete gId sId (s ^. temp . toSpacesHiddenTo)
    case univPerm of
      CollectionPermissionWithExemption Blind _ ->
        forM_ (Set.unfoldlM (s ^. store . toSpaces)) hideSpace
      CollectionPermissionWithExemption _ True ->
        forM_ (Set.unfoldlM (s ^. store . toSpaces)) makeSpaceVisible
      CollectionPermissionWithExemption _ False ->
        -- FIXME what if the space hasn't been listed? Like how I want to make entries spotty if they rely on universe setting
        forM_ (Set.unfoldlM (s ^. store . toSpaces)) $ \sId -> do
          pMinor <- Map.lookup sId (tabOther ^. forSpaces)
          let p = deriveCollectionPermission univPerm pMinor
          if p == Blind then hideSpace sId else makeSpaceVisible sId
  liftBaseWith $ \runInBase ->
    forM_
      (Multimap.unfoldlMByKey gId (s ^. store . toGroupsNext))
      (void . runInBase . updateTabulatedPermissionsStartingAt)

resetTabulation :: TerseM STM ()
resetTabulation = do
  s <- ask
  liftBaseWith $ \runInBase ->
    forM_
      (Set.unfoldlM (s ^. store . toRoots))
      (void . runInBase . updateTabulatedPermissionsStartingAt)

loadRefsAndSubs :: VersionId -> TerseM STM ()
loadRefsAndSubs vId = do
  s <- ask
  liftBase $ do
    forM_ (Multimap.unfoldlMByKey vId (s ^. store . toReferences)) $ \refId ->
      Multimap.insert vId refId (s ^. temp . toReferencesFrom)
    forM_ (Multimap.unfoldlMByKey vId (s ^. store . toSubscriptions)) $ \subId ->
      Multimap.insert vId subId (s ^. temp . toSubscriptionsFrom)

loadForks :: EntityId -> TerseM STM ()
loadForks eId = do
  s <- ask
  liftBase $ do
    mFork <- Map.lookup eId (s ^. store . toForks)
    case mFork of
      Nothing -> pure ()
      Just forkId -> Multimap.insert eId forkId (s ^. temp . toForksFrom)

loadTempFromStore :: TerseM STM ()
loadTempFromStore = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    forM_
      (Set.unfoldlM (s ^. store . toVersions))
      (void . runInBase . loadRefsAndSubs)
    forM_ (Map.unfoldlM (s ^. store . toEntities)) $ \(eId, vs) -> do
      runInBase $ loadForks eId
      for_ vs $ \vId -> Map.insert eId vId (s ^. temp . toEntityOf)
    forM_ (Set.unfoldlM (s ^. store . toActors)) $ \aId ->
      forM_ (Multimap.unfoldlM (s ^. store . toMembers)) $ \(gId, aId') ->
        when (aId == aId') $ Multimap.insert gId aId (s ^. temp . toMemberOf)
    forM_ (Multimap.unfoldlM (s ^. store . toSpaceEntities)) $ \(sId, eId) ->
      Map.insert sId eId (s ^. temp . toSpaceOf)
  resetTabulation
