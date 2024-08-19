module Lib.Actions.Tabulation where

import Lib.Types.Permission
  ( escalate
  , CollectionPermission (Blind)
  , CollectionPermissionWithExemption (..)
  )
import Lib.Types.Id (GroupId, EntityId, VersionId)
import Lib.Types.Store
  ( Shared (..)
  , Store
  , Temp
  , emptyTemp
  , store
  , temp
  , toGroups
  , toEntities
  , toVersions
  , toSpaces
  , toTabulatedGroups
  , toSpacePermissions
  , toEntityPermissions
  , toGroupPermissions
  , toMemberPermissions
  , toReferencesFrom
  , toSubscriptionsFrom
  , toForksFrom
  , toSpacesHiddenTo
  )
import Lib.Types.Store.Tabulation.Group
  ( TabulatedPermissionsForGroup (..)
  , forSpaces
  , forUniverse
  )
import Lib.Types.Store.Groups
  ( nodes
  , universePermission
  , organizationPermission
  , recruiterPermission
  , roots
  , next
  , prev
  )
import Lib.Types.Store.Version
  ( Version
  , entity
  , references
  , subscriptions
  )
import Lib.Types.Store.Entity (space, fork, versions)

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Foldable (traverse_, for_, foldlM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Control.Lens ((^.), (.~), (&), at, non, ix)
import Control.Monad (void)
import Control.Monad.State (MonadState (get, put), modify, State, execState)


-- | Gets an initial tabulation for a specific group; assumes the group is a root
-- node, and isn't inheriting any other groups.
initTabulatedPermissionsForGroup
  :: MonadState Shared m
  => GroupId
  -> m TabulatedPermissionsForGroup
initTabulatedPermissionsForGroup gId = do
  s <- get
  pure $ case s ^. store . toGroups . nodes . at gId of
    Nothing -> error $ "Group " <> show gId <> " doesn't exist in groups store"
    Just group -> do
      let univPerms = group ^. universePermission
          orgPerms = group ^. organizationPermission
          spacePerms = fromMaybe mempty (s ^. store . toSpacePermissions . at gId)
          groupPerms = fromMaybe mempty (s ^. store . toGroupPermissions . at gId)
      TabulatedPermissionsForGroup
        { tabulatedPermissionsForGroupUniverse = univPerms
        , tabulatedPermissionsForGroupOrganization = orgPerms
        , tabulatedPermissionsForGroupRecruiter = group ^. recruiterPermission
        , tabulatedPermissionsForGroupSpaces = fmap (escalate univPerms) spacePerms
        , tabulatedPermissionsForGroupEntities =
            fromMaybe mempty (s ^. store . toEntityPermissions . at gId)
        , tabulatedPermissionsForGroupGroups = fmap (escalate orgPerms) groupPerms
        , tabulatedPermissionsForGroupMembers =
            fromMaybe mempty (s ^. store . toMemberPermissions . at gId)
        }

-- | updates the tab with a possibly erroneous cache for the parent if its missing;
-- usage would only be semantically correct if the parent happened to be the root node.
updateTabulationStartingAt :: MonadState Shared m => GroupId -> m ()
updateTabulationStartingAt gId = do
  s <- get
  let group = case s ^. store . toGroups . nodes . at gId of
        Nothing -> error $ "Group " <> show gId <> " doesn't exist in groups store"
        Just g -> g
  newTab <- do
    initTab <- initTabulatedPermissionsForGroup gId
    parentTab <- case group ^. prev of
      Nothing -> pure mempty -- root node
      Just parent -> case s ^. temp . toTabulatedGroups . at parent of
        Nothing -> do
          t <- initTabulatedPermissionsForGroup parent
          modify $ temp . toTabulatedGroups . at parent .~ Just t
          pure t
        Just t -> pure t
    pure (parentTab <> initTab)
  case s ^. temp . toTabulatedGroups . at gId of
    Just oldTab | newTab == oldTab -> pure ()
    _ -> do
      do  let allSpaces = HM.keysSet (s ^. store . toSpaces)
              (spacesHidden, spacesVisible) = case newTab ^. forUniverse of
                CollectionPermissionWithExemption Blind _ -> (allSpaces, mempty)
                CollectionPermissionWithExemption _ True -> (mempty, allSpaces)
                CollectionPermissionWithExemption _ False ->
                  let notVisible = HM.keysSet (HM.filter (== Blind) (newTab ^. forSpaces))
                  in  (notVisible, allSpaces `HS.difference` notVisible)
          for_ spacesHidden $ \sId ->
            modify $ temp . toSpacesHiddenTo . at sId . non mempty . at gId .~ Just ()
          for_ spacesVisible $ \sId -> do
            s <- get
            if (s ^. temp . toSpacesHiddenTo . at sId) == Just (HS.singleton gId)
            then modify $ temp . toSpacesHiddenTo . at sId .~ Nothing
            else modify $ temp . toSpacesHiddenTo . ix sId . at gId .~ Nothing

      modify $ temp . toTabulatedGroups . at gId .~ Just newTab
      traverse_ updateTabulationStartingAt (group ^. next)


resetTabulation :: MonadState Shared m => m ()
resetTabulation = do
  s <- get
  traverse_ updateTabulationStartingAt . HS.toList $ s ^. store . toGroups . roots


-- | Only applies explicit references & subscriptions - not implicit references
-- due to forking or continuity of versions
loadRefsAndSubs
  :: VersionId
  -> Store
  -> Temp
  -> Either VersionId Temp
loadRefsAndSubs vId s t =
  case s ^. toVersions . at vId of
    Nothing -> Left vId
    Just v ->
      pure (foldr storeSubs (foldr storeRefs t (v ^. references)) (v ^. subscriptions))
  where
    storeRefs refId t = t
      & toReferencesFrom . at refId . non mempty . at vId .~ Just ()

    storeSubs subId t = t
      & toSubscriptionsFrom . at subId . non mempty . at vId .~ Just ()


loadForks
  :: EntityId
  -> Store
  -> Temp
  -> Temp
loadForks eId s t =
  let go (eId, e) t = case e ^. fork of
        Nothing -> t
        Just refId -> t & toForksFrom . at refId . non mempty . at eId .~ Just ()
  in  foldr go t (HM.toList (s ^. toEntities))


tempFromStore :: Store -> Temp
tempFromStore s = execState go emptyTemp
  where
    go :: State Temp ()
    go = do
      loadVersions
      loadForks'
      t <- get
      put $ execState resetTabulation (Shared s t) ^. temp

    loadVersions :: State Temp ()
    loadVersions =
      for_ (HM.toList (s ^. toVersions)) $ \(vId, v) -> do
        t <- get
        case loadRefsAndSubs vId s t of
          Left e -> error (show e)
          Right t' -> put t'

    loadForks' :: State Temp ()
    loadForks' =
      for_ (HM.keysSet $ s ^. toEntities) $ \eId ->
        modify (loadForks eId s)
