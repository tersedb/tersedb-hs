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
  , toReferencesFromEntities
  , toReferencesFromSpaces
  , toSubscriptionsFrom
  , toSubscriptionsFromSpaces
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
  , prevVersion
  )
import Lib.Types.Store.Entity (space)

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Foldable (traverse_, for_, foldlM)
import Control.Lens ((^.), (.~), (&), at, non, ix)
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
          -- let spacesHidden = HM.keysSet (s ^. store . toSpaces) `HS.difference` spacesVisible
          --     spacesVisible = HM.keysSet (HM.filter (> Blind) (newTab ^. forSpaces))
          for_ spacesHidden $ \sId ->
            modify $ temp . toSpacesHiddenTo . at sId . non mempty . at gId .~ Just ()
          for_ spacesVisible $ \sId -> do
            s <- get
            if (s ^. temp . toSpacesHiddenTo . at sId) == Just (HS.singleton gId)
            then modify $ temp . toSpacesHiddenTo . at sId .~ Nothing
            else modify $ temp . toSpacesHiddenTo . ix sId . at gId .~ Nothing

      -- do  let (groupsHidden, groupsVisible) = case newTab ^. forOrganization of
      --           CollectionPermissionWithExemption Blind _ ->
      --             let visible = HM.keysSet (HM.filter (> Blind) (newTab ^. forGroups))
      --             in  (allWithoutSelf `HS.difference` visible, visible)
      --           CollectionPermissionWithExemption _ True ->
      --             (mempty, allWithoutSelf)
      --           CollectionPermissionWithExemption _ False ->
      --             let notVisible = HM.keysSet (HM.filter (== Blind) (newTab ^. forGroups))
      --             in  (notVisible, allWithoutSelf `HS.difference` notVisible)
      --           where
      --             allWithoutSelf = HS.delete gId $ HM.keysSet (s ^. store . toGroups . nodes)
      --     for_ groupsHidden $ \gId' ->
      --       modify $ temp . toGroupsHiddenTo . at gId' . non mempty . at gId .~ Just ()
      --     for_ groupsVisible $ \gId' ->
      --       if (s ^. temp . toGroupsHiddenTo . at gId') == Just (HS.singleton gId)
      --       then modify $ temp . toGroupsHiddenTo . at gId' .~ Nothing
      --       else modify $ temp . toGroupsHiddenTo . ix gId' . at gId .~ Nothing

      -- doing fromJust because initTabulatedPermissionsForGroup already checks
      modify $ temp . toTabulatedGroups . at gId .~ Just newTab
      traverse_ updateTabulationStartingAt (group ^. next)


resetTabulation :: MonadState Shared m => m ()
resetTabulation = do
  s <- get
  traverse_ updateTabulationStartingAt . HS.toList $ s ^. store . toGroups . roots


data LoadRefsAndSubsError
  = ReferenceVersionNotFound VersionId
  | ReferenceEntityNotFound EntityId
  | SubscriptionEntityNotFound EntityId
  deriving (Eq, Show, Read)

loadRefsAndSubs :: VersionId -> Version -> Store -> Temp -> Either LoadRefsAndSubsError Temp
loadRefsAndSubs vId v s t = do
  let refs = v ^. references
  t' <- foldlM storeRefs t . HS.toList $
    maybe refs (flip HS.insert refs) (v ^. prevVersion)
  foldlM storeSubs t' (HS.toList $ v ^. subscriptions)
  where
    storeRefs t refId = case s ^. toVersions . at refId of
      Nothing -> Left (ReferenceVersionNotFound refId)
      Just refV -> case s ^. toEntities . at (refV ^. entity) of
        Nothing -> Left . ReferenceEntityNotFound $ refV ^. entity
        Just refE ->
          pure $ t
               & toReferencesFromSpaces . at (refE ^. space) . non mempty . at vId .~ Just ()
               & toReferencesFromEntities . at (refV ^. entity) . non mempty . at vId .~ Just ()
               & toReferencesFrom . at refId . non mempty . at vId .~ Just ()

    storeSubs t subId = case s ^. toEntities . at subId of
      Nothing -> Left (SubscriptionEntityNotFound subId)
      Just subE ->
        pure $ t
             & toSubscriptionsFromSpaces . at (subE ^. space) . non mempty . at vId .~ Just ()
             & toSubscriptionsFrom . at subId . non mempty . at vId .~ Just ()


tempFromStore :: Store -> Temp
tempFromStore s = execState go emptyTemp
  where
    go :: State Temp ()
    go = do
      loadVersions
      loadTabulationGroups

    loadTabulationGroups :: State Temp ()
    loadTabulationGroups = do
      t <- get
      let shared = execState resetTabulation (Shared s t)
      put $ shared ^. temp

    loadVersions :: State Temp ()
    loadVersions =
      for_ (HM.toList (s ^. toVersions)) $ \(vId, v) -> do
        t <- get
        case loadRefsAndSubs vId v s t of
          Left e -> error (show e)
          Right t' -> put t'
