{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
  , DerivingVia
  , DataKinds
  , DeriveGeneric
  , RankNTypes
  , TemplateHaskell
  , FlexibleContexts
  #-}

module Lib.Actions.Tabulation where

import Lib.Types.Permission (escalate)
import Lib.Types.Id (GroupId, EntityId, VersionId)
import Lib.Types.Store
  ( Shared (..)
  , Store
  , Temp (..)
  , store
  , temp
  , toGroups
  , toEntities
  , toVersions
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
  )
import Lib.Types.Store.Tabulation.Group (TabulatedPermissionsForGroup (..))
import Lib.Types.Store.Groups
  ( nodes
  , universePermission
  , organizationPermission
  , recruiterPermission
  , roots
  , next
  , prev
  )
import Lib.Types.Store.Version (Version, entity, references, subscriptions)
import Lib.Types.Store.Entity (space)

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust, fromMaybe)
import Data.Foldable (traverse_, for_, foldlM)
import Control.Lens ((^.), (.~), (&), at, non)
import Control.Monad.State (MonadState (get, put), modify, State, execState)


-- | Gets an initial tabulation for a specific group; assumes the group is a root
-- node, and isn't inheriting any other groups.
initTabulatedPermissionsForGroup :: MonadState Shared m => GroupId -> m TabulatedPermissionsForGroup
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
        , tabulatedPermissionsForGroupEntities = fromMaybe mempty (s ^. store . toEntityPermissions . at gId)
        , tabulatedPermissionsForGroupGroups = fmap (escalate orgPerms) groupPerms 
        , tabulatedPermissionsForGroupMembers = fromMaybe mempty (s ^. store . toMemberPermissions . at gId)
        }

-- | updates the tab with a possibly erroneous cache for the parent if its missing
-- - would only work if the parent happened to be the root node.
updateTabulationStartingAt :: MonadState Shared m => GroupId -> m ()
updateTabulationStartingAt gId = do
  s <- get
  initTab <- initTabulatedPermissionsForGroup gId
  let group = fromJust (s ^. store . toGroups . nodes . at gId)
  parentTab <- case group ^. prev of
    Nothing -> pure mempty -- root node
    Just parent -> case s ^. temp . toTabulatedGroups . at parent of
      Nothing -> do
        t <- initTabulatedPermissionsForGroup parent
        modify $ temp . toTabulatedGroups . at parent .~ Just t
        pure t
      Just t -> pure t
  let newTab = parentTab <> initTab
  case s ^. temp . toTabulatedGroups . at gId of
    Just oldTab | newTab == oldTab -> pure ()
    _ -> do
      -- doing fromJust because initTabulatedPermissionsForGroup already checks
      modify $ temp . toTabulatedGroups . at gId .~ Just newTab
      traverse_ updateTabulationStartingAt . HS.toList $ group ^. next


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
  let storeRefs t refId =
        let t' = t & toReferencesFrom . at refId . non mempty . at vId .~ Just ()
        in  case s ^. toVersions . at refId of
              Nothing -> Left (ReferenceVersionNotFound refId)
              Just refV ->
                let t = t' & toReferencesFromEntities . at (refV ^. entity) . non mempty . at vId .~ Just ()
                in  case s ^. toEntities . at (refV ^. entity) of
                      Nothing -> Left . ReferenceEntityNotFound $ refV ^. entity
                      Just refE ->
                        pure $ t
                          & toReferencesFromSpaces . at (refE ^. space) . non mempty . at vId .~ Just ()
  t' <- foldlM storeRefs t (HS.toList $ v ^. references)
  let storeSubs t subId =
        let t' = t & toSubscriptionsFrom . at subId . non mempty . at vId .~ Just ()
        in  case s ^. toEntities . at subId of
              Nothing -> Left (SubscriptionEntityNotFound subId)
              Just subE ->
                pure $ t' & toSubscriptionsFromSpaces . at (subE ^. space) . non mempty . at vId .~ Just ()
  foldlM storeSubs t' (HS.toList $ v ^. subscriptions)


tempFromStore :: Store -> Temp
tempFromStore s = execState go (Temp mempty mempty mempty mempty mempty mempty)
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
