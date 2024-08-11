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
import Lib.Types.Id (GroupId)
import Lib.Types.Store
  ( Shared
  , store
  , temp
  , toGroups
  , toTabulatedGroups
  , toSpacePermissions
  , toEntityPermissions
  , toGroupPermissions
  , toMemberPermissions
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

import qualified Data.HashSet as HS
import Data.Maybe (fromJust, fromMaybe)
import Data.Foldable (traverse_)
import Control.Lens ((^.), (.~), at)
import Control.Monad.State (MonadState (get), modify)


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
