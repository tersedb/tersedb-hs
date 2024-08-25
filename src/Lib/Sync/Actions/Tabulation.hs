{-
TerseDB - Entity Management System
Copyright (C) 2024  Athan Clark

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

You can reach me at athan.clark@gmail.com.
-}

module Lib.Sync.Actions.Tabulation where

import Control.Lens (at, ix, non, (&), (.~), (?~), (^.))
import Control.Monad.Extra (when)
import Control.Monad.State (MonadState (get, put), State, execState, modify)
import Data.Foldable (foldlM, for_, traverse_)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import Lib.Sync.Types.Store (
  Shared (..),
  Store,
  Temp,
  emptyTemp,
  store,
  temp,
  toActors,
  toEntities,
  toEntityOf,
  toEntityPermissions,
  toForksFrom,
  toGroupPermissions,
  toGroups,
  toMemberOf,
  toMemberPermissions,
  toReferencesFrom,
  toSpaceOf,
  toSpacePermissions,
  toSpaces,
  toSpacesHiddenTo,
  toSubscriptionsFrom,
  toTabulatedGroups,
  toVersions,
 )
import Lib.Sync.Types.Store.Entity (fork, versions)
import Lib.Sync.Types.Store.Groups (
  members,
  next,
  nodes,
  organizationPermission,
  prev,
  recruiterPermission,
  roots,
  universePermission,
 )
import Lib.Sync.Types.Store.Tabulation.Group (
  TabulatedPermissionsForGroup (..),
  forSpaces,
  forUniverse,
 )
import Lib.Sync.Types.Store.Version (
  references,
  subscriptions,
 )
import Lib.Types.Id (EntityId, GroupId, VersionId)
import Lib.Types.Permission (
  CollectionPermission (Blind),
  CollectionPermissionWithExemption (..),
  escalate,
 )

{- | Gets an initial tabulation for a specific group; assumes the group is a root
node, and isn't inheriting any other groups.
-}
initTabulatedPermissionsForGroup
  :: (MonadState Shared m)
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

{- | updates the tab with a possibly erroneous cache for the parent if its missing;
usage would only be semantically correct if the parent happened to be the root node.
-}
updateTabulationStartingAt :: (MonadState Shared m) => GroupId -> m ()
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
      do
        let allSpaces = HM.keysSet (s ^. store . toSpaces)
            (spacesHidden, spacesVisible) = case newTab ^. forUniverse of
              CollectionPermissionWithExemption Blind _ -> (allSpaces, mempty)
              CollectionPermissionWithExemption _ True -> (mempty, allSpaces)
              CollectionPermissionWithExemption _ False ->
                let notVisible = HM.keysSet (HM.filter (== Blind) (newTab ^. forSpaces))
                 in (notVisible, allSpaces `HS.difference` notVisible)
        for_ spacesHidden $ \sId ->
          modify $ temp . toSpacesHiddenTo . at sId . non mempty . at gId ?~ ()
        for_ spacesVisible $ \sId -> do
          s <- get
          if (s ^. temp . toSpacesHiddenTo . at sId) == Just (HS.singleton gId)
            then modify $ temp . toSpacesHiddenTo . at sId .~ Nothing
            else modify $ temp . toSpacesHiddenTo . ix sId . at gId .~ Nothing

      modify $ temp . toTabulatedGroups . at gId ?~ newTab
      traverse_ updateTabulationStartingAt (group ^. next)

resetTabulation :: (MonadState Shared m) => m ()
resetTabulation = do
  s <- get
  traverse_ updateTabulationStartingAt . HS.toList $ s ^. store . toGroups . roots

{- | Only applies explicit references & subscriptions - not implicit references
due to forking or continuity of versions
-}
loadRefsAndSubs
  :: VersionId
  -> Store
  -> Temp
  -> Either VersionId Temp
loadRefsAndSubs vId s t =
  case s ^. toVersions . at vId of
    Nothing -> Left vId
    Just v ->
      pure
        (foldr storeSubs (foldr storeRefs t (v ^. references)) (v ^. subscriptions))
 where
  storeRefs refId t =
    t
      & toReferencesFrom . at refId . non mempty . at vId ?~ ()

  storeSubs subId t =
    t
      & toSubscriptionsFrom . at subId . non mempty . at vId ?~ ()

loadForks
  :: EntityId
  -> Store
  -> Temp
  -> Temp
loadForks eId s t =
  let go e t = case e ^. fork of
        Nothing -> t
        Just refId -> t & toForksFrom . at refId . non mempty . at eId ?~ ()
   in foldr go t (HM.lookup eId (s ^. toEntities))

tempFromStore :: Store -> Temp
tempFromStore s = execState go emptyTemp
 where
  go :: State Temp ()
  go = do
    loadVersions
    loadForks'
    loadMembers
    loadSpaceOf
    loadEntityOf
    t <- get
    put $ execState resetTabulation (Shared s t) ^. temp

  loadVersions :: State Temp ()
  loadVersions =
    for_ (HM.keysSet (s ^. toVersions)) $ \vId -> do
      t <- get
      case loadRefsAndSubs vId s t of
        Left e -> error (show e)
        Right t' -> put t'

  loadForks' :: State Temp ()
  loadForks' =
    for_ (HM.keysSet $ s ^. toEntities) $ \eId ->
      modify (loadForks eId s)

  loadMembers :: State Temp ()
  loadMembers =
    for_ (s ^. toActors) $ \aId ->
      for_ (HM.toList $ s ^. toGroups . nodes) $ \(gId, g) ->
        when (aId `HS.member` (g ^. members)) $
          modify $
            toMemberOf . at aId . non mempty . at gId ?~ ()

  loadSpaceOf :: State Temp ()
  loadSpaceOf =
    for_ (HM.toList $ s ^. toSpaces) $ \(sId, es) ->
      for_ es $ \eId ->
        modify $ toSpaceOf . at eId ?~ sId

  loadEntityOf :: State Temp ()
  loadEntityOf =
    for_ (HM.toList $ s ^. toEntities) $ \(eId, e) ->
      for_ (e ^. versions) $ \vId ->
        modify $ toEntityOf . at vId ?~ eId
