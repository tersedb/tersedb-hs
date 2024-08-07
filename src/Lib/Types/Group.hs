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

module Lib.Types.Group where

import Lib.Types.Id (GroupId)
import Lib.Types.Permission (Permission)
import Lib.Types.Monad (SheepdogM)
import Lib.Types.Store (Store, toGroups, toTabulatedPermissions, TabulatedPermissionsForGroup (..))
import Lib.Types.Store.Groups
  ( hasCycle
  , emptyGroup
  , nodes
  , universePermission
  , roots
  , outs
  , edges
  , next
  , prev
  )

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)
import System.Random.Stateful (globalStdGen, Uniform (uniformM))
import Control.Lens ((&), (^.), (.~), (%~))
import Control.Monad (void)
import Control.Monad.State (MonadState (get, put), modify)

-- TODO make a tabulation type, then update it when adjusting groups, etc.

newGroup :: SheepdogM GroupId
newGroup = do
  gId <- uniformM globalStdGen
  storeGroup gId
  pure gId

storeGroup :: MonadState Store m => GroupId -> m ()
storeGroup gId = do
  s <- get
  let groups = s ^. toGroups
  let newNodes = HM.insert gId emptyGroup (groups ^. nodes)
      newRoots = HS.insert gId (groups ^. roots)
  put $
    s & toGroups . nodes .~ newNodes
      & toGroups . roots .~ newRoots

data LinkGroupError
  = CycleDetected [GroupId]
  | DuplicateEdge GroupId GroupId
  | MultiParent GroupId
  deriving (Eq, Show, Read)

-- | Gets an initial tabulation for a specific group; assumes the group is a root
-- node, and isn't inheriting any other groups.
initTabulatedPermissionsForGroup :: MonadState Store m => GroupId -> m TabulatedPermissionsForGroup
initTabulatedPermissionsForGroup gId = do
  s <- get
  case HM.lookup gId (s ^. toGroups . nodes) of
    Nothing -> error $ "Group " <> show gId <> " doesn't exist in groups store"
    Just group -> -- TODO fetch all relevant spaces & entities as well 
      -- FIXME make a multihashmap? that way I can just `intersect` the spaces hashsets?
      pure TabulatedPermissionsForGroup
        { tabulatedPermissionsForGroupUniverse = group ^. universePermission
        , tabulatedPermissionsForGroupSpaces = mempty
        , tabulatedPermissionsForGroupEntities = mempty
        }

-- | Partial - assumes that if a prev group exists, that its record in the tabulation
-- already exists as well
updateTabulationStartingAt :: MonadState Store m => GroupId -> m ()
updateTabulationStartingAt gId = do
  initTab <- initTabulatedPermissionsForGroup gId
  s <- get
  -- doing fromJust because initTabulatedPermissionsForGroup already checks
  let group = fromJust $ HM.lookup gId (s ^. toGroups . nodes)
      parentTab :: TabulatedPermissionsForGroup
      parentTab = case group ^. prev of
        Nothing -> mempty -- root node
        Just parent -> case HM.lookup parent (s ^. toTabulatedPermissions) of
          Nothing -> error $ "Parent group " <> show parent <> " doesn't exist in tabulation"
          Just t -> t
  modify (toTabulatedPermissions %~ HM.insert gId (parentTab <> initTab))
  void . traverse updateTabulationStartingAt . HS.toList $ group ^. next


resetTabulation :: MonadState Store m => m ()
resetTabulation = do
  s <- get
  void . traverse updateTabulationStartingAt . HS.toList $ s ^. toGroups . roots


linkGroups :: MonadState Store m => GroupId -> GroupId -> m (Either LinkGroupError ())
linkGroups from to = do
  s <- get
  let groups = s ^. toGroups
  if HS.member (from, to) (groups ^. edges)
  then pure (Left (DuplicateEdge from to))
  else if HS.member to (groups ^. outs)
  then pure (Left (MultiParent to))
  else
    let newGroups = groups
          & edges .~ HS.insert (from,to) (groups ^. edges)
          & outs .~ (groups ^. outs & HS.insert to . HS.delete from)
          & roots .~ HS.delete to (groups ^. roots)
    in case hasCycle newGroups of
        Just cycle -> pure . Left $ CycleDetected cycle
        Nothing -> do
          tabPermsForFrom <- case HM.lookup from (s ^. toTabulatedPermissions) of
            Nothing -> do
              t <- initTabulatedPermissionsForGroup from
              put $ s & toTabulatedPermissions %~ HM.insert from t
              pure t
            Just t -> pure t
          solePermsForTo <- initTabulatedPermissionsForGroup to
          let tabPermsForTo = tabPermsForFrom <> solePermsForTo
          put $ 
            s & toGroups .~ newGroups
              & toTabulatedPermissions %~ HM.insert to tabPermsForTo
          updateTabulationStartingAt to
          pure (Right ())

-- TODO remove from tabulation
unlinkGroups :: MonadState Store m => GroupId -> GroupId -> m ()
unlinkGroups from to = do
  s <- get
  let groups = s ^. toGroups
      newGroups = groups
        & edges .~ HS.delete (from,to) (groups ^. edges)
        & outs .~ HS.delete to (groups ^. outs)
        & roots .~ HS.insert to (groups ^. roots)
  put (s & toGroups .~ newGroups)

-- TODO update tablulation
adjustUniversePermission :: MonadState Store m => (Permission -> Permission) -> GroupId -> m ()
adjustUniversePermission f gId = do
  s <- get
  let groups = s ^. toGroups
      newNodes = HM.adjust (universePermission %~ f) gId (groups ^. nodes)
  put (s & toGroups . nodes .~ newNodes)
