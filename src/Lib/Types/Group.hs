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
import Data.Foldable (traverse_)
import System.Random.Stateful (globalStdGen, Uniform (uniformM))
import Control.Lens ((&), (^.), (.~), (%~))
import Control.Monad.State (MonadState (get, put), modify)

-- TODO make a tabulation type, then update it when adjusting groups, etc.

newGroup :: SheepdogM GroupId
newGroup = do
  gId <- uniformM globalStdGen
  storeGroup gId
  pure gId

storeGroup :: MonadState Store m => GroupId -> m ()
storeGroup gId = do
  modify $ toGroups . nodes %~ HM.insert gId emptyGroup
  modify $ toGroups . roots %~ HS.insert gId
  -- FIXME does a singleton count as an out as well?

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

-- | updates the tab with a possibly erroneous cache for the parent if its missing
-- - would only work if the parent happened to be the root node.
updateTabulationStartingAt :: MonadState Store m => GroupId -> m ()
updateTabulationStartingAt gId = do
  initTab <- initTabulatedPermissionsForGroup gId
  s <- get
  -- doing fromJust because initTabulatedPermissionsForGroup already checks
  let group = fromJust $ HM.lookup gId (s ^. toGroups . nodes)
  parentTab <- case group ^. prev of
    Nothing -> pure mempty -- root node
    Just parent -> case HM.lookup parent (s ^. toTabulatedPermissions) of
      Nothing -> do
        t <- initTabulatedPermissionsForGroup parent
        modify $ toTabulatedPermissions %~ HM.insert parent t
        pure t
      Just t -> pure t
  modify (toTabulatedPermissions %~ HM.insert gId (parentTab <> initTab))
  traverse_ updateTabulationStartingAt . HS.toList $ group ^. next


resetTabulation :: MonadState Store m => m ()
resetTabulation = do
  s <- get
  traverse_ updateTabulationStartingAt . HS.toList $ s ^. toGroups . roots

-- | Loads the parent's untabulated permissions if it's not already tabulated!
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
          & edges %~ HS.insert (from,to)
          & outs %~ (HS.insert to . HS.delete from)
          & roots %~ HS.delete to
          & nodes %~ (HM.adjust (next %~ HS.insert to) from . HM.adjust (prev .~ Just from) to)
    in case hasCycle newGroups of
        Just cycle -> pure . Left $ CycleDetected cycle
        Nothing -> do
          modify $ toGroups .~ newGroups
          updateTabulationStartingAt to
          pure (Right ())

unlinkGroups :: MonadState Store m => GroupId -> GroupId -> m ()
unlinkGroups from to = do
  s <- get
  let groups = s ^. toGroups
      newGroups = groups
        & edges %~ HS.delete (from,to) 
        & outs %~ (HS.delete to . HS.insert from)
        & roots %~ HS.insert to
        & nodes %~ (HM.adjust (next %~ HS.delete to) from . HM.adjust (prev .~ Nothing) to)
  put $ s
      & toGroups .~ newGroups
  updateTabulationStartingAt to

-- TODO update tablulation
adjustUniversePermission :: MonadState Store m => (Permission -> Permission) -> GroupId -> m ()
adjustUniversePermission f gId =
  modify $ toGroups . nodes %~ HM.adjust (universePermission %~ f) gId
