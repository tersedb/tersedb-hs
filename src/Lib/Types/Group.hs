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

import Lib.Types.Id (GroupId, SpaceId, EntityId)
import Lib.Types.Permission (Permission (Blind))
import Lib.Types.Monad (SheepdogM)
import Lib.Types.Store
  ( Store
  , toGroups
  , toSpaces
  , toEntities
  , toTabulatedPermissions
  , toSpacePermissions
  , toEntityPermissions
  , TabulatedPermissionsForGroup (..)
  )
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Entity (Entity (..))
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

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromJust, fromMaybe)
import Data.Foldable (traverse_)
import System.Random.Stateful (globalStdGen, Uniform (uniformM))
import Control.Lens ((&), (^.), (.~), (%~))
import Control.Monad.State (MonadState (get, put), modify)


newGroup :: SheepdogM GroupId
newGroup = do
  gId <- uniformM globalStdGen
  storeGroup gId
  pure gId

-- | Sets the group to empty
storeGroup :: MonadState Store m => GroupId -> m ()
storeGroup gId = do
  modify $ toGroups . nodes %~ HM.insert gId emptyGroup
  modify $ toGroups . roots %~ HS.insert gId

newSpace :: SheepdogM SpaceId
newSpace = do
  sId <- uniformM globalStdGen
  storeSpace sId
  pure sId

-- | Sets the space to empty
storeSpace :: MonadState Store m => SpaceId -> m ()
storeSpace sId = do
  modify $ toSpaces %~ HM.insert sId mempty

newEntity :: SpaceId -> SheepdogM EntityId
newEntity sId = do
  eId <- uniformM globalStdGen
  storeEntity eId (sId)
  pure eId

storeEntity :: MonadState Store m => EntityId -> (SpaceId) -> m ()
storeEntity eId sId = do
  modify $ toEntities %~ HM.insert eId Entity
    { entitySpace = sId
    }
  modify $ toSpaces %~ HM.adjust (entities %~ HS.insert eId) sId

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
    Just group -> do
      -- TODO fetch all relevant spaces & entities as well 
      pure TabulatedPermissionsForGroup
        { tabulatedPermissionsForGroupUniverse = group ^. universePermission
        , tabulatedPermissionsForGroupSpaces = fromMaybe mempty $ HM.lookup gId (s ^. toSpacePermissions)
        , tabulatedPermissionsForGroupEntities = mempty
        }

-- | updates the tab with a possibly erroneous cache for the parent if its missing
-- - would only work if the parent happened to be the root node.
updateTabulationStartingAt :: MonadState Store m => GroupId -> m ()
updateTabulationStartingAt gId = do
  s <- get
  initTab <- initTabulatedPermissionsForGroup gId
  let group = fromJust $ HM.lookup gId (s ^. toGroups . nodes)
  parentTab <- case group ^. prev of
    Nothing -> pure mempty -- root node
    Just parent -> case HM.lookup parent (s ^. toTabulatedPermissions) of
      Nothing -> do
        t <- initTabulatedPermissionsForGroup parent
        modify $ toTabulatedPermissions %~ HM.insert parent t
        pure t
      Just t -> pure t
  let newTab = parentTab <> initTab
  case HM.lookup gId (s ^. toTabulatedPermissions) of
    Just oldTab | newTab == oldTab -> pure ()
    _ -> do
      -- doing fromJust because initTabulatedPermissionsForGroup already checks
      modify (toTabulatedPermissions %~ HM.insert gId newTab)
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

adjustUniversePermission :: MonadState Store m => (Permission -> Permission) -> GroupId -> m ()
adjustUniversePermission f gId = do
  modify $ toGroups . nodes %~ HM.adjust (universePermission %~ f) gId
  updateTabulationStartingAt gId

adjustSpacePermission :: MonadState Store m => (Permission -> Permission) -> GroupId -> SpaceId -> m ()
adjustSpacePermission f gId sId = do
  let adjustSpaces :: Maybe (HashMap SpaceId Permission) -> Maybe (HashMap SpaceId Permission)
      adjustSpaces xs = case xs of
        Just xs -> Just (HM.alter (maybe (Just initF) (Just . f)) sId xs)
        Nothing -> Just (HM.singleton sId initF)
        where
          initF = f Blind
  modify $ toSpacePermissions %~ HM.alter adjustSpaces gId
  updateTabulationStartingAt gId

adjustEntityPermission :: MonadState Store m => (Permission -> Permission) -> GroupId -> SpaceId -> m ()
adjustEntityPermission f gId sId = do
  let adjustSpaces :: Maybe (HashMap SpaceId Permission) -> Maybe (HashMap SpaceId Permission)
      adjustSpaces xs = case xs of
        Just xs -> Just (HM.alter (maybe (Just initF) (Just . f)) sId xs)
        Nothing -> Just (HM.singleton sId initF)
        where
          initF = f Blind
  modify $ toEntityPermissions %~ HM.alter adjustSpaces gId
  updateTabulationStartingAt gId
