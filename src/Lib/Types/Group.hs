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
import Lib.Types.Store (toGroups, toTabulatedPermissions, TabulatedPermissionsForGroup (..))
import Lib.Types.Store.Groups
  ( hasCycle
  , emptyGroup
  , nodes
  , universePermission
  , roots
  , outs
  , edges
  )

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import System.Random.Stateful (globalStdGen, Uniform (uniformM))
import Control.Lens ((&), (^.), (.~), (%~))
import Control.Monad.State (MonadState (get, put))

-- TODO make a tabulation type, then update it when adjusting groups, etc.

newGroup :: SheepdogM GroupId
newGroup = do
  s <- get
  let groups = s ^. toGroups
  gId <- uniformM globalStdGen
  let newNodes = HM.insert gId emptyGroup (groups ^. nodes)
      newRoots = HS.insert gId (groups ^. roots)
  put $
    s & toGroups . nodes .~ newNodes
      & toGroups . roots .~ newRoots
  pure gId

data LinkGroupError
  = CycleDetected
  | DuplicateEdge GroupId GroupId
  | MultiParent GroupId

-- | Gets an initial tabulation for a specific group; assumes the group is a root
-- node, and isn't inheriting any other groups.
initTabulatedPermissionsForGroup :: GroupId -> SheepdogM TabulatedPermissionsForGroup
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

linkGroups :: GroupId -> GroupId -> SheepdogM (Either LinkGroupError ())
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
          & outs .~ HS.insert to (groups ^. outs)
          & roots .~ HS.delete to (groups ^. roots)
    in if hasCycle newGroups
      then pure (Left CycleDetected)
      else do
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
        -- updateTabulationStartingAt to
        pure (Right ())

unlinkGroups :: GroupId -> GroupId -> SheepdogM ()
unlinkGroups from to = do
  s <- get
  let groups = s ^. toGroups
      newGroups = groups
        & edges .~ HS.delete (from,to) (groups ^. edges)
        & outs .~ HS.delete to (groups ^. outs)
        & roots .~ HS.insert to (groups ^. roots)
  put (s & toGroups .~ newGroups)

-- TODO update tablulation
adjustUniversePermission :: (Permission -> Permission) -> GroupId -> SheepdogM ()
adjustUniversePermission f gId = do
  s <- get
  let groups = s ^. toGroups
      newNodes = HM.adjust (universePermission %~ f) gId (groups ^. nodes)
  put (s & toGroups . nodes .~ newNodes)
