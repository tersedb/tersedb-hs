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

module Lib.Types.Store.Groups where

import Lib.Types.Id (GroupId)
import Lib.Types.Permission (Permission (Blind))

import Data.Aeson (ToJSON, FromJSON)
import Deriving.Aeson.Stock (PrefixedSnake, Generic, CustomJSON (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Monoid (Any (..))
import Control.Lens ((^.))
import Control.Lens.TH (makeLensesFor)
import Control.Monad.State (MonadState (get), modify, evalState)
import Control.Monad.Extra (mconcatMapM)



data Group = Group
  { groupPrev :: Maybe GroupId
  , groupNext :: HashSet GroupId
  , groupUniversePermission :: Permission
  } deriving (Eq, Generic)
  deriving (ToJSON, FromJSON)
  via PrefixedSnake "group" Group
makeLensesFor
  [ ("groupPrev", "prev")
  , ("groupNext", "next")
  , ("groupUniversePermission", "universePermission")
  ] ''Group

emptyGroup :: Group
emptyGroup = Group
  { groupPrev = Nothing
  , groupNext = mempty
  , groupUniversePermission = Blind
  }

data Groups = Groups
  { groupsHashMap :: HashMap GroupId Group
  , groupsRoots :: HashSet GroupId -- FIXME roots are just the complement of outs?
  , groupsEdges :: HashSet (GroupId, GroupId)
  , groupsOuts :: HashSet GroupId
  }
makeLensesFor
  [ ("groupsHashMap", "nodes")
  , ("groupsRoots", "roots")
  , ("groupsEdges", "edges")
  , ("groupsOuts", "outs")
  ] ''Groups


-- FIXME make Bool actually Maybe [GroupId], to represent the cycle
hasCycle :: Groups -> Bool
hasCycle groups =
  let dfs :: MonadState (HashSet GroupId) m => GroupId -> m Any
      dfs node = do
        trail <- get
        if (node `HS.member` trail)
        then pure (Any True)
        else do
          modify (HS.insert node)
          let group = case HM.lookup node (groups ^. nodes) of
                Nothing -> error $ "Broken graph, " <> show node <> " doesn't exist"
                Just g -> g
          res <- mconcatMapM dfs (HS.toList (group ^. next))
          modify (HS.delete node)
          pure res
      inState :: MonadState (HashSet GroupId) m => m Bool
      inState = getAny <$> mconcatMapM dfs (HS.toList (groups ^. roots))
  in  evalState inState mempty
