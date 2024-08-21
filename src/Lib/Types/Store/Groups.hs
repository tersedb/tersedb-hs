{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Types.Store.Groups where

import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (
    CollectionPermission,
    CollectionPermissionWithExemption,
 )

import Control.Lens ((^.))
import Control.Lens.TH (makeLensesFor)
import Control.Monad.Extra (mconcatMapM)
import Control.Monad.State (MonadState (get, put), evalState, modify)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (uncons)
import Data.Monoid (First (..))
import Deriving.Aeson.Stock (CustomJSON (..), Generic, PrefixedSnake)

data Group = Group
    { groupPrev :: Maybe GroupId
    , groupNext :: HashSet GroupId
    , groupUniversePermission :: CollectionPermissionWithExemption -- applies to BRCUD for spaces
    , groupOrganizationPermission :: CollectionPermissionWithExemption -- applies to BRCUD for groups
    , groupRecruiterPermission :: CollectionPermission -- applies to BRCUD for actors
    , groupMembers :: HashSet ActorId
    }
    deriving (Eq, Generic, Show, Read)
    deriving
        (ToJSON, FromJSON)
        via PrefixedSnake "group" Group
makeLensesFor
    [ ("groupPrev", "prev")
    , ("groupNext", "next")
    , ("groupUniversePermission", "universePermission")
    , ("groupOrganizationPermission", "organizationPermission")
    , ("groupRecruiterPermission", "recruiterPermission")
    , ("groupMembers", "members")
    ]
    ''Group

emptyGroup :: Group
emptyGroup =
    Group
        { groupPrev = Nothing
        , groupNext = mempty
        , groupUniversePermission = minBound
        , groupOrganizationPermission = minBound
        , groupRecruiterPermission = minBound
        , groupMembers = mempty
        }

-- | How groups interact with one another - inheritance
data Groups = Groups
    { groupsHashMap :: HashMap GroupId Group
    , groupsRoots :: HashSet GroupId -- FIXME roots are just the complement of outs?
    , groupsEdges :: HashSet (GroupId, GroupId)
    , groupsOuts :: HashSet GroupId -- FIXME rename to "has parent"
    }
    deriving (Eq, Generic, Show, Read)

makeLensesFor
    [ ("groupsHashMap", "nodes")
    , ("groupsRoots", "roots")
    , ("groupsEdges", "edges")
    , ("groupsOuts", "outs")
    ]
    ''Groups

emptyGroups :: Groups
emptyGroups =
    Groups
        { groupsHashMap = mempty
        , groupsRoots = mempty
        , groupsEdges = mempty
        , groupsOuts = mempty
        }

-- FIXME return Maybe [GroupId] instead of Bool, to represent the cycle
hasCycle :: Groups -> Maybe [GroupId]
hasCycle groups = evalState overRoots mempty
  where
    overRoots :: (MonadState [GroupId] m) => m (Maybe [GroupId])
    overRoots = getFirst <$> mconcatMapM dfs (HS.toList (groups ^. roots))

    dfs :: (MonadState [GroupId] m) => GroupId -> m (First [GroupId])
    dfs node = do
        trail <- get
        if (node `elem` trail)
            then pure . First . Just $ node : trail
            else do
                modify (node :)
                let group = case HM.lookup node (groups ^. nodes) of
                        Nothing -> error $ "Broken graph, " <> show node <> " doesn't exist"
                        Just g -> g
                res <- mconcatMapM dfs (HS.toList (group ^. next))
                trail <- get
                case uncons trail of
                    Nothing -> error "Broken hasCycle algorithm - empty stack when attemping uncons"
                    Just (_, trail') -> do
                        put trail'
                        pure res
