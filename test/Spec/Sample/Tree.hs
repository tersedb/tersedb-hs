{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Sample.Tree where

import Lib.Actions.Safe (emptyShared)
import Lib.Actions.Safe.Store (storeGroup)
import Lib.Actions.Safe.Update.Group (
  setGroupPermission,
  setMemberPermission,
  setOrganizationPermission,
  setRecruiterPermission,
  setUniversePermission,
 )
import Lib.Actions.Tabulation (initTabulatedPermissionsForGroup)
import Lib.Actions.Unsafe (unsafeEmptyShared)
import Lib.Actions.Unsafe.Update.Group (
  unsafeAdjustOrganizationPermission,
  unsafeAdjustRecruiterPermission,
  unsafeAdjustUniversePermission,
  unsafeLinkGroups,
 )
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption (..),
  SinglePermission (Adjust),
 )
import Lib.Types.Store (
  Shared,
  store,
  temp,
  toGroups,
  toTabulatedGroups,
 )
import Lib.Types.Store.Groups (
  edges,
  emptyGroup,
  next,
  nodes,
  outs,
  prev,
  roots,
 )

import Control.Lens (at, ix, (%~), (&), (.~))
import Control.Monad.Extra (unless)
import Control.Monad.State (State, execState, get, modify)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as LT
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  getSize,
  listOf,
  resize,
 )
import Text.Pretty.Simple (pShowNoColor)

data SampleGroupTree a = SampleGroupTree
  { current :: GroupId
  , univ :: CollectionPermissionWithExemption
  , org :: CollectionPermissionWithExemption
  , recr :: CollectionPermission
  , auxPerGroup :: a
  , children :: [SampleGroupTree a]
  }
  deriving (Eq, Show, Read)
instance Functor SampleGroupTree where
  fmap f x =
    x
      { auxPerGroup = f (auxPerGroup x)
      , children = map (fmap f) (children x)
      }

-- | Breadth-first approach
instance Foldable SampleGroupTree where
  foldr f acc (SampleGroupTree _ _ _ _ x xs) =
    foldr (\x' acc' -> foldr f acc' x') (f x acc) xs

instance Traversable SampleGroupTree where
  sequenceA SampleGroupTree{..} =
    (SampleGroupTree current univ org recr)
      <$> auxPerGroup
      <*> sequenceA (map sequenceA children)

instance Arbitrary (SampleGroupTree ()) where
  arbitrary = resize 10 go
   where
    go = do
      current <- arbitrary
      s <- getSize
      children <- resize (s `div` 2) (listOf go)
      univ <- arbitrary
      org <- arbitrary
      recr <- arbitrary
      pure (SampleGroupTree current univ org recr () children)
  shrink (SampleGroupTree current univ org recr () children) =
    [ SampleGroupTree current univ org recr () []
    ]
      ++ children
      ++ [ SampleGroupTree current univ' org' recr' () children'
         | (univ', org', recr', children') <- shrink (univ, org, recr, children)
         ]

storeSampleTree :: SampleGroupTree a -> ActorId -> GroupId -> Shared
storeSampleTree xs adminActor adminGroup = flip execState (emptyShared adminActor adminGroup) $ do
  succeeded <- storeGroup adminActor (current xs)
  unless succeeded $ do
    s <- get
    error $
      "Failed to store first group "
        <> show (current xs)
        <> " - "
        <> LT.unpack (pShowNoColor s)
  go xs
 where
  setupNode current univ org recr = do
    succeeded <- storeGroup adminActor current -- adds current as a root
    unless succeeded $ do
      s <- get
      error $
        "Failed to store group " <> show current <> " - " <> LT.unpack (pShowNoColor s)
    succeeded <- setGroupPermission adminActor (Just Adjust) adminGroup current
    unless succeeded $ do
      s <- get
      error $
        "Failed to grant the admin group groupWise permissions over current "
          <> show current
          <> " - "
          <> LT.unpack (pShowNoColor s)
    succeeded <- setMemberPermission adminActor Create adminGroup current
    unless succeeded $ do
      s <- get
      error $
        "Failed to grant the admin group member permissions over current "
          <> show current
          <> " - "
          <> LT.unpack (pShowNoColor s)
    succeeded <- setUniversePermission adminActor univ current
    unless succeeded $ do
      s <- get
      error $
        "Failed to set universe permission "
          <> show (univ, current)
          <> " - "
          <> LT.unpack (pShowNoColor s)
    succeeded <- setOrganizationPermission adminActor org current
    unless succeeded $ do
      s <- get
      error $
        "Failed to set organization permission "
          <> show (org, current)
          <> " - "
          <> LT.unpack (pShowNoColor s)
    succeeded <- setRecruiterPermission adminActor recr current
    unless succeeded $ do
      s <- get
      error $
        "Failed to set recruiter permission "
          <> show (recr, current)
          <> " - "
          <> LT.unpack (pShowNoColor s)
    currentTab <- initTabulatedPermissionsForGroup current
    -- ensures that singleton maps still have loaded tabs
    modify $ temp . toTabulatedGroups . at current %~ Just . fromMaybe currentTab

  go :: SampleGroupTree a -> State Shared ()
  go SampleGroupTree{..} = do
    setupNode current univ org recr
    -- loads all nodes
    let linkChild (SampleGroupTree child univChild orgChild recrChild _ _) = do
          setupNode child univChild orgChild recrChild
          mE <- unsafeLinkGroups current child -- FIXME
          case mE of
            Left e -> do
              s <- get
              error $
                "Error detected " <> show e <> " - store: " <> LT.unpack (pShowNoColor s)
            Right _ -> pure ()
    traverse_ linkChild children
    traverse_ go children

-- | Uses unsafe methods
loadSampleTree :: SampleGroupTree a -> Shared
loadSampleTree xs = flip execState unsafeEmptyShared $ do
  modify $ store . toGroups . roots . at (current xs) .~ Just ()
  go xs
 where
  setupNode current univ org recr = do
    modify $ store . toGroups . nodes . at current %~ Just . fromMaybe emptyGroup
    unsafeAdjustUniversePermission (const univ) current
    unsafeAdjustOrganizationPermission (const org) current
    unsafeAdjustRecruiterPermission (const recr) current
    currentTab <- initTabulatedPermissionsForGroup current
    -- ensures that singleton maps still have loaded tabs
    modify $ temp . toTabulatedGroups . at current %~ Just . fromMaybe currentTab

  go :: SampleGroupTree a -> State Shared ()
  go SampleGroupTree{..} = do
    setupNode current univ org recr
    -- loads all nodes
    let linkChild (SampleGroupTree child univChild orgChild recrChild _ _) = do
          setupNode child univChild orgChild recrChild
          mE <- unsafeLinkGroups current child
          case mE of
            Left e -> do
              s <- get
              error $
                "Error detected " <> show e <> " - store: " <> LT.unpack (pShowNoColor s)
            Right _ -> pure ()
    traverse_ linkChild children
    traverse_ go children

loadSampleTreeNoTab :: SampleGroupTree a -> Shared
loadSampleTreeNoTab xs = flip execState unsafeEmptyShared $ do
  modify $ store . toGroups . roots . at (current xs) .~ Just ()
  go xs
 where
  addLink :: GroupId -> GroupId -> State Shared ()
  addLink from to = do
    let adjustGroups groups =
          groups
            & edges . at (from, to) .~ Just ()
            & outs . at to .~ Just ()
            & outs . at from .~ Nothing
            & roots . at to .~ Nothing
            & nodes . ix from . next . at to .~ Just ()
            & nodes . ix to . prev .~ Just from
    modify $ store . toGroups %~ adjustGroups

  setupNode current univ org recr = do
    modify $ store . toGroups . nodes . at current %~ Just . fromMaybe emptyGroup
    unsafeAdjustUniversePermission (const univ) current
    unsafeAdjustOrganizationPermission (const org) current
    unsafeAdjustRecruiterPermission (const recr) current

  go :: SampleGroupTree a -> State Shared ()
  go SampleGroupTree{..} = do
    setupNode current univ org recr
    let linkChild (SampleGroupTree child univChild orgChild recrChild _ _) = do
          setupNode child univChild orgChild recrChild
          addLink current child
    traverse_ linkChild children
    traverse_ go children
