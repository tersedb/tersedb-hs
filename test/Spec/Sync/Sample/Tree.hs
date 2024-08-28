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

module Spec.Sync.Sample.Tree where

import Lib.Class (
  setGroupPermission,
  setMemberPermission,
  setOrganizationPermission,
  setRecruiterPermission,
  setUniversePermission,
  storeGroup,
 )
import Lib.Sync.Actions.Safe (emptyShared)
import Lib.Sync.Actions.Tabulation (initTabulatedPermissionsForGroup)
import Lib.Sync.Actions.Unsafe (unsafeEmptyShared)
import Lib.Sync.Actions.Unsafe.Update.Group (
  unsafeAdjustOrganizationPermission,
  unsafeAdjustRecruiterPermission,
  unsafeAdjustUniversePermission,
  unsafeLinkGroups,
 )
import Lib.Sync.Types.Store (
  Shared,
  store,
  temp,
  toGroups,
  toTabulatedGroups,
 )
import Lib.Sync.Types.Store.Groups (
  edges,
  emptyGroup,
  next,
  nodes,
  outs,
  prev,
  roots,
 )
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption (..),
  SinglePermission (Adjust),
 )
import Control.Lens (at, ix, (%~), (&), (.~), (?~))
import Control.Monad.Extra (unless)
import Control.Monad.State (StateT, get, modify, execStateT)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as LT
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  getSize,
  listOf,
  resize,
 )
import Text.Pretty.Simple (pShowNoColor)
import Control.Exception (SomeException)

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
    foldr (flip (foldr f)) (f x acc) xs

instance Traversable SampleGroupTree where
  sequenceA SampleGroupTree{..} =
    SampleGroupTree current univ org recr
      <$> auxPerGroup
      <*> traverse sequenceA children

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
storeSampleTree xs adminActor adminGroup = 
  let eX = flip execStateT (emptyShared adminActor adminGroup) $ do
        succeeded <- storeGroup (NE.singleton adminActor) (current xs)
        unless succeeded $ do
          s <- get
          error $
            "Failed to store first group "
              <> show (current xs)
              <> " - "
              <> LT.unpack (pShowNoColor s)
        go xs
  in  case eX of
        Left e -> error $ show e
        Right x -> x
 where
  setupNode current univ org recr = do
    succeeded <- storeGroup (NE.singleton adminActor) current -- adds current as a root
    unless succeeded $ do
      s <- get
      error $
        "Failed to store group " <> show current <> " - " <> LT.unpack (pShowNoColor s)
    succeeded <-
      setGroupPermission (NE.singleton adminActor) (Just Adjust) adminGroup current
    unless succeeded $ do
      s <- get
      error $
        "Failed to grant the admin group groupWise permissions over current "
          <> show current
          <> " - "
          <> LT.unpack (pShowNoColor s)
    succeeded <-
      setMemberPermission (NE.singleton adminActor) Create adminGroup current
    unless succeeded $ do
      s <- get
      error $
        "Failed to grant the admin group member permissions over current "
          <> show current
          <> " - "
          <> LT.unpack (pShowNoColor s)
    succeeded <- setUniversePermission (NE.singleton adminActor) univ current
    unless succeeded $ do
      s <- get
      error $
        "Failed to set universe permission "
          <> show (univ, current)
          <> " - "
          <> LT.unpack (pShowNoColor s)
    succeeded <- setOrganizationPermission (NE.singleton adminActor) org current
    unless succeeded $ do
      s <- get
      error $
        "Failed to set organization permission "
          <> show (org, current)
          <> " - "
          <> LT.unpack (pShowNoColor s)
    succeeded <- setRecruiterPermission (NE.singleton adminActor) recr current
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

  go :: SampleGroupTree a -> StateT Shared (Either SomeException) ()
  go SampleGroupTree{..} = do
    setupNode current univ org recr
    -- loads all nodes
    let linkChild (SampleGroupTree child univChild orgChild recrChild _ _) = do
          setupNode child univChild orgChild recrChild
          unsafeLinkGroups current child -- FIXME
    traverse_ linkChild children
    traverse_ go children

-- | Uses unsafe methods
loadSampleTree :: SampleGroupTree a -> Shared
loadSampleTree xs = 
  let eX = flip execStateT unsafeEmptyShared $ do
        modify $ store . toGroups . roots . at (current xs) ?~ ()
        go xs
  in  case eX of
        Left e -> error $ show e
        Right x -> x
 where
  setupNode current univ org recr = do
    modify $ store . toGroups . nodes . at current %~ Just . fromMaybe emptyGroup
    unsafeAdjustUniversePermission (const univ) current
    unsafeAdjustOrganizationPermission (const org) current
    unsafeAdjustRecruiterPermission (const recr) current
    currentTab <- initTabulatedPermissionsForGroup current
    -- ensures that singleton maps still have loaded tabs
    modify $ temp . toTabulatedGroups . at current %~ Just . fromMaybe currentTab

  go :: SampleGroupTree a -> StateT Shared (Either SomeException) ()
  go SampleGroupTree{..} = do
    setupNode current univ org recr
    -- loads all nodes
    let linkChild (SampleGroupTree child univChild orgChild recrChild _ _) = do
          setupNode child univChild orgChild recrChild
          unsafeLinkGroups current child
    traverse_ linkChild children
    traverse_ go children

loadSampleTreeNoTab :: SampleGroupTree a -> Shared
loadSampleTreeNoTab xs =
  let eX = flip execStateT unsafeEmptyShared $ do
        modify $ store . toGroups . roots . at (current xs) ?~ ()
        go xs
  in  case eX of
        Left e -> error $ show e
        Right x -> x
 where
  addLink :: GroupId -> GroupId -> StateT Shared (Either SomeException) ()
  addLink from to = do
    let adjustGroups groups =
          groups
            & edges . at (from, to) ?~ ()
            & outs . at to ?~ ()
            & outs . at from .~ Nothing
            & roots . at to .~ Nothing
            & nodes . ix from . next . at to ?~ ()
            & nodes . ix to . prev ?~ from
    modify $ store . toGroups %~ adjustGroups

  setupNode current univ org recr = do
    modify $ store . toGroups . nodes . at current %~ Just . fromMaybe emptyGroup
    unsafeAdjustUniversePermission (const univ) current
    unsafeAdjustOrganizationPermission (const org) current
    unsafeAdjustRecruiterPermission (const recr) current

  go :: SampleGroupTree a -> StateT Shared (Either SomeException) ()
  go SampleGroupTree{..} = do
    setupNode current univ org recr
    let linkChild (SampleGroupTree child univChild orgChild recrChild _ _) = do
          setupNode child univChild orgChild recrChild
          addLink current child
    traverse_ linkChild children
    traverse_ go children
