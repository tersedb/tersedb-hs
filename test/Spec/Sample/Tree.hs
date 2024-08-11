{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , RecordWildCards
  , FlexibleInstances
  , NamedFieldPuns
  #-}

module Spec.Sample.Tree where

import Lib.Types.Id (GroupId, ActorId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption (..)
  , SinglePermission (Adjust)
  )
import Lib.Types.Store
  ( Store
  , toGroups
  , toTabulatedPermissions
  )
import Lib.Types.Store.Groups
  ( nodes
  , edges
  , roots
  , outs
  , next
  , prev
  , emptyGroup
  )
import Lib.Actions.Unsafe
  ( unsafeEmptyStore
  , unsafeLinkGroups
  , unsafeAdjustUniversePermission
  , unsafeAdjustOrganizationPermission
  , unsafeAdjustRecruiterPermission
  )
import Lib.Actions.Safe
  ( emptyStore
  , storeGroup
  , setUniversePermission
  , setOrganizationPermission
  , setRecruiterPermission
  , setGroupPermission
  )
import Lib.Actions.Tabulation (initTabulatedPermissionsForGroup)

import qualified Data.Aeson as Aeson
import Data.Foldable (traverse_)
import qualified Data.Text.Lazy as LT
import Data.Maybe (fromMaybe)
import Text.Pretty.Simple (pShowNoColor)
import Control.Monad.Extra (unless)
import Control.Monad.State (State, execState, modify, get)
import Control.Lens ((%~), (.~), (&), at, ix)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , getSize
  , resize
  , listOf
  )


data SampleGroupTree a = SampleGroupTree
  { current :: GroupId
  , univ :: CollectionPermissionWithExemption
  , org :: CollectionPermissionWithExemption
  , recr :: CollectionPermission
  , auxPerGroup :: a
  , children :: [SampleGroupTree a]
  } deriving (Eq, Show, Read)
instance Functor SampleGroupTree where
  fmap f x = x
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
    ] ++
    children ++
    [ SampleGroupTree current univ' org' recr' () children'
    | (univ', org', recr', children') <- shrink (univ, org, recr, children) ]


storeSampleTree :: SampleGroupTree a -> ActorId -> GroupId -> Store
storeSampleTree xs adminActor adminGroup = flip execState (emptyStore adminActor adminGroup) $ do
  -- modify $ toGroups . roots . at (current xs) .~ Just ()
  succeeded <- storeGroup adminActor (current xs)
  unless succeeded $ do
    s <- get
    error $ "Failed to store first group " <> show (current xs) <> " - " <> LT.unpack (pShowNoColor s)
  go xs
  where
    setupNode current univ org recr = do
      succeeded <- storeGroup adminActor current -- adds current as a root
      unless succeeded $ do
        s <- get
        error $ "Failed to store group " <> show current <> " - " <> LT.unpack (pShowNoColor s)
      succeeded <- setGroupPermission adminActor (Just Adjust) adminGroup current
      unless succeeded $ do
        s <- get
        error $ "Failed to grant admin group permissions over current " <> show current <> " - " <> LT.unpack (pShowNoColor s)
      succeeded <- setUniversePermission adminActor univ current
      unless succeeded $ do
        s <- get
        error $ "Failed to set universe permission " <> show (univ, current) <> " - " <> LT.unpack (pShowNoColor s)
      succeeded <- setOrganizationPermission adminActor org current
      unless succeeded $ do
        s <- get
        error $ "Failed to set organization permission " <> show (org, current) <> " - " <> LT.unpack (pShowNoColor s)
      succeeded <- setRecruiterPermission adminActor recr current
      unless succeeded $ do
        s <- get
        error $ "Failed to set recruiter permission " <> show (recr, current) <> " - " <> LT.unpack (pShowNoColor s)
      currentTab <- initTabulatedPermissionsForGroup current
      -- ensures that singleton maps still have loaded tabs
      modify $ toTabulatedPermissions . at current %~ Just . fromMaybe currentTab

    go :: SampleGroupTree a -> State Store ()
    go SampleGroupTree{..} = do
      setupNode current univ org recr
      -- loads all nodes
      let linkChild (SampleGroupTree child univChild orgChild recrChild _ _) = do
            setupNode child univChild orgChild recrChild
            mE <- unsafeLinkGroups current child -- FIXME
            case mE of
              Left e -> do
                s <- get
                error $ "Error detected " <> show e <> " - store: " <> LT.unpack (pShowNoColor s)
              Right _ -> pure ()
      traverse_ linkChild children
      traverse_ go children


loadSampleTree :: SampleGroupTree a -> Store
loadSampleTree xs = flip execState unsafeEmptyStore $ do
  modify $ toGroups . roots . at (current xs) .~ Just ()
  go xs
  where
    setupNode current univ org recr = do
      modify $ toGroups . nodes . at current %~ Just . fromMaybe emptyGroup
      unsafeAdjustUniversePermission (const univ) current
      unsafeAdjustOrganizationPermission (const org) current
      unsafeAdjustRecruiterPermission (const recr) current
      currentTab <- initTabulatedPermissionsForGroup current
      -- ensures that singleton maps still have loaded tabs
      modify $ toTabulatedPermissions . at current %~ Just . fromMaybe currentTab

    go :: SampleGroupTree a -> State Store ()
    go SampleGroupTree{..} = do
      setupNode current univ org recr
      -- loads all nodes
      let linkChild (SampleGroupTree child univChild orgChild recrChild _ _) = do
            setupNode child univChild orgChild recrChild
            mE <- unsafeLinkGroups current child
            case mE of
              Left e -> do
                s <- get
                error $ "Error detected " <> show e <> " - store: " <> LT.unpack (pShowNoColor s)
              Right _ -> pure ()
      traverse_ linkChild children
      traverse_ go children


loadSampleTreeNoTab :: SampleGroupTree a -> Store
loadSampleTreeNoTab xs = flip execState unsafeEmptyStore $ do
  modify $ toGroups . roots . at (current xs) .~ Just ()
  go xs
  where
    addLink :: GroupId -> GroupId -> State Store ()
    addLink from to = do
      let adjustGroups groups = groups
            & edges . at (from,to) .~ Just ()
            & outs . at to .~ Just ()
            & outs . at from .~ Nothing
            & roots . at to .~ Nothing
            & nodes . ix from . next . at to .~ Just ()
            & nodes . ix to . prev .~ Just from
      modify $ toGroups %~ adjustGroups

    setupNode current univ org recr = do
      modify $ toGroups . nodes . at current %~ Just . fromMaybe emptyGroup
      unsafeAdjustUniversePermission (const univ) current
      unsafeAdjustOrganizationPermission (const org) current
      unsafeAdjustRecruiterPermission (const recr) current

    go :: SampleGroupTree a -> State Store ()
    go SampleGroupTree{..} = do
      setupNode current univ org recr
      let linkChild (SampleGroupTree child univChild orgChild recrChild _ _) = do
            setupNode child univChild orgChild recrChild
            addLink current child
      traverse_ linkChild children
      traverse_ go children
