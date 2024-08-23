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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Sync.Sample.Store where

import Spec.Sync.Sample.Tree (
  SampleGroupTree (..),
  loadSampleTree,
  storeSampleTree,
 )

import Lib.Sync.Actions.Safe (emptyShared)
import Lib.Sync.Actions.Safe.Store (
  addMember,
  storeActor,
  storeEntity,
  storeNextVersion,
  storeSpace,
 )
import Lib.Sync.Actions.Safe.Update (
  updateVersionReferences,
  updateVersionSubscriptions,
 )
import Lib.Sync.Actions.Safe.Update.Group (
  setEntityPermission,
  setSpacePermission,
 )
import Lib.Sync.Actions.Unsafe.Store (
  unsafeAddMember,
  unsafeStoreActor,
  unsafeStoreEntity,
  unsafeStoreSpace,
  unsafeStoreVersion,
 )
import Lib.Sync.Actions.Unsafe.Update (
  unsafeUpdateVersionReferences,
  unsafeUpdateVersionSubscriptions,
 )
import Lib.Sync.Actions.Unsafe.Update.Group (
  unsafeAdjustEntityPermission,
  unsafeAdjustSpacePermission,
 )
import Lib.Sync.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Sync.Types.Permission (
  CollectionPermission (..),
  SinglePermission,
 )
import Lib.Sync.Types.Store (Shared, store, temp)

import Control.Lens (at, (%~), (.~), (^.), _1, _2)
import Control.Monad (replicateM, void)
import Control.Monad.Extra (unless)
import Control.Monad.State (State, execState, get, modify)
import Data.Foldable (foldlM, for_, traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty, uncons)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, isNothing, mapMaybe)
import qualified Data.Text.Lazy as LT
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Gen,
  chooseInt,
  elements,
  listOf,
  listOf1,
 )
import Text.Pretty.Simple (pShowNoColor)

type SampleEntity =
  ( SpaceId
  , NonEmpty (VersionId, HashSet VersionId, HashSet EntityId)
  , Maybe VersionId
  )

data SampleStore = SampleStore
  { sampleSpaces :: HashSet SpaceId
  , sampleEntities :: [(EntityId, SampleEntity)] -- "should be forked"
  , sampleActors :: HashMap ActorId (HashSet GroupId)
  , sampleGroups
      :: SampleGroupTree
          (HashMap SpaceId SinglePermission, HashMap SpaceId CollectionPermission) -- spaces & entities
  }
  deriving (Eq, Show, Read)

instance Arbitrary SampleStore where
  arbitrary = do
    spaces <- arbitrary
    -- FIXME sample entityId's from set
    entities <-
      if null spaces
        then pure mempty
        else do
          let go :: [(EntityId, SampleEntity)] -> () -> Gen [(EntityId, SampleEntity)]
              go generatedSoFar () = do
                entity <- arbitrary
                space <- elements (HS.toList spaces)
                let entsAndVersions :: HashMap EntityId (HashSet VersionId) -- nonempty HashSet
                    entsAndVersions =
                      HM.fromList $
                        map
                          ( \(k, (_, vs, _)) -> (k, HS.fromList $ map (\(vId, _, _) -> vId) (NE.toList vs))
                          )
                          generatedSoFar
                versions <- fmap NE.fromList . listOf1 $ do
                  (vId :: VersionId) <- arbitrary
                  refs <- do
                    refsCount <- chooseInt (0, 5)
                    if null entsAndVersions
                      then pure mempty
                      else do
                        fmap HS.fromList . replicateM refsCount $ do
                          (_, versionsToRefTo) <- elements $ HM.toList entsAndVersions
                          elements $ HS.toList versionsToRefTo
                  subs <- do
                    subsCount <- chooseInt (0, 5)
                    if null entsAndVersions
                      then pure mempty
                      else do
                        fmap HS.fromList . replicateM subsCount $ do
                          elements $ HM.keys entsAndVersions
                  pure (vId, refs, subs)
                fork <- do
                  shouldBeForked <- (== 0) <$> chooseInt (0, 10)
                  if not shouldBeForked
                    then pure Nothing
                    else do
                      if null entsAndVersions
                        then pure Nothing
                        else do
                          (_, versionsToForkFrom) <- elements $ HM.toList entsAndVersions
                          Just <$> elements (HS.toList versionsToForkFrom)
                pure $ (entity, (space, versions, fork)) : generatedSoFar
          count <- arbitrary
          reverse <$> foldlM go [] (replicate count ())
    (groupsStructure :: SampleGroupTree ()) <- arbitrary
    let fromSpaces
          :: Gen (HashMap SpaceId SinglePermission, HashMap SpaceId CollectionPermission) -- Spaces & entities rights
        fromSpaces =
          let relevantPermissions :: (Arbitrary a) => Gen (HashMap SpaceId a)
              relevantPermissions =
                if null spaces
                  then pure mempty
                  else fmap HM.fromList . listOf $ (,) <$> elements (HS.toList spaces) <*> arbitrary
           in (,) <$> relevantPermissions <*> relevantPermissions
    groups <- sequenceA $ fmap (const fromSpaces) groupsStructure -- replaces () with fromSpaces
    let allGroups :: HashSet GroupId
        allGroups =
          let go (SampleGroupTree g _ _ _ _ gs) = HS.insert g . HS.unions $ fmap go gs
           in go groups
    actors <- fmap HM.fromList . listOf $ do
      aId <- arbitrary
      gs <-
        if null allGroups
          then pure mempty
          else fmap HS.fromList . listOf . elements $ HS.toList allGroups
      pure (aId, gs)
    pure
      SampleStore
        { sampleSpaces = spaces
        , sampleEntities = entities
        , sampleActors = actors
        , sampleGroups = groups
        }
  shrink (SampleStore spaces entities actors groups) =
    [ SampleStore
        { sampleSpaces = spaces
        , sampleEntities = entities
        , sampleActors = fmap (HS.filter (\g -> g == current groups)) actors
        , sampleGroups = groups{children = []}
        }
    ]

loadSample :: SampleStore -> Shared
loadSample SampleStore{..} = flip execState (loadSampleTree sampleGroups) $ do
  for_ (HS.toList sampleSpaces) $ \sId -> do
    unsafeStoreSpace sId
  for_ sampleEntities $ \(eId, (sId, vIds, mFork)) -> do
    let ((vId, refIds, subIds), vIdsTail) = uncons vIds
    case mFork of
      Nothing -> do
        eWorked <- unsafeStoreEntity eId sId vId Nothing
        case eWorked of
          Left e -> error $ "Error during store genesis entity " <> show e
          Right () -> pure ()
      Just fork -> do
        eWorked <- unsafeStoreEntity eId sId vId (Just fork)
        case eWorked of
          Left e -> error $ "Error during store fork entity " <> show e
          Right () -> pure ()
    eWorked <- unsafeUpdateVersionReferences vId refIds
    case eWorked of
      Left e -> error $ "Error during version references update " <> show e
      Right () -> pure ()
    eWorked <- unsafeUpdateVersionSubscriptions vId subIds
    case eWorked of
      Left e -> error $ "Error during version subscriptions update " <> show e
      Right () -> pure ()
    case vIdsTail of
      Nothing -> pure ()
      Just vIdsTail -> void . (\f -> foldlM f vId vIdsTail) $ \prevVId (vId, refIds, subIds) -> do
        eWorked <- unsafeStoreVersion eId vId
        case eWorked of
          Left e -> error $ "Error during store version " <> show e
          Right () -> do
            eWorked <- unsafeUpdateVersionReferences vId refIds
            case eWorked of
              Left e -> error $ "Error during version references update " <> show e
              Right () -> do
                eWorked <- unsafeUpdateVersionSubscriptions vId subIds
                case eWorked of
                  Left e -> error $ "Error during version subscriptions update " <> show e
                  Right () -> pure vId
  let loadPermissions
        :: SampleGroupTree
            (HashMap SpaceId SinglePermission, HashMap SpaceId CollectionPermission)
        -> State Shared ()
      loadPermissions SampleGroupTree{current, auxPerGroup = (spacesPerms, entityPerms), children} = do
        for_ (HM.toList spacesPerms) $ \(space, permission) -> do
          unsafeAdjustSpacePermission (const (Just permission)) current space
        for_ (HM.toList entityPerms) $ \(space, permission) -> do
          unsafeAdjustEntityPermission (const permission) current space
        traverse_ loadPermissions children
  loadPermissions sampleGroups
  for_ (HM.toList sampleActors) $ \(aId, gs) -> do
    unsafeStoreActor aId
    for_ (HS.toList gs) $ \gId ->
      unsafeAddMember gId aId

storeSample :: SampleStore -> ActorId -> GroupId -> Shared
storeSample SampleStore{..} adminActor adminGroup =
  flip execState (storeSampleTree sampleGroups adminActor adminGroup) $ do
    for_ (HS.toList sampleSpaces) $ \sId -> do
      succeeded <- storeSpace (NE.singleton adminActor) sId
      unless succeeded $ do
        s <- get
        error $
          "Failed to store space " <> show sId <> " - " <> LT.unpack (pShowNoColor s)
      succeeded <- setEntityPermission (NE.singleton adminActor) Update adminGroup sId
      unless succeeded $ do
        s <- get
        error $
          "Failed to set entity permissions "
            <> show sId
            <> " - "
            <> LT.unpack (pShowNoColor s)
    for_ sampleEntities $ \(eId, (sId, vIds, mFork)) -> do
      -- FIXME use a State to keep retrying on fork failure? Or just sort the list?
      let ((vId, refIds, subIds), vIdsTail) = uncons vIds
      case mFork of
        Nothing -> do
          mWorked <- storeEntity (NE.singleton adminActor) eId sId vId Nothing
          case mWorked of
            Just (Right ()) -> pure ()
            _ -> do
              s <- get
              error $
                "Failed to store entity "
                  <> show (eId, sId, vId)
                  <> " - "
                  <> LT.unpack (pShowNoColor s)
        Just fork -> do
          mE <- storeEntity (NE.singleton adminActor) eId sId vId (Just fork)
          case mE of
            Just (Right ()) -> pure ()
            _ -> do
              s <- get
              error $
                "Failed to store forked entity "
                  <> show (mE, eId, sId, vId, fork)
                  <> " - "
                  <> LT.unpack (pShowNoColor s)
      mE <- updateVersionReferences (NE.singleton adminActor) vId refIds
      case mE of
        Just (Right ()) -> pure ()
        _ -> do
          s <- get
          error $
            "Failed to store version references "
              <> show (mE, eId, vId)
              <> " - "
              <> LT.unpack (pShowNoColor s)
      mE <- updateVersionSubscriptions (NE.singleton adminActor) vId subIds
      case mE of
        Just (Right ()) -> pure ()
        _ -> do
          s <- get
          error $
            "Failed to store version subscriptions "
              <> show (mE, eId, vId)
              <> " - "
              <> LT.unpack (pShowNoColor s)
      case vIdsTail of
        Nothing -> pure ()
        Just vIdsTail -> for_ vIdsTail $ \(vId, refIds, subIds) -> do
          mE <- storeNextVersion (NE.singleton adminActor) eId vId
          case mE of
            Just (Right ()) -> pure ()
            _ -> do
              s <- get
              error $
                "Failed to store version "
                  <> show (mE, eId, vId)
                  <> " - "
                  <> LT.unpack (pShowNoColor s)
          mE <- updateVersionReferences (NE.singleton adminActor) vId refIds
          case mE of
            Just (Right ()) -> pure ()
            _ -> do
              s <- get
              error $
                "Failed to store version references "
                  <> show (mE, eId, vId)
                  <> " - "
                  <> LT.unpack (pShowNoColor s)
          mE <- updateVersionSubscriptions (NE.singleton adminActor) vId subIds
          case mE of
            Just (Right ()) -> pure ()
            _ -> do
              s <- get
              error $
                "Failed to store version subscriptions "
                  <> show (mE, eId, vId)
                  <> " - "
                  <> LT.unpack (pShowNoColor s)
    let loadPermissions
          :: SampleGroupTree
              (HashMap SpaceId SinglePermission, HashMap SpaceId CollectionPermission)
          -> State Shared ()
        loadPermissions SampleGroupTree{current, auxPerGroup = (spacesPerms, entityPerms), children} = do
          for_ (HM.toList spacesPerms) $ \(space, permission) -> do
            succeeded <-
              setSpacePermission (NE.singleton adminActor) (Just permission) current space
            unless succeeded $ do
              s <- get
              error $
                "Failed to set space permission "
                  <> show (current, space)
                  <> " - "
                  <> LT.unpack (pShowNoColor s)
          for_ (HM.toList entityPerms) $ \(space, permission) -> do
            succeeded <-
              setEntityPermission (NE.singleton adminActor) permission current space
            unless succeeded $ do
              s <- get
              error $
                "Failed to set entity permission "
                  <> show (current, space)
                  <> " - "
                  <> LT.unpack (pShowNoColor s)
          traverse_ loadPermissions children
    loadPermissions sampleGroups
    for_ (HM.toList sampleActors) $ \(aId, gs) -> do
      succeeded <- storeActor (NE.singleton adminActor) aId
      unless succeeded $ do
        s <- get
        error $
          "Failed to store actor " <> show aId <> " - " <> LT.unpack (pShowNoColor s)
      for_ (HS.toList gs) $ \gId -> do
        succeeded <- addMember (NE.singleton adminActor) gId aId
        unless succeeded $ do
          s <- get
          error $
            "Failed to add member "
              <> show (aId, gId)
              <> " - "
              <> LT.unpack (pShowNoColor s)

arbitraryShared :: Gen (Shared, ActorId, GroupId)
arbitraryShared = do
  xs <- arbitrary
  adminActor <- arbitrary
  adminGroup <- arbitrary
  let s = storeSample xs adminActor adminGroup
  pure (s, adminActor, adminGroup)

arbitraryEmptyShared :: Gen (Shared, ActorId, GroupId)
arbitraryEmptyShared = do
  adminActor <- arbitrary
  adminGroup <- arbitrary
  let s = emptyShared adminActor adminGroup
  pure (s, adminActor, adminGroup)
