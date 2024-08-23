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

module Spec.Sync.Test.Safe.Update where

import Control.Lens (at, ix, non, (^.), (^?), _Just, (^?!))
import Control.Monad.Extra (unless)
import Control.Monad.State (MonadState, evalState, execState)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (elemIndex)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, isJust)
import Lib.Sync.Actions.Safe.Store (
  addMember,
  storeActor,
  storeEntity,
  storeGroup,
  storeSpace,
 )
import Lib.Sync.Actions.Safe.Update (
  addReference,
  addSubscription,
  moveEntity,
  offsetVersionIndex,
  removeReference,
  removeSubscription,
  setVersionIndex,
  updateEntitySpace,
  updateFork,
 )
import Lib.Sync.Actions.Safe.Update.Group (
  linkGroups,
  setEntityPermission,
  setMemberPermission,
  setUniversePermission,
  unlinkGroups,
  updateGroupParent,
 )
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption (..),
 )
import Lib.Sync.Types.Store (
  Shared,
  store,
  temp,
  toEntities,
  toForksFrom,
  toGroups,
  toReferencesFrom,
  toSpaces,
  toSubscriptionsFrom,
  toTabulatedGroups,
  toVersions,
  toSpaceOf,
 )
import Lib.Sync.Types.Store.Entity (fork, versions)
import Lib.Sync.Types.Store.Groups (next, nodes, prev)
import Lib.Sync.Types.Store.Tabulation.Group (hasLessOrEqualPermissionsTo)
import Lib.Sync.Types.Store.Version (references, subscriptions)
import Spec.Sync.Sample.Store (
  arbitraryEmptyShared,
  arbitraryShared,
  storeSample,
 )
import Test.QuickCheck (
  arbitrary,
  chooseInt,
  elements,
  forAll,
  property,
  suchThat,
  suchThatMap,
 )
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)

updateTests :: Spec
updateTests = describe "Update" $ do
  describe "Space" $
    it "doesn't apply" True
  describe "Entity" $ do
    describe "Should Succeed" $ do
      it "Move An Entity to different Space" $
        forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
          property $
            \( aId :: ActorId
              , gId :: GroupId
              , sId :: SpaceId
              , sId' :: SpaceId
              , eId :: EntityId
              , vId :: VersionId
              ) -> do
                let s' = flip execState s $ do
                      setStage adminActor adminGroup aId gId
                      worked <-
                        setUniversePermission
                          (NE.singleton adminActor)
                          (CollectionPermissionWithExemption Read False)
                          gId
                      unless worked $ error $ "Couldn't grant read universe permission " <> show aId
                      worked <- storeSpace (NE.singleton adminActor) sId
                      unless worked $ error $ "Couldn't store space " <> show sId
                      worked <- storeSpace (NE.singleton adminActor) sId'
                      unless worked $ error $ "Couldn't store space " <> show sId'
                      worked <- setEntityPermission (NE.singleton adminActor) Create adminGroup sId
                      unless worked $ error $ "Couldn't set entity permission " <> show sId
                      mWorked <- storeEntity (NE.singleton adminActor) eId sId vId Nothing
                      case mWorked of
                        Just (Right ()) -> pure ()
                        _ -> error $ "Couldn't store entity " <> show (eId, vId)
                      worked <- setEntityPermission (NE.singleton adminActor) Create gId sId'
                      unless worked $ error $ "Couldn't set entity permission " <> show sId'
                      worked <- setEntityPermission (NE.singleton adminActor) Delete gId sId
                      unless worked $ error $ "Couldn't set entity permission " <> show sId
                      worked <- updateEntitySpace (NE.singleton aId) eId sId'
                      unless worked $ error $ "Couldn't move entity " <> show eId
                (s' ^? store . toSpaces . ix sId . ix eId)
                  `shouldBe` Nothing
                (s' ^? store . toSpaces . ix sId' . ix eId)
                  `shouldBe` Just ()
                (s' ^? temp . toSpaceOf . ix eId) `shouldBe` Just sId'
    describe "Should Fail" $ do
      it "Move An Entity to different Space without delete access" $
        forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
          property $
            \( aId :: ActorId
              , gId :: GroupId
              , sId :: SpaceId
              , sId' :: SpaceId
              , eId :: EntityId
              , vId :: VersionId
              ) -> do
                let go = do
                      setStage adminActor adminGroup aId gId
                      worked <-
                        setUniversePermission
                          (NE.singleton adminActor)
                          (CollectionPermissionWithExemption Read False)
                          gId
                      unless worked $ error $ "Couldn't grant read universe permission " <> show aId
                      worked <- storeSpace (NE.singleton adminActor) sId
                      unless worked $ error $ "Couldn't store space " <> show sId
                      worked <- storeSpace (NE.singleton adminActor) sId'
                      unless worked $ error $ "Couldn't store space " <> show sId'
                      worked <- setEntityPermission (NE.singleton adminActor) Create adminGroup sId
                      unless worked $ error $ "Couldn't set entity permission " <> show sId
                      mWorked <- storeEntity (NE.singleton adminActor) eId sId vId Nothing
                      case mWorked of
                        Just (Right ()) -> pure ()
                        _ -> error $ "Couldn't store entity " <> show (eId, vId)
                      worked <- setEntityPermission (NE.singleton adminActor) Create gId sId'
                      unless worked $ error $ "Couldn't set entity permission " <> show sId'
                      updateEntitySpace (NE.singleton aId) eId sId'
                    s' = execState go s
                evalState go s `shouldBe` False
                (s' ^? store . toSpaces . ix sId . ix eId)
                  `shouldBe` Just ()
                (s' ^? store . toSpaces . ix sId' . ix eId)
                  `shouldBe` Nothing
                (s' ^? temp . toSpaceOf . ix eId) `shouldBe` Just sId
      it "Move An Entity to different Space without create access" $
        forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
          property $
            \( aId :: ActorId
              , gId :: GroupId
              , sId :: SpaceId
              , sId' :: SpaceId
              , eId :: EntityId
              , vId :: VersionId
              ) -> do
                let go = do
                      setStage adminActor adminGroup aId gId
                      worked <-
                        setUniversePermission
                          (NE.singleton adminActor)
                          (CollectionPermissionWithExemption Read False)
                          gId
                      unless worked $ error $ "Couldn't grant read universe permission " <> show aId
                      worked <- storeSpace (NE.singleton adminActor) sId
                      unless worked $ error $ "Couldn't store space " <> show sId
                      worked <- storeSpace (NE.singleton adminActor) sId'
                      unless worked $ error $ "Couldn't store space " <> show sId'
                      worked <- setEntityPermission (NE.singleton adminActor) Create adminGroup sId
                      unless worked $ error $ "Couldn't set entity permission " <> show sId
                      mWorked <- storeEntity (NE.singleton adminActor) eId sId vId Nothing
                      case mWorked of
                        Just (Right ()) -> pure ()
                        _ -> error $ "Couldn't store entity " <> show (eId, vId)
                      worked <- setEntityPermission (NE.singleton adminActor) Delete gId sId
                      unless worked $ error $ "Couldn't set entity permission " <> show sId
                      updateEntitySpace (NE.singleton aId) eId sId'
                    s' = execState go s
                evalState go s `shouldBe` False
                (s' ^. store . toSpaces . at sId . non mempty . at eId)
                  `shouldBe` Just ()
                (s' ^. store . toSpaces . at sId' . non mempty . at eId)
                  `shouldBe` Nothing
                (s' ^? temp . toSpaceOf . ix eId) `shouldBe` Just sId
  -- updating an entity occurs when you store a version or modify the set of an entity's versions
  -- or changing what space it belongs to (requires create/entity rights on target space)
  describe "Version" $ do
    describe "Should Succeed" $ do
      it "Add Reference" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              HM.size (s ^. store . toVersions) >= 2
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let versions = s ^. store . toVersions
               in forAll (elements $ HM.keys versions) $ \vId ->
                    forAll (elements . HM.keys $ HM.delete vId versions) $ \refId -> do
                      let s' = flip execState s $ do
                            mE <- addReference (NE.singleton adminActor) vId refId
                            case mE of
                              Just (Right ()) -> pure ()
                              _ -> error $ "Couldn't add reference " <> show (vId, refId, mE)
                      (s' ^? store . toVersions . ix vId . references . ix refId) `shouldBe` Just ()
                      (s' ^? temp . toReferencesFrom . ix refId . ix vId) `shouldBe` Just ()
      it "Remove Reference" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              HM.size (s ^. temp . toReferencesFrom) >= 2
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let refs = s ^. temp . toReferencesFrom
               in forAll (elements $ HM.keys refs) $ \refId ->
                    forAll (elements . HS.toList . fromJust $ HM.lookup refId refs) $ \vId -> do
                      let s' = flip execState s $ do
                            mE <- removeReference (NE.singleton adminActor) vId refId
                            case mE of
                              Just (Right ()) -> pure ()
                              _ -> error $ "Couldn't remove reference " <> show (vId, refId, mE)
                      (s' ^? store . toVersions . ix vId . references . ix refId) `shouldBe` Nothing
                      (s' ^? temp . toReferencesFrom . ix refId . ix vId) `shouldBe` Nothing
      it "Add Subscription" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              HM.size (s ^. store . toEntities) >= 2
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let entities = s ^. store . toEntities
                  selectVersionAndEntity = do
                    (eId, e) <- elements $ HM.toList entities
                    vId <- elements . NE.toList $ e ^. versions
                    pure (eId, vId)
               in forAll selectVersionAndEntity $ \(eIdOfVId, vId) ->
                    forAll (elements . HM.keys $ HM.delete eIdOfVId entities) $ \subId -> do
                      let s' = flip execState s $ do
                            mE <- addSubscription (NE.singleton adminActor) vId subId
                            case mE of
                              Just (Right ()) -> pure ()
                              _ -> error $ "Couldn't add subscription " <> show (vId, subId, mE)
                      (s' ^? store . toVersions . ix vId . subscriptions . ix subId)
                        `shouldBe` Just ()
                      (s' ^? temp . toSubscriptionsFrom . ix subId . ix vId) `shouldBe` Just ()
      it "Remove Subscription" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              not . null $ s ^. temp . toSubscriptionsFrom
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let subs = s ^. temp . toSubscriptionsFrom
               in forAll (elements $ HM.keys subs) $ \subId ->
                    forAll (elements . HS.toList . fromJust $ HM.lookup subId subs) $ \vId -> do
                      let s' = flip execState s $ do
                            mE <- removeSubscription (NE.singleton adminActor) vId subId
                            case mE of
                              Just (Right ()) -> pure ()
                              _ -> error $ "Couldn't remove subscription " <> show (vId, subId, mE)
                      (s' ^? store . toVersions . ix vId . subscriptions . ix subId)
                        `shouldBe` Nothing
                      (s' ^? temp . toSubscriptionsFrom . ix subId . ix vId) `shouldBe` Nothing
      it "Re-Fork an Entity" $
        let gen = do
              let genSharedAndEnt = do
                    (shared, f, aA, aG) <- suchThatMap arbitrary $ \(xs, adminActor, adminGroup) ->
                      let s = storeSample xs adminActor adminGroup
                          forking = HM.filter (\e -> isJust (e ^. fork)) (s ^. store . toEntities)
                       in if null forking || length (s ^. store . toVersions) < 5
                            then Nothing
                            else Just (s, forking, adminActor, adminGroup)
                    (eId, e) <- elements (HM.toList f)
                    pure (shared, eId, e, aA, aG)
              (s, eId, notForked, adminA, adminG) <- suchThatMap genSharedAndEnt $ \(shared, eId, e, aA, aG) ->
                let notForked =
                      filter (\vId -> e ^. fork /= Just vId) . HM.keys $ shared ^. store . toVersions
                 in if null notForked then Nothing else Just (shared, eId, notForked, aA, aG)
              newFork <- elements notForked
              pure (s, eId, newFork, adminA, adminG)
         in forAll gen $ \(s, eId, newFork, adminActor, adminGroup) -> do
              let s' = flip execState s $ do
                    mE <- updateFork (NE.singleton adminActor) eId (Just newFork)
                    case mE of
                      Just (Right ()) -> pure ()
                      _ -> error $ "Couldn't remove version " <> show (newFork, mE)
              (s' ^? store . toEntities . ix eId . fork . _Just) `shouldBe` Just newFork
              (s' ^? temp . toForksFrom . ix newFork . ix eId) `shouldBe` Just ()
      it "Move an Entity" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              not (null $ s ^. store . toEntities) && length (s ^. store . toSpaces) >= 2
         in forAll gen $ \(s, adminActor, adminGroup) ->
              forAll (elements . HM.toList $ s ^. store . toEntities) $ \(eId, e) ->
                forAll
                  ( elements
                      . HM.keys
                      . HM.filter
                        (\es -> not $ eId `HS.member` es)
                      $ s ^. store . toSpaces
                  )
                  $ \newSId -> do
                    let s' = flip execState s $ do
                          mE <- moveEntity (NE.singleton adminActor) eId newSId
                          case mE of
                            Just (Right ()) -> pure ()
                            _ -> error $ "Couldn't move entity " <> show (eId, newSId, mE)
                    (s' ^? temp . toSpaceOf . ix eId) `shouldBe` Just newSId
                    (s' ^? store . toSpaces . ix newSId . ix eId) `shouldBe` Just ()
                    (s' ^? store . toSpaces . ix (s ^?! temp . toSpaceOf . ix eId) . ix eId)
                      `shouldBe` Nothing
      it "Offset a Version" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              not
                . null
                . HM.filter (\e -> length (e ^. versions) > 1)
                $ s ^. store . toEntities
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let genE =
                    elements . HM.toList . HM.filter (\e -> length (e ^. versions) > 1) $
                      s ^. store . toEntities
               in forAll genE $ \(eId, e) ->
                    let genV = do
                          v <- elements . NE.toList $ e ^. versions
                          offsetDirection <- arbitrary
                          offsetMagnitude <- do
                            let idx = fromJust . elemIndex v . NE.toList $ e ^. versions
                            if offsetDirection
                              then chooseInt (0, length (e ^. versions) - idx)
                              else negate <$> chooseInt (0, idx)
                          pure (v, offsetMagnitude)
                     in forAll genV $ \(vId, offset) -> do
                          let s' = flip execState s $ do
                                mE <- offsetVersionIndex (NE.singleton adminActor) vId offset
                                case mE of
                                  Just (Right ()) -> pure ()
                                  _ -> error $ "Couldn't offset version " <> show (eId, vId, mE)
                          shouldSatisfy (s' ^? store . toEntities . ix eId . versions) $ \mVs ->
                            let vs = NE.toList $ fromJust mVs
                                oldIdx = fromJust . elemIndex vId . NE.toList $ e ^. versions
                                newIdx = fromJust $ elemIndex vId vs
                             in (newIdx - oldIdx) <= offset
      it "Set a Version Index" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              not
                . null
                . HM.filter (\e -> length (e ^. versions) > 1)
                $ s ^. store . toEntities
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let genE =
                    elements . HM.toList . HM.filter (\e -> length (e ^. versions) > 1) $
                      s ^. store . toEntities
               in forAll genE $ \(eId, e) ->
                    let genV = do
                          v <- elements . NE.toList $ e ^. versions
                          idx <- chooseInt (0, length (e ^. versions) - 1)
                          pure (v, idx)
                     in forAll genV $ \(vId, idx) -> do
                          let s' = flip execState s $ do
                                mE <- setVersionIndex (NE.singleton adminActor) vId idx
                                case mE of
                                  Just (Right ()) -> pure ()
                                  _ -> error $ "Couldn't set version index " <> show (eId, vId, mE)
                          (s' ^? store . toEntities . ix eId . versions . ix idx) `shouldBe` Just vId
  -- updating a version occurs when you modify an existing one; still subject to modifying the
  -- entity by extension. Modifying a version - changing its references / subscriptions,
  -- changing what it forks from
  describe "Group" $ do
    -- updating what it inherits from / who it inherits to
    describe "Should Succeed" $ do
      it "deleting parent relationship should affect both nodes, and unrelate them" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              not . null . HM.filter (\g -> isJust (g ^. prev)) $
                s ^. store . toGroups . nodes
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let gsWithParent = HM.filter (\g -> isJust (g ^. prev)) $ s ^. store . toGroups . nodes
               in forAll (elements (HM.toList gsWithParent)) $ \(gId :: GroupId, g) -> do
                    let s' = flip execState s $ do
                          mE <- updateGroupParent (NE.singleton adminActor) gId Nothing
                          case mE of
                            Just (Right ()) -> pure ()
                            _ -> error $ "Couldn't set group parent " <> show (gId, mE)
                        parentId = fromJust $ g ^. prev
                    (s' ^? store . toGroups . nodes . ix gId . prev . _Just) `shouldBe` Nothing
                    (s' ^? store . toGroups . nodes . ix parentId . next . ix gId)
                      `shouldBe` Nothing
      it "deleting child relationship should affect both nodes, and unrelate them" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              not . null . HM.filter (\g -> isJust (g ^. prev)) $
                s ^. store . toGroups . nodes
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let gsWithParent = HM.filter (\g -> isJust (g ^. prev)) $ s ^. store . toGroups . nodes
               in forAll (elements (HM.toList gsWithParent)) $ \(gId :: GroupId, g) -> do
                    let s' = flip execState s $ do
                          worked <- unlinkGroups (NE.singleton adminActor) parentId gId
                          unless worked $ error $ "Couldn't unlink groups " <> show (parentId, gId)
                        parentId = fromJust $ g ^. prev
                    (s' ^? store . toGroups . nodes . ix gId . prev . _Just) `shouldBe` Nothing
                    (s' ^? store . toGroups . nodes . ix parentId . next . ix gId)
                      `shouldBe` Nothing
      it "adding child relationship should cause inheritance" $
        let gen = do
              (s', aA, aG) <- suchThat arbitraryShared $ \(s, _, _) ->
                not . null . HM.filter (\g -> isJust (g ^. prev)) $
                  s ^. store . toGroups . nodes
              nG <- arbitrary
              pure (s', aA, aG, nG)
         in forAll gen $ \(s, adminActor, adminGroup, newGId) ->
              let gsWithParent = HM.filter (\g -> isJust (g ^. prev)) $ s ^. store . toGroups . nodes
               in forAll (elements (HM.toList gsWithParent)) $ \(gId :: GroupId, _) ->
                    let s' = flip execState s $ do
                          worked <- storeGroup (NE.singleton adminActor) newGId
                          unless worked $ error $ "Couldn't store group " <> show newGId
                          mE <- linkGroups (NE.singleton adminActor) gId newGId
                          case mE of
                            Just (Right ()) -> pure ()
                            _ -> error $ "Couldn't set child " <> show (gId, newGId, mE)
                     in shouldSatisfy (s', gId, newGId) $ \_ ->
                          fromJust (s' ^? temp . toTabulatedGroups . ix gId)
                            `hasLessOrEqualPermissionsTo` fromJust (s' ^? temp . toTabulatedGroups . ix newGId)
  describe "Member" $
    it "doesn't apply" True
  describe "Actor" $
    it "doesn't apply" True

setStage
  :: (MonadState Shared m) => ActorId -> GroupId -> ActorId -> GroupId -> m ()
setStage adminActor adminGroup aId gId = do
  worked <- storeActor (NE.singleton adminActor) aId
  unless worked $ error $ "Couldn't create actor " <> show aId
  worked <- storeGroup (NE.singleton adminActor) gId
  unless worked $ error $ "Couldn't create group " <> show gId
  worked <- setMemberPermission (NE.singleton adminActor) Create adminGroup gId
  unless worked $
    error $
      "Couldn't grant membership creation to admin group " <> show gId
  worked <- addMember (NE.singleton adminActor) gId aId
  unless worked $ error $ "Couldn't add member " <> show (gId, aId)
