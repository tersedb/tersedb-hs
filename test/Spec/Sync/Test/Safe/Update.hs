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

import Control.Lens (at, ix, non, (^.), (^?), (^?!), _Just)
import Control.Monad.Extra (unless)
import Control.Monad.State (MonadState, evalState, execState)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (elemIndex)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, isJust)
import Lib.Class (
  TerseDB,
  addMember,
  addReference,
  addSubscription,
  linkGroups,
  moveEntity,
  offsetVersionIndex,
  removeReference,
  removeSubscription,
  setEntityPermission,
  setMemberPermission,
  setUniversePermission,
  setVersionIndex,
  storeActor,
  storeEntity,
  storeGroup,
  storeSpace,
  unlinkGroups,
  updateFork,
 )
import Lib.Sync.Actions.Safe.Update.Group (
  updateGroupParent,
 )
import Lib.Sync.Types.Store (
  Shared,
  store,
  temp,
  toEntities,
  toForksFrom,
  toGroups,
  toReferencesFrom,
  toSpaceOf,
  toSpaces,
  toSubscriptionsFrom,
  toTabulatedGroups,
  toVersions,
 )
import Lib.Sync.Types.Store.Entity (fork, versions)
import Lib.Sync.Types.Store.Groups (next, nodes, prev)
import Lib.Sync.Types.Store.Tabulation.Group (hasLessOrEqualPermissionsTo)
import Lib.Sync.Types.Store.Version (references, subscriptions)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption (..),
 )
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
  -- updating an entity occurs when you store a version or modify the set of an entity's versions
  -- or changing what space it belongs to (requires create/entity rights on target space)
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
                          worked <- updateGroupParent (NE.singleton adminActor) gId Nothing
                          unless worked $ error $ "Couldn't set group parent " <> show (gId)
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
                          worked <- linkGroups (NE.singleton adminActor) gId newGId
                          unless worked $ error $ "Couldn't set child " <> show (gId, newGId)
                     in shouldSatisfy (s', gId, newGId) $ \_ ->
                          fromJust (s' ^? temp . toTabulatedGroups . ix gId)
                            `hasLessOrEqualPermissionsTo` fromJust (s' ^? temp . toTabulatedGroups . ix newGId)
  describe "Member" $
    it "doesn't apply" True
  describe "Actor" $
    it "doesn't apply" True

setStage
  :: (TerseDB n m) => ActorId -> GroupId -> ActorId -> GroupId -> m ()
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
