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

module Spec.Sync.Test.Safe.Delete where

import Control.Lens (at, ix, (^.), (^?), (^?!))
import Control.Monad.Extra (unless)
import Control.Monad.State (execState)
import Data.Foldable (fold, for_)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe)
import Lib.Class (
  removeActor,
  removeEntity,
  removeGroup,
  removeMember,
  removeSpace,
  removeVersion,
  setEntityPermission,
  setMemberPermission,
 )
import Lib.Sync.Types.Store (
  store,
  temp,
  toActors,
  toEntities,
  toForksFrom,
  toGroups,
  toMemberOf,
  toReferencesFrom,
  toSpaceOf,
  toSpaces,
  toSubscriptionsFrom,
  toVersions,
 )
import Lib.Sync.Types.Store.Entity (fork, versions)
import Lib.Sync.Types.Store.Groups (edges, members, nodes)
import Lib.Sync.Types.Store.Version (references, subscriptions)
import Lib.Types.Id (ActorId)
import Lib.Types.Permission (
  CollectionPermission (..),
 )
import Spec.Sync.Sample.Store (
  arbitraryShared,
 )
import Test.QuickCheck (
  elements,
  forAll,
  suchThat,
 )
import Test.Syd (Spec, context, describe, it, shouldBe)

removeTests :: Spec
removeTests = describe "Remove" $ do
  it "Version" $
    let gen = suchThat arbitraryShared $ \(s, _, _) ->
          not . null . HM.filter (\e -> length (e ^. versions) > 1) $
            s ^. store . toEntities
     in forAll gen $ \(s, adminActor, adminGroup) ->
          let entsWith2OrMoreVersions =
                HM.filter (\e -> length (e ^. versions) > 1) $
                  s ^. store . toEntities
           in forAll (elements $ HM.toList entsWith2OrMoreVersions) $ \(eId, e) ->
                forAll (elements . NE.toList $ e ^. versions) $ \vId -> do
                  let s' = flip execState s $ do
                        worked <- removeVersion (NE.singleton adminActor) vId
                        unless worked $ error $ "Couldn't remove version " <> show (vId)
                  (s' ^? store . toVersions . ix vId) `shouldBe` Nothing
                  ( filter (== vId) . fromMaybe mempty . fmap NE.toList $
                      s' ^? store . toEntities . ix eId . versions
                    )
                    `shouldBe` mempty
                  for_ (fromJust (s ^? store . toVersions . ix vId . references)) $ \refId ->
                    (s' ^? temp . toReferencesFrom . ix refId . ix vId) `shouldBe` Nothing
                  for_ (fromJust (s ^? store . toVersions . ix vId . subscriptions)) $ \subId ->
                    (s' ^? temp . toSubscriptionsFrom . ix subId . ix vId) `shouldBe` Nothing
                  (s' ^? temp . toReferencesFrom . ix vId) `shouldBe` Nothing
  describe "Entity" $
    it "should delete all versions" $
      let gen = suchThat arbitraryShared $ \(s, _, _) ->
            not . null $ s ^. store . toEntities
       in forAll gen $ \(s, adminActor, adminGroup) ->
            let genE = elements . HM.toList $ s ^. store . toEntities
             in forAll genE $ \(eId, e) -> do
                  let s' = flip execState s $ do
                        worked <-
                          setEntityPermission
                            (NE.singleton adminActor)
                            Delete
                            adminGroup
                            (s ^?! temp . toSpaceOf . ix eId)
                        unless worked $
                          error $
                            "Couldn't set delete permission for entities "
                              <> show (s ^?! temp . toSpaceOf . ix eId)
                        worked <- removeEntity (NE.singleton adminActor) eId
                        unless worked $ error $ "Couldn't remove entity " <> show (eId)
                  (s' ^. store . toEntities . at eId) `shouldBe` Nothing
                  for_ (e ^. versions) $ \vId ->
                    (s' ^. store . toVersions . at vId) `shouldBe` Nothing
                  for_ (e ^. fork) $ \forkId ->
                    (s' ^? temp . toForksFrom . ix forkId . ix eId) `shouldBe` Nothing
                  (s' ^? temp . toSubscriptionsFrom . ix eId) `shouldBe` Nothing
  -- TODO verify that subscriptions are also removed
  describe "Space" $
    it "should delete all entities" $
      let gen = suchThat arbitraryShared $ \(s, _, _) ->
            not . null $ s ^. store . toSpaces
       in forAll gen $ \(s, adminActor, adminGroup) ->
            let genS = elements . HM.toList $ s ^. store . toSpaces
             in forAll genS $ \(sId, es) -> do
                  let s' = flip execState s $ do
                        worked <- setEntityPermission (NE.singleton adminActor) Delete adminGroup sId
                        unless worked $
                          error $
                            "Couldn't set delete permission for entities " <> show sId
                        worked <- removeSpace (NE.singleton adminActor) sId
                        unless worked $ error $ "Couldn't remove space " <> show (sId)
                  (s' ^. store . toSpaces . at sId) `shouldBe` Nothing
                  for_ es $ \eId -> do
                    (s' ^. store . toEntities . at eId) `shouldBe` Nothing
                    (s' ^? temp . toSubscriptionsFrom . ix eId) `shouldBe` Nothing
                    for_ (s ^?! store . toEntities . ix eId . versions) $ \vId ->
                      (s' ^? temp . toReferencesFrom . ix vId) `shouldBe` Nothing
  describe "Member" $
    it "should remove member from group" $
      let gen = suchThat arbitraryShared $ \(s, _, _) ->
            not . null . fold $ s ^. temp . toMemberOf
       in forAll gen $ \(s, adminActor, adminGroup) ->
            let genG = elements . HM.toList . HM.filter (not . null) $ s ^. temp . toMemberOf
             in forAll genG $ \(aId :: ActorId, gs) ->
                  forAll (elements $ HS.toList gs) $ \gId -> do
                    let s' = flip execState s $ do
                          worked <- setMemberPermission (NE.singleton adminActor) Delete adminGroup gId
                          unless worked $
                            error $
                              "Couldn't set group permission " <> show gId
                          worked <- removeMember (NE.singleton adminActor) gId aId
                          unless worked $
                            error $
                              "Couldn't remove member " <> show (gId, aId)
                    context "Actor in Group" $
                      (s' ^? store . toGroups . nodes . ix gId . members . ix aId) `shouldBe` Nothing
                    context "Group in Actor" $
                      (s' ^? temp . toMemberOf . ix aId . ix gId) `shouldBe` Nothing
  describe "Actor" $
    it "should remove member from group" $
      let gen = suchThat arbitraryShared $ \(s, _, _) ->
            not . null $ s ^. store . toActors
       in forAll gen $ \(s, adminActor, adminGroup) ->
            let genG = elements . HM.keys $ s ^. temp . toMemberOf
             in forAll genG $ \(aId :: ActorId) -> do
                  let s' = flip execState s $ do
                        worked <- removeActor (NE.singleton adminActor) aId
                        unless worked $
                          error $
                            "Couldn't remove actor " <> show aId
                  (s' ^? temp . toMemberOf . ix aId) `shouldBe` Nothing
                  (foldMap (^. members) (s' ^. store . toGroups . nodes) ^. at aId)
                    `shouldBe` Nothing
  describe "Group" $
    it "should remove member from group" $
      let gen = suchThat arbitraryShared $ \(s, _, _) ->
            not . null $ s ^. store . toGroups . nodes
       in forAll gen $ \(s, adminActor, adminGroup) ->
            let genG = elements . HM.keys $ s ^. store . toGroups . nodes
             in forAll genG $ \gId -> do
                  let s' = flip execState s $ do
                        worked <- removeGroup (NE.singleton adminActor) gId
                        unless worked $ error $ "Couldn't remove group " <> show gId
                  (s' ^? store . toGroups . nodes . ix gId) `shouldBe` Nothing
                  let (froms, tos) = unzip . HS.toList $ s' ^. store . toGroups . edges
                  filter (== gId) froms `shouldBe` mempty
                  filter (== gId) tos `shouldBe` mempty
