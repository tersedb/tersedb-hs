module Spec.Test.Safe.Delete where

import Control.Lens (at, ix, (^.), (^?))
import Control.Monad.Extra (unless)
import Control.Monad.State (execState)
import Data.Foldable (for_, fold)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe)
import Lib.Actions.Safe.Remove (
  removeEntity,
  removeSpace,
  removeVersion,
  removeMember,
 )
import Lib.Actions.Safe.Update.Group (
  setEntityPermission,
  setMemberPermission
 )
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission (..),
 )
import Lib.Types.Store (
  store,
  temp,
  toEntities,
  toForksFrom,
  toReferencesFrom,
  toSpaces,
  toGroups,
  toActors,
  toSubscriptionsFrom,
  toVersions,
 )
import Lib.Types.Store.Entity (fork, space, versions)
import Lib.Types.Store.Groups (nodes, members)
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Tabulation.Group (hasLessOrEqualPermissionsTo)
import Lib.Types.Store.Version (entity, references, subscriptions)
import Spec.Sample.Store (
  arbitraryShared,
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
import Test.Syd (Spec, describe, it, shouldBe, context)

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
                        mE <- removeVersion adminActor vId
                        case mE of
                          Just (Right ()) -> pure ()
                          _ -> error $ "Couldn't remove version " <> show (vId, mE)
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
                        worked <- setEntityPermission adminActor Delete adminGroup (e ^. space)
                        unless worked $
                          error $
                            "Couldn't set delete permission for entities " <> show (e ^. space)
                        eWorked <- removeEntity adminActor eId
                        case eWorked of
                          Just (Right ()) -> pure ()
                          _ -> error $ "Couldn't remove entity " <> show (eId, eWorked)
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
             in forAll genS $ \(sId, sp) -> do
                  let s' = flip execState s $ do
                        worked <- setEntityPermission adminActor Delete adminGroup sId
                        unless worked $
                          error $
                            "Couldn't set delete permission for entities " <> show sId
                        eWorked <- removeSpace adminActor sId
                        case eWorked of
                          Just (Right ()) -> pure ()
                          _ -> error $ "Couldn't remove space " <> show (sId, eWorked)
                  (s' ^. store . toSpaces . at sId) `shouldBe` Nothing
                  for_ (sp ^. entities) $ \eId -> do
                    (s' ^. store . toEntities . at eId) `shouldBe` Nothing
                    (s' ^? temp . toSubscriptionsFrom . ix eId) `shouldBe` Nothing
                    for_ (fromJust (s ^? store . toEntities . ix eId . versions)) $ \vId ->
                      (s' ^? temp . toReferencesFrom . ix vId) `shouldBe` Nothing
  describe "Member" $
    it "should remove member from group" $
      let gen = suchThat arbitraryShared $ \(s,_,_) ->
            not . null . fold $ s ^. store . toActors
      in forAll gen $ \(s, adminActor, adminGroup) ->
          let genG = elements . HM.toList . HM.filter (not . null) $ s ^. store . toActors
          in  forAll genG $ \(aId :: ActorId, gs) ->
                forAll (elements $ HS.toList gs) $ \gId -> do
                  let s' = flip execState s $ do
                        worked <- setMemberPermission adminActor Delete adminGroup gId
                        unless worked $
                          error $ "Couldn't set group permission " <> show gId
                        worked <- removeMember adminActor gId aId
                        unless worked $
                          error $ "Couldn't remove member " <> show (gId, aId)
                  context "Actor in Group" $
                    (s' ^? store . toGroups . nodes . ix gId . members . ix aId) `shouldBe` Nothing
                  context "Group in Actor" $
                    (s' ^? store . toActors . ix aId . ix gId) `shouldBe` Nothing
