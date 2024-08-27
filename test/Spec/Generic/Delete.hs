module Spec.Generic.Delete where

import Control.Lens (At (at), Ixed (ix), (^.), (^?), (^?!))
import Control.Monad (unless)
import Data.Data (Proxy (Proxy))
import Data.Foldable (fold, for_)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe)
import Lib.Class (
  TerseDB (commit, genSyncShared),
  TerseDBGen (runTerseDB),
  removeActor,
  removeEntity,
  removeGroup,
  removeMember,
  removeSpace,
  removeVersion,
  setEntityPermission,
  setMemberPermission,
 )
import qualified Lib.Sync.Types.Store as Sync
import qualified Lib.Sync.Types.Store.Entity as Sync
import qualified Lib.Sync.Types.Store.Groups as Sync
import qualified Lib.Sync.Types.Store.Version as Sync
import Lib.Types.Id (ActorId)
import Lib.Types.Permission (CollectionPermission (Delete))
import Spec.Sync.Sample.Store (arbitraryShared)
import Test.QuickCheck (elements, forAll, suchThat)
import Test.Syd (Spec, context, describe, it, shouldBe, shouldSatisfy)

removeTests :: forall n m. (TerseDB n m, TerseDBGen n) => Proxy m -> Spec
removeTests Proxy = do
  it "Version" $
    let gen = suchThat arbitraryShared $ \(s, _, _) ->
          not . null . HM.filter (\e -> length (e ^. Sync.versions) > 1) $
            s ^. Sync.store . Sync.toEntities
     in forAll gen $ \(s, adminActor, adminGroup) ->
          let entsWith2OrMoreVersions =
                HM.filter (\e -> length (e ^. Sync.versions) > 1) $
                  s ^. Sync.store . Sync.toEntities
           in forAll (elements $ HM.toList entsWith2OrMoreVersions) $ \(eId, e) ->
                forAll (elements . NE.toList $ e ^. Sync.versions) $ \vId -> do
                  let go :: m Sync.Shared
                      go = do
                        worked <- removeVersion (NE.singleton adminActor) vId
                        unless worked $ error $ "Couldn't remove version " <> show (vId)
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  context "Version shouldn't exist" $
                    shouldSatisfy (s', vId, eId) $ \_ ->
                      (s' ^? Sync.store . Sync.toVersions . ix vId) == Nothing
                  context "Version shouldn't be in its entity" $
                    ( filter (== vId) . fromMaybe mempty . fmap NE.toList $
                        s' ^? Sync.store . Sync.toEntities . ix eId . Sync.versions
                    )
                      `shouldBe` mempty
                  context "References are removed" $
                    for_ (fromJust (s ^? Sync.store . Sync.toVersions . ix vId . Sync.references)) $ \refId ->
                      (s' ^? Sync.temp . Sync.toReferencesFrom . ix refId . ix vId) `shouldBe` Nothing
                  context "Subscriptions are removed" $
                    for_
                      (fromJust (s ^? Sync.store . Sync.toVersions . ix vId . Sync.subscriptions)) $ \subId ->
                      (s' ^? Sync.temp . Sync.toSubscriptionsFrom . ix subId . ix vId)
                        `shouldBe` Nothing
                  context "Doesn't exist as a reference" $
                    (s' ^? Sync.temp . Sync.toReferencesFrom . ix vId) `shouldBe` Nothing
  describe "Entity" $
    it "should delete all versions" $
      let gen = suchThat arbitraryShared $ \(s, _, _) ->
            not . null $ s ^. Sync.store . Sync.toEntities
       in forAll gen $ \(s, adminActor, adminGroup) ->
            let genE = elements . HM.toList $ s ^. Sync.store . Sync.toEntities
             in forAll genE $ \(eId, e) -> do
                  let go :: m Sync.Shared
                      go = do
                        worked <-
                          setEntityPermission
                            (NE.singleton adminActor)
                            Delete
                            adminGroup
                            (s ^?! Sync.temp . Sync.toSpaceOf . ix eId)
                        unless worked $
                          error $
                            "Couldn't set delete permission for entities "
                              <> show (s ^?! Sync.temp . Sync.toSpaceOf . ix eId)
                        worked <- removeEntity (NE.singleton adminActor) eId
                        unless worked $ error $ "Couldn't remove entity " <> show (eId)
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  context "Entity shouldn't exist" $
                    (s' ^. Sync.store . Sync.toEntities . at eId) `shouldBe` Nothing
                  context "No versions should exist" $
                    for_ (e ^. Sync.versions) $ \vId ->
                      (s' ^. Sync.store . Sync.toVersions . at vId) `shouldBe` Nothing
                  context "Shouldn't be forked by any version" $
                    for_ (e ^. Sync.fork) $ \forkId ->
                      (s' ^? Sync.temp . Sync.toForksFrom . ix forkId . ix eId) `shouldBe` Nothing
                  context "Shouldn't be subscribed to" $
                    (s' ^? Sync.temp . Sync.toSubscriptionsFrom . ix eId) `shouldBe` Nothing
  describe "Space" $
    it "should delete all entities" $
      let gen = suchThat arbitraryShared $ \(s, _, _) ->
            not . null $ s ^. Sync.store . Sync.toSpaces
       in forAll gen $ \(s, adminActor, adminGroup) ->
            let genS = elements . HM.toList $ s ^. Sync.store . Sync.toSpaces
             in forAll genS $ \(sId, es) -> do
                  let go :: m Sync.Shared
                      go = do
                        worked <- setEntityPermission (NE.singleton adminActor) Delete adminGroup sId
                        unless worked $
                          error $
                            "Couldn't set delete permission for entities " <> show sId
                        worked <- removeSpace (NE.singleton adminActor) sId
                        unless worked $ error $ "Couldn't remove space " <> show (sId)
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  (s' ^. Sync.store . Sync.toSpaces . at sId) `shouldBe` Nothing
                  for_ es $ \eId -> do
                    (s' ^. Sync.store . Sync.toEntities . at eId) `shouldBe` Nothing
                    (s' ^? Sync.temp . Sync.toSubscriptionsFrom . ix eId) `shouldBe` Nothing
                    for_ (s ^?! Sync.store . Sync.toEntities . ix eId . Sync.versions) $ \vId ->
                      (s' ^? Sync.temp . Sync.toReferencesFrom . ix vId) `shouldBe` Nothing
  describe "Member" $
    it "should remove member from group" $
      let gen = suchThat arbitraryShared $ \(s, _, _) ->
            not . null . fold $ s ^. Sync.temp . Sync.toMemberOf
       in forAll gen $ \(s, adminActor, adminGroup) ->
            let genG =
                  elements . HM.toList . HM.filter (not . null) $ s ^. Sync.temp . Sync.toMemberOf
             in forAll genG $ \(aId :: ActorId, gs) ->
                  forAll (elements $ HS.toList gs) $ \gId -> do
                    let go :: m Sync.Shared
                        go = do
                          worked <- setMemberPermission (NE.singleton adminActor) Delete adminGroup gId
                          unless worked $
                            error $
                              "Couldn't set group permission " <> show gId
                          worked <- removeMember (NE.singleton adminActor) gId aId
                          unless worked $
                            error $
                              "Couldn't remove member " <> show (gId, aId)
                          genSyncShared
                    s' <- runTerseDB (commit go) s
                    context "Actor in Group" $
                      (s' ^? Sync.store . Sync.toGroups . Sync.nodes . ix gId . Sync.members . ix aId)
                        `shouldBe` Nothing
                    context "Group in Actor" $
                      (s' ^? Sync.temp . Sync.toMemberOf . ix aId . ix gId) `shouldBe` Nothing
  describe "Actor" $
    it "should remove member from group" $
      let gen = suchThat arbitraryShared $ \(s, _, _) ->
            not . null $ s ^. Sync.store . Sync.toActors
       in forAll gen $ \(s, adminActor, adminGroup) ->
            let genG = elements . HM.keys $ s ^. Sync.temp . Sync.toMemberOf
             in forAll genG $ \(aId :: ActorId) -> do
                  let go :: m Sync.Shared
                      go = do
                        worked <- removeActor (NE.singleton adminActor) aId
                        unless worked $
                          error $
                            "Couldn't remove actor " <> show aId
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  (s' ^? Sync.temp . Sync.toMemberOf . ix aId) `shouldBe` Nothing
                  ( foldMap (^. Sync.members) (s' ^. Sync.store . Sync.toGroups . Sync.nodes)
                      ^. at aId
                    )
                    `shouldBe` Nothing
  describe "Group" $
    it "should remove member from group" $
      let gen = suchThat arbitraryShared $ \(s, _, _) ->
            not . null $ s ^. Sync.store . Sync.toGroups . Sync.nodes
       in forAll gen $ \(s, adminActor, adminGroup) ->
            let genG = elements . HM.keys $ s ^. Sync.store . Sync.toGroups . Sync.nodes
             in forAll genG $ \gId -> do
                  let go :: m Sync.Shared
                      go = do
                        worked <- removeGroup (NE.singleton adminActor) gId
                        unless worked $ error $ "Couldn't remove group " <> show gId
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  (s' ^? Sync.store . Sync.toGroups . Sync.nodes . ix gId) `shouldBe` Nothing
                  let (froms, tos) = unzip . HS.toList $ s' ^. Sync.store . Sync.toGroups . Sync.edges
                  filter (== gId) froms `shouldBe` mempty
                  filter (== gId) tos `shouldBe` mempty
