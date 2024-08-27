module Spec.Generic.Update where

import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)
import Lib.Types.Id (ActorId, GroupId, SpaceId, EntityId, VersionId)
import Lib.Types.Permission (CollectionPermission (..), CollectionPermissionWithExemption (..))
import Test.QuickCheck (forAll, Testable (property), suchThat, elements, suchThatMap, chooseInt, Arbitrary (arbitrary))
import Spec.Sync.Sample.Store (arbitraryEmptyShared, arbitraryShared)
import Lib.Class (TerseDB (genSyncShared, commit), storeActor, storeGroup, setMemberPermission, addMember, setUniversePermission, storeSpace, setEntityPermission, storeEntity, moveEntity, TerseDBGen (runTerseDB), addReference, removeReference, addSubscription, removeSubscription, updateFork, offsetVersionIndex, setVersionIndex, updateGroupParent, unlinkGroups, linkGroups)
import Control.Monad (unless)
import qualified Data.List.NonEmpty as NE
import Control.Lens ((^?), Ixed (ix), (^.), _Just, (^?!))
import Data.Data (Proxy(Proxy))
import qualified Lib.Sync.Types.Store as Sync
import qualified Lib.Sync.Types.Store.Version as Sync
import qualified Lib.Sync.Types.Store.Entity as Sync
import qualified Lib.Sync.Types.Store.Groups as Sync
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromJust, isJust)
import Data.List (elemIndex)
import Lib.Sync.Types.Store.Tabulation.Group (hasLessOrEqualPermissionsTo)


updateTests :: forall m n. (TerseDBGen n, TerseDB n m) => Proxy m -> Spec
updateTests Proxy = do
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
                let go :: m Sync.Shared
                    go = do
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
                      worked <- storeEntity (NE.singleton adminActor) eId sId vId Nothing
                      unless worked $ error $ "Couldn't store entity " <> show (eId, vId)
                      worked <- setEntityPermission (NE.singleton adminActor) Create gId sId'
                      unless worked $ error $ "Couldn't set entity permission " <> show sId'
                      worked <- setEntityPermission (NE.singleton adminActor) Delete gId sId
                      unless worked $ error $ "Couldn't set entity permission " <> show sId
                      worked <- moveEntity (NE.singleton aId) eId sId'
                      unless worked $ error $ "Couldn't move entity " <> show eId
                      genSyncShared
                s' <- runTerseDB (commit go) s
                (s' ^? Sync.store . Sync.toSpaces . ix sId . ix eId)
                  `shouldBe` Nothing
                (s' ^? Sync.store . Sync.toSpaces . ix sId' . ix eId)
                  `shouldBe` Just ()
                (s' ^? Sync.temp . Sync.toSpaceOf . ix eId) `shouldBe` Just sId'
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
                let go :: m (Bool, Sync.Shared)
                    go = do
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
                      worked <- storeEntity (NE.singleton adminActor) eId sId vId Nothing
                      unless worked $ error $ "Couldn't store entity " <> show (eId, vId)
                      worked <- setEntityPermission (NE.singleton adminActor) Create gId sId'
                      unless worked $ error $ "Couldn't set entity permission " <> show sId'
                      res <- moveEntity (NE.singleton aId) eId sId'
                      s' <- genSyncShared
                      pure (res,s')
                (res, s') <- runTerseDB (commit go) s
                res `shouldBe` False
                (s' ^? Sync.store . Sync.toSpaces . ix sId . ix eId)
                  `shouldBe` Just ()
                (s' ^? Sync.store . Sync.toSpaces . ix sId' . ix eId)
                  `shouldBe` Nothing
                (s' ^? Sync.temp . Sync.toSpaceOf . ix eId) `shouldBe` Just sId
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
                let go :: m (Bool, Sync.Shared)
                    go = do
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
                      worked <- storeEntity (NE.singleton adminActor) eId sId vId Nothing
                      unless worked $ error $ "Couldn't store entity " <> show (eId, vId)
                      worked <- setEntityPermission (NE.singleton adminActor) Delete gId sId
                      unless worked $ error $ "Couldn't set entity permission " <> show sId
                      res <- moveEntity (NE.singleton aId) eId sId'
                      s' <- genSyncShared
                      pure (res,s')
                (res, s') <- runTerseDB (commit go) s
                res `shouldBe` False
                (s' ^? Sync.store . Sync.toSpaces . ix sId . ix eId)
                  `shouldBe` Just ()
                (s' ^? Sync.store . Sync.toSpaces . ix sId' . ix eId)
                  `shouldBe` Nothing
                (s' ^? Sync.temp . Sync.toSpaceOf . ix eId) `shouldBe` Just sId
  describe "Version" $ do
    describe "Should Succeed" $ do
      it "Add Reference" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              HM.size (s ^. Sync.store . Sync.toVersions) >= 2
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let versions = s ^. Sync.store . Sync.toVersions
               in forAll (elements $ HM.keys versions) $ \vId ->
                    forAll (elements . HM.keys $ HM.delete vId versions) $ \refId -> do
                      let go :: m Sync.Shared
                          go = do
                            worked <- addReference (NE.singleton adminActor) vId refId
                            unless worked $ error $ "Couldn't add reference " <> show (vId, refId)
                            genSyncShared
                      s' <- runTerseDB (commit go) s
                      (s' ^? Sync.store . Sync.toVersions . ix vId . Sync.references . ix refId) `shouldBe` Just ()
                      (s' ^? Sync.temp . Sync.toReferencesFrom . ix refId . ix vId) `shouldBe` Just ()
      it "Remove Reference" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              HM.size (s ^. Sync.temp . Sync.toReferencesFrom) >= 2
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let refs = s ^. Sync.temp . Sync.toReferencesFrom
               in forAll (elements $ HM.keys refs) $ \refId ->
                    forAll (elements . HS.toList . fromJust $ HM.lookup refId refs) $ \vId -> do
                      let go :: m Sync.Shared
                          go = do
                            worked <- removeReference (NE.singleton adminActor) vId refId
                            unless worked $ error $ "Couldn't remove reference " <> show (vId, refId)
                            genSyncShared
                      s' <- runTerseDB (commit go) s
                      (s' ^? Sync.store . Sync.toVersions . ix vId . Sync.references . ix refId) `shouldBe` Nothing
                      (s' ^? Sync.temp . Sync.toReferencesFrom . ix refId . ix vId) `shouldBe` Nothing
      it "Add Subscription" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              HM.size (s ^. Sync.store . Sync.toEntities) >= 2
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let entities = s ^. Sync.store . Sync.toEntities
                  selectVersionAndEntity = do
                    (eId, e) <- elements $ HM.toList entities
                    vId <- elements . NE.toList $ e ^. Sync.versions
                    pure (eId, vId)
               in forAll selectVersionAndEntity $ \(eIdOfVId, vId) ->
                    forAll (elements . HM.keys $ HM.delete eIdOfVId entities) $ \subId -> do
                      let go :: m Sync.Shared
                          go = do
                            worked <- addSubscription (NE.singleton adminActor) vId subId
                            unless worked $ error $ "Couldn't add subscription " <> show (vId, subId)
                            genSyncShared
                      s' <- runTerseDB (commit go) s
                      (s' ^? Sync.store . Sync.toVersions . ix vId . Sync.subscriptions . ix subId)
                        `shouldBe` Just ()
                      (s' ^? Sync.temp . Sync.toSubscriptionsFrom . ix subId . ix vId) `shouldBe` Just ()
      it "Remove Subscription" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              not . null $ s ^. Sync.temp . Sync.toSubscriptionsFrom
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let subs = s ^. Sync.temp . Sync.toSubscriptionsFrom
               in forAll (elements $ HM.keys subs) $ \subId ->
                    forAll (elements . HS.toList . fromJust $ HM.lookup subId subs) $ \vId -> do
                      let go :: m Sync.Shared
                          go = do
                            worked <- removeSubscription (NE.singleton adminActor) vId subId
                            unless worked $ error $ "Couldn't remove subscription " <> show (vId, subId)
                            genSyncShared
                      s' <- runTerseDB (commit go) s
                      (s' ^? Sync.store . Sync.toVersions . ix vId . Sync.subscriptions . ix subId)
                        `shouldBe` Nothing
                      (s' ^? Sync.temp . Sync.toSubscriptionsFrom . ix subId . ix vId) `shouldBe` Nothing
      it "Re-Fork an Entity" $
        let gen = do
              let genSharedAndEnt = do
                    (shared, f, aA, aG) <- suchThatMap arbitraryShared $ \(s, adminActor, adminGroup) ->
                      let forking = HM.filter (\e -> isJust (e ^. Sync.fork)) (s ^. Sync.store . Sync.toEntities)
                       in if null forking || length (s ^. Sync.store . Sync.toVersions) < 5
                            then Nothing
                            else Just (s, forking, adminActor, adminGroup)
                    (eId, e) <- elements (HM.toList f)
                    pure (shared, eId, e, aA, aG)
              (s, eId, notForked, adminA, adminG) <- suchThatMap genSharedAndEnt $ \(shared, eId, e, aA, aG) ->
                let notForked =
                      filter (\vId -> e ^. Sync.fork /= Just vId) . HM.keys $ shared ^. Sync.store . Sync.toVersions
                 in if null notForked then Nothing else Just (shared, eId, notForked, aA, aG)
              newFork <- elements notForked
              pure (s, eId, newFork, adminA, adminG)
         in forAll gen $ \(s, eId, newFork, adminActor, adminGroup) -> do
              let go :: m Sync.Shared
                  go = do
                    worked <- updateFork (NE.singleton adminActor) eId (Just newFork)
                    unless worked $ error $ "Couldn't remove version " <> show (newFork)
                    genSyncShared
              s' <- runTerseDB (commit go) s
              (s' ^? Sync.store . Sync.toEntities . ix eId . Sync.fork . _Just) `shouldBe` Just newFork
              (s' ^? Sync.temp . Sync.toForksFrom . ix newFork . ix eId) `shouldBe` Just ()
      it "Move an Entity" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              not (null $ s ^. Sync.store . Sync.toEntities) && length (s ^. Sync.store . Sync.toSpaces) >= 2
         in forAll gen $ \(s, adminActor, adminGroup) ->
              forAll (elements . HM.toList $ s ^. Sync.store . Sync.toEntities) $ \(eId, e) ->
                forAll
                  ( elements
                      . HM.keys
                      . HM.filter
                        (\es -> not $ eId `HS.member` es)
                      $ s ^. Sync.store . Sync.toSpaces
                  )
                  $ \newSId -> do
                    let go :: m Sync.Shared
                        go = do
                          worked <-
                            setEntityPermission
                              (NE.singleton adminActor)
                              Delete
                              adminGroup
                              (s ^?! Sync.temp . Sync.toSpaceOf . ix eId)
                          unless worked $ error "Couldn't set delete entity permission"
                          worked <- setEntityPermission (NE.singleton adminActor) Update adminGroup newSId
                          unless worked $ error "Couldn't set create entity permission"
                          worked <- moveEntity (NE.singleton adminActor) eId newSId
                          unless worked $ error $ "Couldn't move entity " <> show (eId, newSId)
                          genSyncShared
                    s' <- runTerseDB (commit go) s
                    (s' ^? Sync.temp . Sync.toSpaceOf . ix eId) `shouldBe` Just newSId
                    (s' ^? Sync.store . Sync.toSpaces . ix newSId . ix eId) `shouldBe` Just ()
                    (s' ^? Sync.store . Sync.toSpaces . ix (s ^?! Sync.temp . Sync.toSpaceOf . ix eId) . ix eId)
                      `shouldBe` Nothing
      it "Offset a Version" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              not
                . null
                . HM.filter (\e -> length (e ^. Sync.versions) > 1)
                $ s ^. Sync.store . Sync.toEntities
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let genE =
                    elements . HM.toList . HM.filter (\e -> length (e ^. Sync.versions) > 1) $
                      s ^. Sync.store . Sync.toEntities
               in forAll genE $ \(eId, e) ->
                    let genV = do
                          v <- elements . NE.toList $ e ^. Sync.versions
                          offsetDirection <- arbitrary
                          offsetMagnitude <- do
                            let idx = fromJust . elemIndex v . NE.toList $ e ^. Sync.versions
                            if offsetDirection
                              then chooseInt (0, length (e ^. Sync.versions) - idx)
                              else negate <$> chooseInt (0, idx)
                          pure (v, offsetMagnitude)
                     in forAll genV $ \(vId, offset) -> do
                          let go :: m Sync.Shared
                              go = do
                                worked <- offsetVersionIndex (NE.singleton adminActor) vId offset
                                unless worked $ error $ "Couldn't offset version " <> show (eId, vId)
                                genSyncShared
                          s' <- runTerseDB (commit go) s
                          shouldSatisfy (s' ^? Sync.store . Sync.toEntities . ix eId . Sync.versions) $ \mVs ->
                            let vs = NE.toList $ fromJust mVs
                                oldIdx = fromJust . elemIndex vId . NE.toList $ e ^. Sync.versions
                                newIdx = fromJust $ elemIndex vId vs
                             in (newIdx - oldIdx) <= offset
      it "Set a Version Index" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              not
                . null
                . HM.filter (\e -> length (e ^. Sync.versions) > 1)
                $ s ^. Sync.store . Sync.toEntities
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let genE =
                    elements . HM.toList . HM.filter (\e -> length (e ^. Sync.versions) > 1) $
                      s ^. Sync.store . Sync.toEntities
               in forAll genE $ \(eId, e) ->
                    let genV = do
                          v <- elements . NE.toList $ e ^. Sync.versions
                          idx <- chooseInt (0, length (e ^. Sync.versions) - 1)
                          pure (v, idx)
                     in forAll genV $ \(vId, idx) -> do
                          let go :: m Sync.Shared
                              go = do
                                worked <- setVersionIndex (NE.singleton adminActor) vId idx
                                unless worked $ error $ "Couldn't set version index " <> show (eId, vId)
                                genSyncShared
                          s' <- runTerseDB (commit go) s
                          (s' ^? Sync.store . Sync.toEntities . ix eId . Sync.versions . ix idx) `shouldBe` Just vId
  describe "Group" $ do
    -- updating what it inherits from / who it inherits to
    describe "Should Succeed" $ do
      it "deleting parent relationship should affect both nodes, and unrelate them" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              not . null . HM.filter (\g -> isJust (g ^. Sync.prev)) $
                s ^. Sync.store . Sync.toGroups . Sync.nodes
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let gsWithParent = HM.filter (\g -> isJust (g ^. Sync.prev)) $
                    s ^. Sync.store . Sync.toGroups . Sync.nodes
               in forAll (elements (HM.toList gsWithParent)) $ \(gId :: GroupId, g) -> do
                    let go :: m Sync.Shared
                        go = do
                          worked <- updateGroupParent (NE.singleton adminActor) gId Nothing
                          unless worked $ error $ "Couldn't set group parent " <> show (gId)
                          genSyncShared
                        parentId = fromJust $ g ^. Sync.prev
                    s' <- runTerseDB (commit go) s
                    (s' ^? Sync.store . Sync.toGroups . Sync.nodes . ix gId . Sync.prev . _Just) `shouldBe` Nothing
                    (s' ^? Sync.store . Sync.toGroups . Sync.nodes . ix parentId . Sync.next . ix gId)
                      `shouldBe` Nothing
      it "deleting child relationship should affect both nodes, and unrelate them" $
        let gen = suchThat arbitraryShared $ \(s, _, _) ->
              not . null . HM.filter (\g -> isJust (g ^. Sync.prev)) $
                s ^. Sync.store . Sync.toGroups . Sync.nodes
         in forAll gen $ \(s, adminActor, adminGroup) ->
              let gsWithParent = HM.filter (\g -> isJust (g ^. Sync.prev)) $ s ^. Sync.store . Sync.toGroups . Sync.nodes
               in forAll (elements (HM.toList gsWithParent)) $ \(gId :: GroupId, g) -> do
                    let go :: m Sync.Shared
                        go = do
                          worked <- unlinkGroups (NE.singleton adminActor) parentId gId
                          unless worked $ error $ "Couldn't unlink groups " <> show (parentId, gId)
                          genSyncShared
                        parentId = fromJust $ g ^. Sync.prev
                    s' <- runTerseDB (commit go) s
                    (s' ^? Sync.store . Sync.toGroups . Sync.nodes . ix gId . Sync.prev . _Just) `shouldBe` Nothing
                    (s' ^? Sync.store . Sync.toGroups . Sync.nodes . ix parentId . Sync.next . ix gId)
                      `shouldBe` Nothing
      it "adding child relationship should cause inheritance" $
        let gen = do
              (s', aA, aG) <- suchThat arbitraryShared $ \(s, _, _) ->
                not . null . HM.filter (\g -> isJust (g ^. Sync.prev)) $
                  s ^. Sync.store . Sync.toGroups . Sync.nodes
              nG <- arbitrary
              pure (s', aA, aG, nG)
         in forAll gen $ \(s, adminActor, adminGroup, newGId) ->
              let gsWithParent = HM.filter (\g -> isJust (g ^. Sync.prev)) $ s ^. Sync.store . Sync.toGroups . Sync.nodes
               in forAll (elements (HM.toList gsWithParent)) $ \(gId :: GroupId, _) -> do
                    let go :: m Sync.Shared
                        go = do
                          worked <- storeGroup (NE.singleton adminActor) newGId
                          unless worked $ error $ "Couldn't Sync.store group " <> show newGId
                          worked <- linkGroups (NE.singleton adminActor) gId newGId
                          unless worked $ error $ "Couldn't set child " <> show (gId, newGId)
                          genSyncShared
                    s' <- runTerseDB (commit go) s
                    shouldSatisfy (s', gId, newGId) $ \_ ->
                      fromJust (s' ^? Sync.temp . Sync.toTabulatedGroups . ix gId)
                        `hasLessOrEqualPermissionsTo` fromJust (s' ^? Sync.temp . Sync.toTabulatedGroups . ix newGId)
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
