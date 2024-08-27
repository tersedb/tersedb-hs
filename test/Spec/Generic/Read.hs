module Spec.Generic.Read where

import Test.Syd (Spec, describe, it, shouldSatisfy, shouldBe)
import Test.QuickCheck (suchThat, forAll, elements, Testable (property))
import Spec.Sync.Sample.Store (arbitraryShared, arbitraryEmptyShared)
import Lib.Sync.Types.Store (store, toActors, toSpaces)
import Lib.Types.Id (SpaceId, ActorId, GroupId, EntityId, VersionId)
import qualified Lib.Sync.Types.Store as Sync
import qualified Lib.Sync.Types.Store.Groups as Sync
import Control.Lens ((^.), Ixed (ix), (^?!), (^?))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Data (Proxy(Proxy))
import Lib.Class (TerseDB (commit, anyCanReadSpace, anyCanReadSpaceOld, genSyncShared, anyCanReadAllEntities, anyCanReadVersion), storeEntity, TerseDBGen (runTerseDB), setUniversePermission, storeSpace, storeActor, storeGroup, setMemberPermission, addMember, setEntityPermission)
import qualified Data.List.NonEmpty as NE
import Lib.Types.Permission (CollectionPermission(Read, Create), CollectionPermissionWithExemption (CollectionPermissionWithExemption))
import Control.Monad (unless)

readTests :: forall m n. (TerseDB n m, TerseDBGen n) => Proxy m -> Spec
readTests Proxy = do
  describe "New vs. Old" $
    it "Spaces" $
      let gen = suchThat arbitraryShared $ \(s, _, _) ->
            not (null (s ^. store . toActors)) && not (null (s ^. store . toSpaces))
       in forAll gen $ \(s, _, _) ->
            let genAId = elements . HS.toList $ s ^. Sync.store . Sync.toActors
                genSId = elements . HM.keys $ s ^. Sync.store . Sync.toSpaces
             in forAll ((,) <$> genAId <*> genSId) $ \(aId, sId) -> do
                  let goNew :: m Bool
                      goNew = do
                        anyCanReadSpace (NE.singleton aId) sId
                      goOld :: m Bool
                      goOld = do
                        anyCanReadSpaceOld (NE.singleton aId) sId
                  resNew <- runTerseDB (commit goNew) s
                  resOld <- runTerseDB (commit goOld) s
                  let gs = HS.toList $ s ^?! Sync.temp . Sync.toMemberOf . ix aId
                  shouldSatisfy
                    ( gs
                    , (\g -> (g, s ^?! Sync.store . Sync.toGroups . Sync.nodes . ix g)) <$> gs
                    , (\g -> (g, s ^?! Sync.temp . Sync.toTabulatedGroups . ix g)) <$> gs
                    , s ^? Sync.temp . Sync.toSpacesHiddenTo . ix sId
                    , (\g -> (g, s ^? store . Sync.toSpacePermissions . ix g)) <$> gs
                    , resNew
                    , resOld
                    , aId
                    , sId
                    )
                    $ \_ -> resNew == resOld
  describe "Should Succeed" $ do
    it "Spaces" $
      forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
        property $ \(sId :: SpaceId, aId :: ActorId, gId :: GroupId) -> do
          let go :: m (Bool, Sync.Shared)
              go = do
                setup adminActor adminGroup aId gId
                worked <- storeSpace (NE.singleton adminActor) sId
                unless worked $ error $ "Couldn't make space " <> show sId
                worked <-
                  setUniversePermission
                    (NE.singleton adminActor)
                    (CollectionPermissionWithExemption Read False)
                    gId
                unless worked $ error $ "Couldn't set universe permission " <> show gId
                res <- anyCanReadSpace (NE.singleton aId) sId
                s' <- genSyncShared
                pure (res,s')
          (res,s') <- runTerseDB (commit go) s
          shouldSatisfy (s',sId,aId,gId) $ \_ -> res == True
    it "Entities" $
      forAll arbitraryEmptyShared $
        \( s
          , adminActor :: ActorId
          , adminGroup :: GroupId
          ) -> property $
           \( sId :: SpaceId
            , aId :: ActorId
            , gId :: GroupId
            ) -> do
            let go :: m (Bool, Sync.Shared)
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeSpace (NE.singleton adminActor) sId
                  unless worked $ error $ "Couldn't make space " <> show sId
                  worked <-
                    setUniversePermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Read False)
                      gId
                  unless worked $ error $ "Couldn't set universe permission " <> show gId
                  worked <- setEntityPermission (NE.singleton adminActor) Read gId sId
                  unless worked $ error $ "Couldn't set entity permission " <> show gId
                  res <- anyCanReadAllEntities (NE.singleton aId) sId
                  s' <- genSyncShared
                  pure (res,s')
            (res, s') <- runTerseDB (commit go) s
            shouldSatisfy (s', aId, gId, sId) $ \_ -> res == True
    it "Version" $
      forAll arbitraryEmptyShared $
        \( s
          , adminActor :: ActorId
          , adminGroup :: GroupId
          ) -> property $
         \( sId :: SpaceId
          , aId :: ActorId
          , gId :: GroupId
          , eId :: EntityId
          , vId :: VersionId
          ) -> do
            let go :: m (Bool, Sync.Shared)
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeSpace (NE.singleton adminActor) sId
                  unless worked $ error $ "Couldn't make space " <> show sId
                  worked <- setEntityPermission (NE.singleton adminActor) Create adminGroup sId
                  unless worked $ error $ "Couldn't grant entity permissions " <> show sId
                  worked <- storeEntity (NE.singleton adminActor) eId sId vId Nothing
                  unless worked $ error $ "Couldn't store entity " <> show (eId, vId)
                  worked <-
                    setUniversePermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Read False)
                      gId
                  unless worked $ error $ "Couldn't set universe permission " <> show gId
                  worked <- setEntityPermission (NE.singleton adminActor) Read gId sId
                  unless worked $ error $ "Couldn't set entity permission " <> show gId
                  res <- anyCanReadVersion (NE.singleton aId) vId
                  s' <- genSyncShared
                  pure (res,s')
            (res, s') <- runTerseDB (commit go) s
            shouldSatisfy (s', aId, gId, sId) $ \_ -> res == True
  where
    setup adminActor adminGroup aId gId = do
      worked <- storeActor (NE.singleton adminActor) aId
      unless worked $ error $ "Couldn't make actor " <> show aId
      worked <- storeGroup (NE.singleton adminActor) gId
      unless worked $ error $ "Couldn't make group " <> show gId
      worked <- setMemberPermission (NE.singleton adminActor) Create adminGroup gId
      unless worked $ error $ "Couldn't set group permission " <> show gId
      worked <- addMember (NE.singleton adminActor) gId aId
      unless worked $ error $ "Couldn't add member " <> show (gId, aId)
