module Spec.Generic.Create where

import Lib.Class (TerseDB (genSyncShared, commit), TerseDBGen (runTerseDB), storeSpace, setUniversePermission, storeActor, storeGroup, setMemberPermission, addMember, setEntityPermission, storeEntity, storeNextVersion, setOrganizationPermission)
import Data.Data (Proxy(Proxy))
import Test.Syd (Spec, describe, it, shouldSatisfy)
import Lib.Types.Id (ActorId, GroupId, SpaceId, EntityId, VersionId)
import Test.QuickCheck (forAll, Testable (property))
import Spec.Sync.Sample.Store (arbitraryEmptyShared)
import qualified Lib.Sync.Types.Store as Sync
import qualified Lib.Sync.Types.Store.Groups as Sync
import Control.Lens ((^.), At (at))
import Control.Monad (unless)
import qualified Data.List.NonEmpty as NE
import Lib.Types.Permission (
  CollectionPermissionWithExemption(..), CollectionPermission (..))
import Data.Maybe (isJust)

createTests :: forall m n. (TerseDB n m, TerseDBGen n) => Proxy m -> Spec
createTests Proxy = do
  describe "Should Succeed" $ do
    it "Space via universe" $
      forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
        property $ \(aId :: ActorId, gId :: GroupId, sId :: SpaceId) -> do
          let go :: m Sync.Shared
              go = do
                setStage adminActor adminGroup aId gId
                worked <-
                  setUniversePermission
                    (NE.singleton adminActor)
                    (CollectionPermissionWithExemption Create False)
                    gId
                unless worked $
                  error $
                    "Couldn't grant universe create permissions " <> show gId
                worked <- storeSpace (NE.singleton aId) sId
                unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
                genSyncShared
          s' <- runTerseDB (commit go) s
          shouldSatisfy (s' ^. Sync.store . Sync.toSpaces . at sId) isJust
    it "Space via universe" $
      forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
        property $ \(aId :: ActorId, gId :: GroupId, sId :: SpaceId) -> do
          let go :: m Sync.Shared
              go = do
                setStage adminActor adminGroup aId gId
                worked <-
                  setUniversePermission
                    (NE.singleton adminActor)
                    (CollectionPermissionWithExemption Create False)
                    gId
                unless worked $
                  error $
                    "Couldn't grant universe create permissions " <> show gId
                worked <- storeSpace (NE.singleton aId) sId
                unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
                genSyncShared
          s' <- runTerseDB (commit go) s
          shouldSatisfy (s' ^. Sync.store . Sync.toSpaces . at sId) isJust
    it "Entity" $
      forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
        property $
          \( aId :: ActorId
            , gId :: GroupId
            , sId :: SpaceId
            , eId :: EntityId
            , vId :: VersionId
            ) -> do
              let go :: m Sync.Shared
                  go = do
                    setStage adminActor adminGroup aId gId
                    worked <-
                      setUniversePermission
                        (NE.singleton adminActor)
                        (CollectionPermissionWithExemption Create False)
                        gId
                    unless worked $
                      error $
                        "Couldn't grant universe create permissions " <> show gId
                    worked <- storeSpace (NE.singleton aId) sId
                    unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
                    worked <- setEntityPermission (NE.singleton adminActor) Create gId sId
                    unless worked $ error $ "Couldn't set entity permission " <> show (gId, sId)
                    worked <- storeEntity (NE.singleton aId) eId sId vId Nothing
                    unless worked $ error $ "Couldn't store entity " <> show (eId, vId)
                    genSyncShared
              s' <- runTerseDB (commit go) s
              shouldSatisfy (s' ^. Sync.store . Sync.toEntities . at eId) isJust
              shouldSatisfy (s' ^. Sync.store . Sync.toVersions . at vId) isJust
    it "Next Version" $
      forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
        property $
          \( aId :: ActorId
            , gId :: GroupId
            , sId :: SpaceId
            , eId :: EntityId
            , vId :: VersionId
            , vId' :: VersionId
            ) -> do
              let go :: m Sync.Shared
                  go = do
                    setStage adminActor adminGroup aId gId
                    worked <-
                      setUniversePermission
                        (NE.singleton adminActor)
                        (CollectionPermissionWithExemption Create False)
                        gId
                    unless worked $
                      error $
                        "Couldn't grant universe create permissions " <> show gId
                    worked <- storeSpace (NE.singleton aId) sId
                    unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
                    worked <- setEntityPermission (NE.singleton adminActor) Update gId sId
                    unless worked $ error $ "Couldn't set entity permission " <> show (gId, sId)
                    worked <- storeEntity (NE.singleton aId) eId sId vId Nothing
                    unless worked $ error $ "Couldn't store entity " <> show (eId, vId)
                    worked <- storeNextVersion (NE.singleton aId) eId vId'
                    unless worked $ error $ "Couldn't store version " <> show vId
                    genSyncShared
              s' <- runTerseDB (commit go) s
              shouldSatisfy (s' ^. Sync.store . Sync.toVersions . at vId') isJust
    it "Group via organization" $
      forAll arbitraryEmptyShared $
        \( s
          , adminActor :: ActorId
          , adminGroup :: GroupId
          ) ->
          property $
         \( aId :: ActorId
          , gId :: GroupId
          , gId' :: GroupId
          ) -> do
            let go :: m Sync.Shared
                go = do
                  setStage adminActor adminGroup aId gId
                  worked <-
                    setOrganizationPermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Create False)
                      gId
                  unless worked $ error $ "Couldn't set organization permission " <> show gId
                  worked <- storeGroup (NE.singleton aId) gId'
                  unless worked $ error $ "Couldn't store group " <> show gId'
                  genSyncShared
            s' <- runTerseDB (commit go) s
            shouldSatisfy s' $ \_ ->
              isJust (s' ^. Sync.store . Sync.toGroups . Sync.nodes . at gId')

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
