module Spec.Generic.Create where

import Control.Lens (At (at), non, (^.))
import Control.Monad (unless, when)
import Data.Data (Proxy (Proxy))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, isNothing)
import Lib.Class (
  TerseDB (commit, genSyncShared),
  TerseDBGen (runTerseDB),
  addMember,
  setEntityPermission,
  setMemberPermission,
  setOrganizationPermission,
  setRecruiterPermission,
  setUniversePermission,
  storeActor,
  storeEntity,
  storeGroup,
  storeNextVersion,
  storeSpace,
 )
import qualified Lib.Sync.Types.Store as Sync
import qualified Lib.Sync.Types.Store.Groups as Sync
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption (..),
 )
import Spec.Sync.Sample.Store (arbitraryEmptyShared)
import Test.QuickCheck (Testable (property), forAll)
import Test.Syd (Spec, describe, it, shouldSatisfy)

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
    it "Actor via recruiter" $
      forAll arbitraryEmptyShared $
        \( s
          , adminActor :: ActorId
          , adminGroup :: GroupId
          ) ->
            property $
              \( aId :: ActorId
                , gId :: GroupId
                , aId' :: ActorId
                ) -> do
                  let go :: m Sync.Shared
                      go = do
                        setStage adminActor adminGroup aId gId
                        worked <- setRecruiterPermission (NE.singleton adminActor) Create gId
                        unless worked $ error $ "Couldn't set recruiter permission " <> show gId
                        worked <- storeActor (NE.singleton aId) aId'
                        unless worked $ error $ "Couldn't store actor " <> show aId'
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  shouldSatisfy s' $ \_ ->
                    isJust (s' ^. Sync.store . Sync.toActors . at aId')
    it "Member" $
      forAll arbitraryEmptyShared $
        \( s
          , adminActor :: ActorId
          , adminGroup :: GroupId
          ) ->
            property $
              \( aId :: ActorId
                , gId :: GroupId
                , aId' :: ActorId
                , gId'
                ) -> do
                  let go :: m Sync.Shared
                      go = do
                        setStage adminActor adminGroup aId gId
                        worked <- storeActor (NE.singleton adminActor) aId'
                        unless worked $ error $ "Couldn't store actor " <> show aId'
                        worked <- storeGroup (NE.singleton adminActor) gId'
                        unless worked $ error $ "Couldn't store group " <> show gId'
                        worked <- setMemberPermission (NE.singleton adminActor) Create gId gId'
                        unless worked $ error $ "Couldn't grant member permission " <> show (gId, gId')
                        worked <-
                          setOrganizationPermission
                            (NE.singleton adminActor)
                            (CollectionPermissionWithExemption Read False)
                            gId
                        unless worked $
                          error $
                            "Couldn't grant read organization permission " <> show aId
                        worked <- addMember (NE.singleton aId) gId' aId'
                        unless worked $ error $ "Couldn't add member " <> show (gId', aId')
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  shouldSatisfy s' $ \_ ->
                    isJust
                      ( s'
                          ^. Sync.store
                            . Sync.toGroups
                            . Sync.nodes
                            . at gId'
                            . non Sync.emptyGroup
                            . Sync.members
                            . at aId'
                      )
  describe "Should Fail" $ do
    it "Space via universe" $
      forAll arbitraryEmptyShared $
        \( s
          , adminActor :: ActorId
          , adminGroup :: GroupId
          ) ->
            property $
              \( aId :: ActorId
                , gId :: GroupId
                , sId :: SpaceId
                ) -> do
                  let go :: m Sync.Shared
                      go = do
                        setStage adminActor adminGroup aId gId
                        worked <- storeSpace (NE.singleton aId) sId
                        when worked $ error $ "Could store space " <> show (aId, gId, sId)
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  shouldSatisfy s' $ \_ -> isNothing $ s' ^. Sync.store . Sync.toSpaces . at sId
    it "Entity" $
      forAll arbitraryEmptyShared $
        \( s
          , adminActor :: ActorId
          , adminGroup :: GroupId
          ) ->
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
                        worked <- storeEntity (NE.singleton aId) eId sId vId Nothing
                        when worked $ error $ "Couldn't store entity " <> show (eId, vId)
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  shouldSatisfy s' $ \_ ->
                    isNothing (s' ^. Sync.store . Sync.toEntities . at eId)
                  shouldSatisfy s' $ \_ ->
                    isNothing (s' ^. Sync.store . Sync.toVersions . at vId)
    -- FIXME test for forking
    it "Next Version" $
      forAll arbitraryEmptyShared $
        \( s
          , adminActor :: ActorId
          , adminGroup :: GroupId
          ) ->
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
                        worked <- setEntityPermission (NE.singleton adminActor) Read gId sId
                        unless worked $ error $ "Couldn't set entity permission " <> show (gId, sId)
                        worked <- storeNextVersion (NE.singleton aId) eId vId'
                        when worked $ error $ "Could store version " <> show vId
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  shouldSatisfy s' $ \_ ->
                    isJust (s' ^. Sync.store . Sync.toEntities . at eId)
                  shouldSatisfy s' $ \_ ->
                    isNothing (s' ^. Sync.store . Sync.toVersions . at vId')
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
                        worked <- storeGroup (NE.singleton aId) gId'
                        when worked $ error $ "Could store group " <> show gId'
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  shouldSatisfy s' $ \_ ->
                    isNothing (s' ^. Sync.store . Sync.toGroups . Sync.nodes . at gId')
    it "Actor via recruiter" $
      forAll arbitraryEmptyShared $
        \( s
          , adminActor :: ActorId
          , adminGroup :: GroupId
          ) ->
            property $
              \( aId :: ActorId
                , gId :: GroupId
                , aId' :: ActorId
                ) -> do
                  let go :: m Sync.Shared
                      go = do
                        setStage adminActor adminGroup aId gId
                        worked <- storeActor (NE.singleton aId) aId'
                        when worked $ error $ "Could store actor " <> show aId'
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  shouldSatisfy s' $ \_ ->
                    isNothing (s' ^. Sync.store . Sync.toActors . at aId')
    it "Member" $
      forAll arbitraryEmptyShared $
        \( s
          , adminActor :: ActorId
          , adminGroup :: GroupId
          ) ->
            property $
              \( aId :: ActorId
                , gId :: GroupId
                , aId' :: ActorId
                , gId' :: GroupId
                ) -> do
                  let go :: m Sync.Shared
                      go = do
                        setStage adminActor adminGroup aId gId
                        worked <- storeActor (NE.singleton adminActor) aId'
                        unless worked $ error $ "Couldn't store actor " <> show aId'
                        worked <- storeGroup (NE.singleton adminActor) gId'
                        unless worked $ error $ "Couldn't store group " <> show gId'
                        worked <-
                          setOrganizationPermission
                            (NE.singleton adminActor)
                            (CollectionPermissionWithExemption Read False)
                            gId
                        unless worked $
                          error $
                            "Couldn't grant read organization permission " <> show aId
                        worked <- addMember (NE.singleton aId) gId' aId'
                        when worked $ error $ "Could add member " <> show (gId', aId')
                        genSyncShared
                  s' <- runTerseDB (commit go) s
                  shouldSatisfy s' $ \_ ->
                    isNothing
                      ( s'
                          ^. Sync.store
                            . Sync.toGroups
                            . Sync.nodes
                            . at gId'
                            . non Sync.emptyGroup
                            . Sync.members
                            . at aId'
                      )

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
