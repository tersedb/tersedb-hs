module Spec.Generic.Joint where

import Test.Syd (Spec, it, context, shouldBe)
import Lib.Class (TerseDB (genSyncShared, commit), TerseDBGen (runTerseDB), storeActor, storeGroup, setMemberPermission, addMember, storeSpace, setUniversePermission, setEntityPermission, storeEntity, moveEntity, setSpacePermission)
import Data.Data (Proxy(Proxy))
import Lib.Types.Id (ActorId, GroupId, SpaceId, EntityId, VersionId)
import Lib.Types.Permission (CollectionPermission(..), CollectionPermissionWithExemption (..), SinglePermission (NonExistent))
import Control.Monad (unless)
import Test.QuickCheck (forAll, Testable (property))
import Spec.Sync.Sample.Store (arbitraryEmptyShared)
import qualified Data.List.NonEmpty as NE
import qualified Lib.Sync.Types.Store as Sync
import Control.Lens ((^?), Ixed (ix))

jointTests :: forall n m. (TerseDB n m, TerseDBGen n) => Proxy m -> Spec
jointTests Proxy = do
  it "Move Request Between Public Spaces" $
    forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
      property $
        \( actorA :: ActorId
          , groupA :: GroupId
          , actorB :: ActorId
          , groupB :: GroupId
          , spaceA :: SpaceId
          , spaceB :: SpaceId
          , eId :: EntityId
          , vId :: VersionId
          ) -> do
            let go :: m Sync.Shared
                go = do
                  worked <- storeActor (NE.singleton adminActor) actorA
                  unless worked $
                    error $
                      "Couldn't store actor " <> show actorA
                  worked <- storeActor (NE.singleton adminActor) actorB
                  unless worked $
                    error $
                      "Couldn't store actor " <> show actorB
                  worked <- storeGroup (NE.singleton adminActor) groupA
                  unless worked $
                    error $
                      "Couldn't store group " <> show groupA
                  worked <- storeGroup (NE.singleton adminActor) groupB
                  unless worked $
                    error $
                      "Couldn't store group " <> show groupB

                  worked <- setMemberPermission (NE.singleton adminActor) Create adminGroup groupA
                  unless worked $
                    error $
                      "Couldn't set member permission " <> show groupA
                  worked <- setMemberPermission (NE.singleton adminActor) Create adminGroup groupB
                  unless worked $
                    error $
                      "Couldn't set member permission " <> show groupB

                  worked <- addMember (NE.singleton adminActor) groupA actorA
                  unless worked $
                    error $
                      "Couldn't add member " <> show (groupA, actorA)
                  worked <- addMember (NE.singleton adminActor) groupB actorB
                  unless worked $
                    error $
                      "Couldn't add member " <> show (groupB, actorB)

                  worked <- storeSpace (NE.singleton adminActor) spaceA
                  unless worked $
                    error $
                      "Couldn't store space " <> show spaceA
                  worked <- storeSpace (NE.singleton adminActor) spaceB
                  unless worked $
                    error $
                      "Couldn't store space " <> show spaceB

                  worked <-
                    setUniversePermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Read False)
                      groupA
                  unless worked $
                    error $
                      "Couldn't set universe permission " <> show groupA
                  worked <-
                    setUniversePermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Read False)
                      groupB
                  unless worked $
                    error $
                      "Couldn't set universe permission " <> show groupB

                  worked <- setEntityPermission (NE.singleton adminActor) Delete groupA spaceA
                  unless worked $
                    error $
                      "Couldn't set space permission " <> show spaceA
                  worked <- setEntityPermission (NE.singleton adminActor) Delete groupB spaceB
                  unless worked $
                    error $
                      "Couldn't set space permission " <> show spaceB

                  worked <- storeEntity (NE.singleton actorA) eId spaceA vId Nothing
                  unless worked $ error $ "Couldn't store entity " <> show eId

                  worked <- moveEntity (NE.fromList [actorA, actorB]) eId spaceB
                  unless worked $
                    error $
                      "Couldn't move entity " <> show eId
                  genSyncShared
            s' <- runTerseDB (commit go) s
            context "Entity is in Space B" $
              (s' ^? Sync.store . Sync.toSpaces . ix spaceB . ix eId) `shouldBe` Just ()
            context "Entity is not in Space A" $
              (s' ^? Sync.store . Sync.toSpaces . ix spaceA . ix eId) `shouldBe` Nothing
  it "Move Request From Private Space To Public Spaces" $
    forAll arbitraryEmptyShared $
      \(s, adminActor, adminGroup) ->
        property $
          \( actorA :: ActorId
            , groupA :: GroupId
            , actorB :: ActorId
            , groupB :: GroupId
            , spaceA :: SpaceId
            , spaceB :: SpaceId
            , eId :: EntityId
            , vId :: VersionId
            ) -> do
              let go :: m Sync.Shared
                  go = do
                    worked <- storeActor (NE.singleton adminActor) actorA
                    unless worked $
                      error $
                        "Couldn't store actor " <> show actorA
                    worked <- storeActor (NE.singleton adminActor) actorB
                    unless worked $
                      error $
                        "Couldn't store actor " <> show actorB
                    worked <- storeGroup (NE.singleton adminActor) groupA
                    unless worked $
                      error $
                        "Couldn't store group " <> show groupA
                    worked <- storeGroup (NE.singleton adminActor) groupB
                    unless worked $
                      error $
                        "Couldn't store group " <> show groupB

                    worked <- setMemberPermission (NE.singleton adminActor) Create adminGroup groupA
                    unless worked $
                      error $
                        "Couldn't set member permission " <> show groupA
                    worked <- setMemberPermission (NE.singleton adminActor) Create adminGroup groupB
                    unless worked $
                      error $
                        "Couldn't set member permission " <> show groupB

                    worked <- addMember (NE.singleton adminActor) groupA actorA
                    unless worked $
                      error $
                        "Couldn't add member " <> show (groupA, actorA)
                    worked <- addMember (NE.singleton adminActor) groupB actorB
                    unless worked $
                      error $
                        "Couldn't add member " <> show (groupB, actorB)

                    worked <- storeSpace (NE.singleton adminActor) spaceA
                    unless worked $
                      error $
                        "Couldn't store space " <> show spaceA
                    worked <- storeSpace (NE.singleton adminActor) spaceB
                    unless worked $
                      error $
                        "Couldn't store space " <> show spaceB

                    worked <-
                      setUniversePermission
                        (NE.singleton adminActor)
                        (CollectionPermissionWithExemption Read False)
                        groupA
                    unless worked $
                      error $
                        "Couldn't set universe permission " <> show groupA
                    worked <-
                      setUniversePermission
                        (NE.singleton adminActor)
                        (CollectionPermissionWithExemption Read False)
                        groupB
                    unless worked $
                      error $
                        "Couldn't set universe permission " <> show groupB

                    worked <- setEntityPermission (NE.singleton adminActor) Delete groupA spaceA
                    unless worked $
                      error $
                        "Couldn't set space permission " <> show spaceA
                    worked <- setEntityPermission (NE.singleton adminActor) Delete groupB spaceB
                    unless worked $
                      error $
                        "Couldn't set space permission " <> show spaceB
                    worked <-
                      setSpacePermission (NE.singleton adminActor) (Just NonExistent) groupB spaceA
                    unless worked $
                      error $
                        "Couldn't set space permission " <> show spaceA

                    worked <- storeEntity (NE.singleton actorA) eId spaceA vId Nothing
                    unless worked $ error $ "Couldn't store entity " <> show eId

                    worked <- moveEntity (NE.fromList [actorA, actorB]) eId spaceB
                    unless worked $
                      error $
                        "Couldn't move entity " <> show eId
                    genSyncShared
              s' <- runTerseDB (commit go) s
              context "Entity is in Space B" $
                (s' ^? Sync.store . Sync.toSpaces . ix spaceB . ix eId) `shouldBe` Just ()
              context "Entity is not in Space A" $
                (s' ^? Sync.store . Sync.toSpaces . ix spaceA . ix eId) `shouldBe` Nothing
