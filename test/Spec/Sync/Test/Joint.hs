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

module Spec.Sync.Test.Joint where

import Control.Lens (ix, (^?))
import Control.Monad.Extra (unless)
import Control.Monad.State (execState)
import qualified Data.List.NonEmpty as NE
import Lib.Sync.Actions.Safe.Store (
  addMember,
  storeActor,
  storeEntity,
  storeGroup,
  storeSpace,
 )
import Lib.Sync.Actions.Safe.Update (updateEntitySpace)
import Lib.Sync.Actions.Safe.Update.Group (
  setEntityPermission,
  setMemberPermission,
  setUniversePermission,
  setSpacePermission,
 )
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission (Create, Delete, Read),
  CollectionPermissionWithExemption (..),
  SinglePermission (NonExistent),
 )
import Lib.Sync.Types.Store (
  store,
  toSpaces,
 )
import Lib.Sync.Types.Store.Space (entities)
import Spec.Sync.Sample.Store (
  arbitraryEmptyShared,
 )
import Test.QuickCheck (
  forAll,
  property,
 )
import Test.Syd (Spec, context, describe, it, shouldBe)

jointTests :: Spec
jointTests = describe "Joint" $ do
  it "Move Request Between Public Spaces" $
    forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
      property $ \( actorA :: ActorId
                    , groupA :: GroupId
                    , actorB :: ActorId
                    , groupB :: GroupId
                    , spaceA :: SpaceId
                    , spaceB :: SpaceId
                    , eId :: EntityId
                    , vId :: VersionId
                    ) -> do
        let s' = flip execState s $ do
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

              eWorked <- storeEntity (NE.singleton actorA) eId spaceA vId Nothing
              case eWorked of
                Just (Right ()) -> pure ()
                _ -> error $ "Couldn't store entity " <> show eId

              worked <- updateEntitySpace (NE.fromList [actorA, actorB]) eId spaceB
              unless worked $
                error $
                  "Couldn't move entity " <> show eId
        context "Entity is in Space B" $
          (s' ^? store . toSpaces . ix spaceB . entities . ix eId) `shouldBe` Just ()
        context "Entity is not in Space A" $
          (s' ^? store . toSpaces . ix spaceA . entities . ix eId) `shouldBe` Nothing
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
              let s' = flip execState s $ do
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

                    eWorked <- storeEntity (NE.singleton actorA) eId spaceA vId Nothing
                    case eWorked of
                      Just (Right ()) -> pure ()
                      _ -> error $ "Couldn't store entity " <> show eId

                    worked <- updateEntitySpace (NE.fromList [actorA, actorB]) eId spaceB
                    unless worked $
                      error $
                        "Couldn't move entity " <> show eId
              context "Entity is in Space B" $
                (s' ^? store . toSpaces . ix spaceB . entities . ix eId) `shouldBe` Just ()
              context "Entity is not in Space A" $
                (s' ^? store . toSpaces . ix spaceA . entities . ix eId) `shouldBe` Nothing
