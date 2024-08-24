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

module Spec.Sync.Test.Safe.Create where

import Control.Lens (at, non, (^.))
import Control.Monad.Extra (unless, when)
import Control.Monad.State (MonadState, execState)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, isNothing)
import Lib.Sync.Actions.Safe (emptyShared)
import Lib.Sync.Actions.Safe.Store (
  addMember,
  storeActor,
  storeEntity,
  storeGroup,
  storeNextVersion,
  storeSpace,
 )
import Lib.Sync.Actions.Safe.Update.Group (
  setEntityPermission,
  setMemberPermission,
  setOrganizationPermission,
  setRecruiterPermission,
  setUniversePermission,
 )
import Lib.Sync.Types.Store (
  Shared,
  store,
  temp,
  toActors,
  toEntities,
  toGroups,
  toReferencesFrom,
  toSpaces,
  toVersions,
 )
import Lib.Sync.Types.Store.Groups (emptyGroup, members, nodes)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption (..),
 )
import Spec.Sync.Sample.Store (arbitraryEmptyShared, arbitraryShared)
import Test.QuickCheck (
  arbitrary,
  forAll,
  property,
  suchThat,
 )
import Test.Syd (Spec, describe, it, shouldSatisfy)

createTests :: Spec
createTests = describe "Create" $ do
  describe "Should Succeed" $ do
    it "Space via universe" $
      forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
        property $ \(aId :: ActorId, gId :: GroupId, sId :: SpaceId) ->
          let s' = flip execState s $ do
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
           in shouldSatisfy (s' ^. store . toSpaces . at sId) isJust
    it "Entity" $
      forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
        property $
          \( aId :: ActorId
            , gId :: GroupId
            , sId :: SpaceId
            , eId :: EntityId
            , vId :: VersionId
            ) -> do
              let s' = flip execState s $ do
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
                    mWorked <- storeEntity (NE.singleton aId) eId sId vId Nothing
                    case mWorked of
                      Just (Right ()) -> pure ()
                      _ -> error $ "Couldn't store entity " <> show (eId, vId)
              shouldSatisfy (s' ^. store . toEntities . at eId) isJust
              shouldSatisfy (s' ^. store . toVersions . at vId) isJust
    it "Next Version" $
      forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
        property $
          \( aId :: ActorId
            , gId :: GroupId
            , sId :: SpaceId
            , eId :: EntityId
            , vId :: VersionId
            , vId' :: VersionId
            ) ->
              let s' = flip execState s $ do
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
                    mWorked <- storeEntity (NE.singleton aId) eId sId vId Nothing
                    case mWorked of
                      Just (Right ()) -> pure ()
                      _ -> error $ "Couldn't store entity " <> show (eId, vId)
                    mWorked <- storeNextVersion (NE.singleton aId) eId vId'
                    case mWorked of
                      Just (Right ()) -> pure ()
                      _ -> error $ "Couldn't store version " <> show mWorked
               in shouldSatisfy (s' ^. store . toVersions . at vId') isJust
    it "Group via organization" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          , gId' :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                s' = flip execState s $ do
                  setStage adminActor adminGroup aId gId
                  worked <-
                    setOrganizationPermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Create False)
                      gId
                  unless worked $ error $ "Couldn't set organization permission " <> show gId
                  worked <- storeGroup (NE.singleton aId) gId'
                  unless worked $ error $ "Couldn't store group " <> show gId'
             in shouldSatisfy s' $ \_ ->
                  isJust (s' ^. store . toGroups . nodes . at gId')
    it "Actor via recruiter" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          , aId' :: ActorId
          ) ->
            let s = emptyShared adminActor adminGroup
                s' = flip execState s $ do
                  setStage adminActor adminGroup aId gId
                  worked <- setRecruiterPermission (NE.singleton adminActor) Create gId
                  unless worked $ error $ "Couldn't set recruiter permission " <> show gId
                  worked <- storeActor (NE.singleton aId) aId'
                  unless worked $ error $ "Couldn't store actor " <> show aId'
             in shouldSatisfy s' $ \_ ->
                  isJust (s' ^. store . toActors . at aId')
    it "Member" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          , aId' :: ActorId
          , gId'
          ) ->
            let s = emptyShared adminActor adminGroup
                s' = flip execState s $ do
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
             in shouldSatisfy s' $ \_ ->
                  isJust
                    (s' ^. store . toGroups . nodes . at gId' . non emptyGroup . members . at aId')
  describe "Should Fail" $ do
    it "Space via universe" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          , sId :: SpaceId
          ) ->
            let s = emptyShared adminActor adminGroup
                s' = flip execState s $ do
                  setStage adminActor adminGroup aId gId
                  worked <- storeSpace (NE.singleton aId) sId
                  when worked $ error $ "Could store space " <> show (aId, gId, sId)
             in shouldSatisfy s' $ \_ -> isNothing $ s' ^. store . toSpaces . at sId
    it "Entity" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          , sId :: SpaceId
          , eId :: EntityId
          , vId :: VersionId
          ) ->
            let s = emptyShared adminActor adminGroup
                s' = flip execState s $ do
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
                  mWorked <- storeEntity (NE.singleton aId) eId sId vId Nothing
                  case mWorked of
                    Just (Right ()) -> error $ "Couldn't store entity " <> show (eId, vId)
                    _ -> pure ()
             in shouldSatisfy s' $ \_ ->
                  isNothing (s' ^. store . toEntities . at eId)
                    && isNothing (s' ^. store . toVersions . at vId)
    -- FIXME test for forking
    it "Next Version" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          , sId :: SpaceId
          , eId :: EntityId
          , vId :: VersionId
          , vId' :: VersionId
          ) ->
            let s = emptyShared adminActor adminGroup
                s' = flip execState s $ do
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
                  mWorked <- storeEntity (NE.singleton aId) eId sId vId Nothing
                  case mWorked of
                    Just (Right ()) -> pure ()
                    _ -> error $ "Couldn't store entity " <> show (eId, vId)
                  worked <- setEntityPermission (NE.singleton adminActor) Read gId sId
                  unless worked $ error $ "Couldn't set entity permission " <> show (gId, sId)
                  mWorked <- storeNextVersion (NE.singleton aId) eId vId'
                  case mWorked of
                    Just (Right ()) -> error $ "Could store version " <> show mWorked
                    _ -> pure ()
             in shouldSatisfy s' $ \_ ->
                  isJust (s' ^. store . toEntities . at eId)
                    && isNothing (s' ^. store . toVersions . at vId')
    it "Group via organization" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          , gId' :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                s' = flip execState s $ do
                  setStage adminActor adminGroup aId gId
                  worked <- storeGroup (NE.singleton aId) gId'
                  when worked $ error $ "Could store group " <> show gId'
             in shouldSatisfy s' $ \_ ->
                  isNothing (s' ^. store . toGroups . nodes . at gId')
    it "Actor via recruiter" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          , aId' :: ActorId
          ) ->
            let s = emptyShared adminActor adminGroup
                s' = flip execState s $ do
                  setStage adminActor adminGroup aId gId
                  worked <- storeActor (NE.singleton aId) aId'
                  when worked $ error $ "Could store actor " <> show aId'
             in shouldSatisfy s' $ \_ ->
                  isNothing (s' ^. store . toActors . at aId')
    it "Member" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          , aId' :: ActorId
          , gId' :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                s' = flip execState s $ do
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
             in shouldSatisfy s' $ \_ ->
                  isNothing
                    (s' ^. store . toGroups . nodes . at gId' . non emptyGroup . members . at aId')

setStage
  :: (MonadState Shared m) => ActorId -> GroupId -> ActorId -> GroupId -> m ()
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
