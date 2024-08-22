module Spec.Test.Safe.Read where

import Control.Lens ((^.))
import Control.Monad.Extra (unless)
import Control.Monad.State (State, evalState, execState)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import Lib.Actions.Safe (emptyShared)
import Lib.Actions.Safe.Store (
  addMember,
  storeActor,
  storeEntity,
  storeGroup,
  storeSpace,
 )
import Lib.Actions.Safe.Update.Group (
  setEntityPermission,
  setGroupPermission,
  setMemberPermission,
  setOrganizationPermission,
  setRecruiterPermission,
  setSpacePermission,
  setUniversePermission,
 )
import Lib.Actions.Safe.Verify.Actor (anyCanReadActor)
import Lib.Actions.Safe.Verify.Group (anyCanReadGroup)
import Lib.Actions.Safe.Verify.Member (anyCanReadMember)
import Lib.Actions.Safe.Verify.SpaceAndEntity (
  anyCanReadAllEntities,
  anyCanReadEntity,
  anyCanReadSpace,
  anyCanReadSpaceOld,
  anyCanReadVersion,
 )
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption (..),
  SinglePermission (NonExistent),
 )
import Lib.Types.Store (
  Shared,
  store,
  toActors,
  toSpaces,
 )
import Spec.Sample.Store (
  arbitraryEmptyShared,
  arbitraryShared,
 )
import Test.QuickCheck (
  elements,
  forAll,
  property,
  suchThat,
 )
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)

readTests :: Spec
readTests = describe "Read" $ do
  describe "New vs. Old" $ do
    it "Spaces" $
      let gen = suchThat arbitraryShared $ \(s, _, _) ->
            not (null (s ^. store . toActors)) && not (null (s ^. store . toSpaces))
       in forAll gen $ \(s, _, _) ->
            let genAId = elements . HM.keys $ s ^. store . toActors
                genSId = elements . HM.keys $ s ^. store . toSpaces
             in forAll ((,) <$> genAId <*> genSId) $ \(aId, sId) ->
                  let resNew = evalState (anyCanReadSpace (NE.singleton aId) sId) s
                      resOld = evalState (anyCanReadSpaceOld (NE.singleton aId) sId) s
                   in resNew `shouldBe` resOld
  describe "Should Succeed" $ do
    it "Spaces" $
      forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
        property $ \(sId :: SpaceId, aId :: ActorId, gId :: GroupId) ->
          let go :: State Shared Bool
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
                anyCanReadSpace (NE.singleton aId) sId
           in evalState go s `shouldBe` True
    it "Entities" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , sId :: SpaceId
          , aId :: ActorId
          , gId :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
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
                  anyCanReadAllEntities (NE.singleton aId) sId
                s' = execState go s
             in shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == True
    it "Entity" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , sId :: SpaceId
          , aId :: ActorId
          , gId :: GroupId
          , eId :: EntityId
          , vId :: VersionId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeSpace (NE.singleton adminActor) sId
                  unless worked $ error $ "Couldn't make space " <> show sId
                  worked <- setEntityPermission (NE.singleton adminActor) Create adminGroup sId
                  unless worked $ error $ "Couldn't grant entity permissions " <> show sId
                  mWorked <- storeEntity (NE.singleton adminActor) eId sId vId Nothing
                  case mWorked of
                    Just (Right ()) -> pure ()
                    _ -> error $ "Couldn't store entity " <> show (eId, vId)
                  worked <-
                    setUniversePermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Read False)
                      gId
                  unless worked $ error $ "Couldn't set universe permission " <> show gId
                  worked <- setEntityPermission (NE.singleton adminActor) Read gId sId
                  unless worked $ error $ "Couldn't set entity permission " <> show gId
                  anyCanReadEntity (NE.singleton aId) eId
                s' = execState go s
             in shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == True
    it "Version" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , sId :: SpaceId
          , aId :: ActorId
          , gId :: GroupId
          , eId :: EntityId
          , vId :: VersionId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeSpace (NE.singleton adminActor) sId
                  unless worked $ error $ "Couldn't make space " <> show sId
                  worked <- setEntityPermission (NE.singleton adminActor) Create adminGroup sId
                  unless worked $ error $ "Couldn't grant entity permissions " <> show sId
                  mWorked <- storeEntity (NE.singleton adminActor) eId sId vId Nothing
                  case mWorked of
                    Just (Right ()) -> pure ()
                    _ -> error $ "Couldn't store entity " <> show (eId, vId)
                  worked <-
                    setUniversePermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Read False)
                      gId
                  unless worked $ error $ "Couldn't set universe permission " <> show gId
                  worked <- setEntityPermission (NE.singleton adminActor) Read gId sId
                  unless worked $ error $ "Couldn't set entity permission " <> show gId
                  anyCanReadVersion (NE.singleton aId) vId
                s' = execState go s
             in shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == True
    it "Group" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , gId' :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeGroup (NE.singleton adminActor) gId'
                  unless worked $ error $ "Couldn't make group " <> show gId'
                  worked <-
                    setOrganizationPermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Read False)
                      gId
                  unless worked $ error $ "Couldn't set organization permission " <> show gId
                  anyCanReadGroup (NE.singleton aId) gId'
                s' = execState go s
             in shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == True
    it "Member" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , gId' :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeGroup (NE.singleton adminActor) gId'
                  unless worked $ error $ "Couldn't make group " <> show gId'
                  worked <-
                    setOrganizationPermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Read False)
                      gId
                  unless worked $ error $ "Couldn't set organization permission " <> show gId
                  worked <- setMemberPermission (NE.singleton adminActor) Read gId gId'
                  unless worked $ error $ "Couldn't set member permission " <> show gId
                  anyCanReadMember (NE.singleton aId) gId'
                s' = execState go s
             in shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == True
    it "Actor" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- setRecruiterPermission (NE.singleton adminActor) Read gId
              unless worked $ error $ "Couldn't set recruiter permission " <> show gId
              anyCanReadActor (NE.singleton aId)
            s' = execState go s
         in shouldSatisfy (s', aId, gId) $ \_ -> evalState go s == True
  describe "Should Fail" $ do
    it "Spaces when explicit via universe" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , sId :: SpaceId
          , aId :: ActorId
          , gId :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeSpace (NE.singleton adminActor) sId
                  unless worked $ error $ "Couldn't make space " <> show sId
                  worked <-
                    setUniversePermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Blind False)
                      gId
                  unless worked $ error $ "Couldn't set universe permission " <> show gId
                  anyCanReadSpace (NE.singleton aId) sId
                s' = execState go s
             in shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Spaces when explicit" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , sId :: SpaceId
          , aId :: ActorId
          , gId :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
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
                  worked <-
                    setSpacePermission (NE.singleton adminActor) (Just NonExistent) gId sId
                  unless worked $ error $ "Couldn't set spaces permission " <> show (gId, sId)
                  anyCanReadSpace (NE.singleton aId) sId
                s' = execState go s
             in shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Spaces when implicit" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , sId :: SpaceId
          , aId :: ActorId
          , gId :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeSpace (NE.singleton adminActor) sId
                  unless worked $ error $ "Couldn't make space " <> show sId
                  anyCanReadSpace (NE.singleton aId) sId
                s' = execState go s
             in shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Entities when explicit" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , sId :: SpaceId
          , aId :: ActorId
          , gId :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
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
                  worked <- setEntityPermission (NE.singleton adminActor) Blind gId sId
                  unless worked $ error $ "Couldn't set entity permission " <> show gId
                  anyCanReadAllEntities (NE.singleton aId) sId
                s' = execState go s
             in shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Entity when explicit" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , sId :: SpaceId
          , aId :: ActorId
          , gId :: GroupId
          , eId :: EntityId
          , vId :: VersionId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeSpace (NE.singleton adminActor) sId
                  unless worked $ error $ "Couldn't make space " <> show sId
                  worked <- setEntityPermission (NE.singleton adminActor) Create adminGroup sId
                  unless worked $ error $ "Couldn't grant entity permissions " <> show sId
                  mWorked <- storeEntity (NE.singleton adminActor) eId sId vId Nothing
                  case mWorked of
                    Just (Right ()) -> pure ()
                    _ -> error $ "Couldn't make entity " <> show (eId, vId)
                  worked <-
                    setUniversePermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Read False)
                      gId
                  unless worked $ error $ "Couldn't set universe permission " <> show gId
                  worked <- setEntityPermission (NE.singleton adminActor) Blind gId sId
                  unless worked $ error $ "Couldn't set entity permission " <> show gId
                  anyCanReadEntity (NE.singleton aId) eId
                s' = execState go s
             in shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Entities when implicit" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , sId :: SpaceId
          , aId :: ActorId
          , gId :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
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
                  anyCanReadAllEntities (NE.singleton aId) sId
                s' = execState go s
             in shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Entity when implicit" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , sId :: SpaceId
          , aId :: ActorId
          , gId :: GroupId
          , eId :: EntityId
          , vId :: VersionId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeSpace (NE.singleton adminActor) sId
                  unless worked $ error $ "Couldn't make space " <> show sId
                  worked <- setEntityPermission (NE.singleton adminActor) Create adminGroup sId
                  unless worked $ error $ "Couldn't grant entity permissions " <> show sId
                  mWorked <- storeEntity (NE.singleton adminActor) eId sId vId Nothing
                  case mWorked of
                    Just (Right ()) -> pure ()
                    _ -> error $ "Couldn't make entity " <> show (eId, vId)
                  worked <-
                    setUniversePermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Read False)
                      gId
                  unless worked $ error $ "Couldn't set universe permission " <> show gId
                  anyCanReadEntity (NE.singleton aId) eId
                s' = execState go s
             in shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Group when explicit via universe" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , gId' :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeGroup (NE.singleton adminActor) gId'
                  unless worked $ error $ "Couldn't make group " <> show gId'
                  worked <-
                    setOrganizationPermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Blind False)
                      gId
                  unless worked $ error $ "Couldn't set organization permission " <> show gId
                  anyCanReadGroup (NE.singleton aId) gId'
                s' = execState go s
             in shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == False
    it "Group when explicit" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , gId' :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeGroup (NE.singleton adminActor) gId'
                  unless worked $ error $ "Couldn't make group " <> show gId'
                  worked <-
                    setOrganizationPermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Read False)
                      gId
                  unless worked $ error $ "Couldn't set organization permission " <> show gId
                  worked <-
                    setGroupPermission (NE.singleton adminActor) (Just NonExistent) gId gId'
                  unless worked $ error $ "Couldn't set group permission " <> show (gId, gId')
                  anyCanReadGroup (NE.singleton aId) gId'
                s' = execState go s
             in shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == False
    it "Group when implicit" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , gId' :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeGroup (NE.singleton adminActor) gId'
                  unless worked $ error $ "Couldn't make group " <> show gId'
                  anyCanReadGroup (NE.singleton aId) gId'
                s' = execState go s
             in shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == False
    it "Member when explicit" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , gId' :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeGroup (NE.singleton adminActor) gId'
                  unless worked $ error $ "Couldn't make group " <> show gId'
                  worked <-
                    setOrganizationPermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Read False)
                      gId
                  unless worked $ error $ "Couldn't set organization permission " <> show gId
                  worked <- setMemberPermission (NE.singleton adminActor) Blind gId gId'
                  unless worked $ error $ "Couldn't set member permission " <> show gId
                  anyCanReadMember (NE.singleton aId) gId'
                s' = execState go s
             in shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == False
    it "Member when implicit" $
      property $
        \( adminActor :: ActorId
          , adminGroup :: GroupId
          , gId' :: GroupId
          , aId :: ActorId
          , gId :: GroupId
          ) ->
            let s = emptyShared adminActor adminGroup
                go :: State Shared Bool
                go = do
                  setup adminActor adminGroup aId gId
                  worked <- storeGroup (NE.singleton adminActor) gId'
                  unless worked $ error $ "Couldn't make group " <> show gId'
                  worked <-
                    setOrganizationPermission
                      (NE.singleton adminActor)
                      (CollectionPermissionWithExemption Read False)
                      gId
                  unless worked $ error $ "Couldn't set organization permission " <> show gId
                  anyCanReadMember (NE.singleton aId) gId'
                s' = execState go s
             in shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == False
    it "Actor when explicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- setRecruiterPermission (NE.singleton adminActor) Blind gId
              unless worked $ error $ "Couldn't set recruiter permission " <> show gId
              anyCanReadActor (NE.singleton aId)
            s' = execState go s
         in shouldSatisfy (s', aId, gId) $ \_ -> evalState go s == False
    it "Actor when implicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              anyCanReadActor (NE.singleton aId)
            s' = execState go s
         in shouldSatisfy (s', aId, gId) $ \_ -> evalState go s == False
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

-- FIXME each group or space or something should have a set of shit it specifically _can't_ see?
-- i.e., the shit it's blind to? Er, NonExists within the set, assuming read rights are present?

-- TODO test to see if setting SinglePermissions also causes the same effect - and combinations
-- of whether or not entities are granted, but space isn't, etc.
