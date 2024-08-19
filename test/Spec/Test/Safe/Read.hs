module Spec.Test.Safe.Read where

import Spec.Sample.Store
  ( SampleStore (..)
  , loadSample
  , arbitraryShared
  , arbitraryEmptyShared
  )

import Lib.Types.Id (GroupId, ActorId, SpaceId, EntityId, VersionId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption (..)
  , SinglePermission (NonExistent)
  )
import Lib.Types.Store
  ( Shared
  , store
  , toSpaces
  , toActors
  )
import Lib.Actions.Safe (emptyShared)
import Lib.Actions.Safe.Store (storeActor, storeSpace, storeGroup, addMember, storeEntity)
import Lib.Actions.Safe.Update.Group
  ( setUniversePermission
  , setOrganizationPermission
  , setRecruiterPermission
  , setMemberPermission
  , setEntityPermission
  , setSpacePermission
  , setGroupPermission
  )
import Lib.Actions.Safe.Verify.SpaceAndEntity
  ( canReadSpace
  , canReadSpaceOld
  , canReadEntity
  , canReadAllEntities
  , canReadVersion
  )
import Lib.Actions.Safe.Verify.Group (canReadGroup)
import Lib.Actions.Safe.Verify.Member (canReadMember)
import Lib.Actions.Safe.Verify.Actor (canReadActor)

import qualified Data.HashMap.Strict as HM
import Control.Monad.Extra (unless)
import Control.Monad.State (execState, evalState, State)
import Control.Lens ((^.))
import Test.Syd (Spec, describe, it, shouldSatisfy, shouldBe)
import Test.QuickCheck
  ( property
  , forAll
  , elements
  , arbitrary
  , suchThat
  )

readTests :: Spec
readTests = describe "Read" $ do
  describe "New vs. Old" $ do
    it "Spaces" $
      let gen = suchThat arbitraryShared $ \(s,_,_) ->
            not (null (s ^. store . toActors)) && not (null (s ^. store . toSpaces))
      in  forAll gen $ \(s, _, _) ->
            let genAId = elements . HM.keys $ s ^. store . toActors
                genSId = elements . HM.keys $ s ^. store . toSpaces
            in  forAll ((,) <$> genAId <*> genSId) $ \(aId, sId) ->
                  let resNew = evalState (canReadSpace aId sId) s
                      resOld = evalState (canReadSpaceOld aId sId) s
                  in  resNew `shouldBe` resOld
  describe "Should Succeed" $ do
    it "Spaces" $
      forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
        property $ \(sId :: SpaceId, aId :: ActorId, gId:: GroupId) ->
          let s = emptyShared adminActor adminGroup
              go :: State Shared Bool
              go = do
                setup adminActor adminGroup aId gId
                worked <- storeSpace adminActor sId
                unless worked $ error $ "Couldn't make space " <> show sId
                worked <- setUniversePermission adminActor
                  (CollectionPermissionWithExemption Read False) gId
                unless worked $ error $ "Couldn't set universe permission " <> show gId
                canReadSpace aId sId
              s' = execState go s
          in  evalState go s `shouldBe` True
    it "Entities" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, sId :: SpaceId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeSpace adminActor sId
              unless worked $ error $ "Couldn't make space " <> show sId
              worked <- setUniversePermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set universe permission " <> show gId
              worked <- setEntityPermission adminActor Read gId sId
              unless worked $ error $ "Couldn't set entity permission " <> show gId
              canReadAllEntities aId sId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == True
    it "Entity" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, sId :: SpaceId, aId :: ActorId, gId:: GroupId, eId :: EntityId, vId :: VersionId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeSpace adminActor sId
              unless worked $ error $ "Couldn't make space " <> show sId
              worked <- setEntityPermission adminActor Create adminGroup sId
              unless worked $ error $ "Couldn't grant entity permissions " <> show sId
              mWorked <- storeEntity adminActor eId sId vId Nothing
              case mWorked of
                Just (Right ()) -> pure ()
                _ -> error $ "Couldn't store entity " <> show (eId, vId)
              worked <- setUniversePermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set universe permission " <> show gId
              worked <- setEntityPermission adminActor Read gId sId
              unless worked $ error $ "Couldn't set entity permission " <> show gId
              canReadEntity aId eId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == True
    it "Version" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, sId :: SpaceId, aId :: ActorId, gId:: GroupId, eId :: EntityId, vId :: VersionId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeSpace adminActor sId
              unless worked $ error $ "Couldn't make space " <> show sId
              worked <- setEntityPermission adminActor Create adminGroup sId
              unless worked $ error $ "Couldn't grant entity permissions " <> show sId
              mWorked <- storeEntity adminActor eId sId vId Nothing
              case mWorked of
                Just (Right ()) -> pure ()
                _ -> error $ "Couldn't store entity " <> show (eId, vId)
              worked <- setUniversePermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set universe permission " <> show gId
              worked <- setEntityPermission adminActor Read gId sId
              unless worked $ error $ "Couldn't set entity permission " <> show gId
              canReadVersion aId vId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == True
    it "Group" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, gId' :: GroupId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeGroup adminActor gId'
              unless worked $ error $ "Couldn't make group " <> show gId'
              worked <- setOrganizationPermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set organization permission " <> show gId
              canReadGroup aId gId'
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == True
    it "Member" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, gId' :: GroupId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeGroup adminActor gId'
              unless worked $ error $ "Couldn't make group " <> show gId'
              worked <- setOrganizationPermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set organization permission " <> show gId
              worked <- setMemberPermission adminActor Read gId gId'
              unless worked $ error $ "Couldn't set member permission " <> show gId
              canReadMember aId gId'
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == True
    it "Actor" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- setRecruiterPermission adminActor Read gId
              unless worked $ error $ "Couldn't set recruiter permission " <> show gId
              canReadActor aId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId) $ \_ -> evalState go s == True
  describe "Should Fail" $ do
    it "Spaces when explicit via universe" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, sId :: SpaceId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeSpace adminActor sId
              unless worked $ error $ "Couldn't make space " <> show sId
              worked <- setUniversePermission adminActor
                (CollectionPermissionWithExemption Blind False) gId
              unless worked $ error $ "Couldn't set universe permission " <> show gId
              canReadSpace aId sId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Spaces when explicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, sId :: SpaceId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeSpace adminActor sId
              unless worked $ error $ "Couldn't make space " <> show sId
              worked <- setUniversePermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set universe permission " <> show gId
              worked <- setSpacePermission adminActor (Just NonExistent) gId sId
              unless worked $ error $ "Couldn't set spaces permission " <> show (gId, sId)
              canReadSpace aId sId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Spaces when implicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, sId :: SpaceId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeSpace adminActor sId
              unless worked $ error $ "Couldn't make space " <> show sId
              canReadSpace aId sId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Entities when explicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, sId :: SpaceId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeSpace adminActor sId
              unless worked $ error $ "Couldn't make space " <> show sId
              worked <- setUniversePermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set universe permission " <> show gId
              worked <- setEntityPermission adminActor Blind gId sId
              unless worked $ error $ "Couldn't set entity permission " <> show gId
              canReadAllEntities aId sId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Entity when explicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, sId :: SpaceId, aId :: ActorId, gId:: GroupId, eId :: EntityId, vId :: VersionId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeSpace adminActor sId
              unless worked $ error $ "Couldn't make space " <> show sId
              worked <- setEntityPermission adminActor Create adminGroup sId
              unless worked $ error $ "Couldn't grant entity permissions " <> show sId
              mWorked <- storeEntity adminActor eId sId vId Nothing
              case mWorked of
                Just (Right ()) -> pure ()
                _ -> error $ "Couldn't make entity " <> show (eId, vId)
              worked <- setUniversePermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set universe permission " <> show gId
              worked <- setEntityPermission adminActor Blind gId sId
              unless worked $ error $ "Couldn't set entity permission " <> show gId
              canReadEntity aId eId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Entities when implicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, sId :: SpaceId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeSpace adminActor sId
              unless worked $ error $ "Couldn't make space " <> show sId
              worked <- setUniversePermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set universe permission " <> show gId
              canReadAllEntities aId sId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Entity when implicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, sId :: SpaceId, aId :: ActorId, gId:: GroupId, eId :: EntityId, vId :: VersionId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeSpace adminActor sId
              unless worked $ error $ "Couldn't make space " <> show sId
              worked <- setEntityPermission adminActor Create adminGroup sId
              unless worked $ error $ "Couldn't grant entity permissions " <> show sId
              mWorked <- storeEntity adminActor eId sId vId Nothing
              case mWorked of
                Just (Right ()) -> pure ()
                _ -> error $ "Couldn't make entity " <> show (eId, vId)
              worked <- setUniversePermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set universe permission " <> show gId
              canReadEntity aId eId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Group when explicit via universe" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, gId' :: GroupId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeGroup adminActor gId'
              unless worked $ error $ "Couldn't make group " <> show gId'
              worked <- setOrganizationPermission adminActor
                (CollectionPermissionWithExemption Blind False) gId
              unless worked $ error $ "Couldn't set organization permission " <> show gId
              canReadGroup aId gId'
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == False
    it "Group when explicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, gId' :: GroupId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeGroup adminActor gId'
              unless worked $ error $ "Couldn't make group " <> show gId'
              worked <- setOrganizationPermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set organization permission " <> show gId
              worked <- setGroupPermission adminActor (Just NonExistent) gId gId'
              unless worked $ error $ "Couldn't set group permission " <> show (gId, gId')
              canReadGroup aId gId'
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == False
    it "Group when implicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, gId' :: GroupId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeGroup adminActor gId'
              unless worked $ error $ "Couldn't make group " <> show gId'
              canReadGroup aId gId'
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == False
    it "Member when explicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, gId' :: GroupId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeGroup adminActor gId'
              unless worked $ error $ "Couldn't make group " <> show gId'
              worked <- setOrganizationPermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set organization permission " <> show gId
              worked <- setMemberPermission adminActor Blind gId gId'
              unless worked $ error $ "Couldn't set member permission " <> show gId
              canReadMember aId gId'
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == False
    it "Member when implicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, gId' :: GroupId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- storeGroup adminActor gId'
              unless worked $ error $ "Couldn't make group " <> show gId'
              worked <- setOrganizationPermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set organization permission " <> show gId
              canReadMember aId gId'
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, gId') $ \_ -> evalState go s == False
    it "Actor when explicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              worked <- setRecruiterPermission adminActor Blind gId
              unless worked $ error $ "Couldn't set recruiter permission " <> show gId
              canReadActor aId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId) $ \_ -> evalState go s == False
    it "Actor when implicit" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId:: GroupId) ->
        let s = emptyShared adminActor adminGroup
            go :: State Shared Bool
            go = do
              setup adminActor adminGroup aId gId
              canReadActor aId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId) $ \_ -> evalState go s == False
  where
    setup adminActor adminGroup aId gId = do
      worked <- storeActor adminActor aId
      unless worked $ error $ "Couldn't make actor " <> show aId
      worked <- storeGroup adminActor gId
      unless worked $ error $ "Couldn't make group " <> show gId
      worked <- setMemberPermission adminActor Create adminGroup gId
      unless worked $ error $ "Couldn't set group permission " <> show gId
      worked <- addMember adminActor gId aId
      unless worked $ error $ "Couldn't add member " <> show (gId, aId)

-- FIXME each group or space or something should have a set of shit it specifically _can't_ see?
-- i.e., the shit it's blind to? Er, NonExists within the set, assuming read rights are present?

-- TODO test to see if setting SinglePermissions also causes the same effect - and combinations
-- of whether or not entities are granted, but space isn't, etc.
