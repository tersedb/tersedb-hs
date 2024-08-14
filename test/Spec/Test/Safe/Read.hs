module Spec.Test.Safe.Read where

import Spec.Sample.Tree (SampleGroupTree (..))
import Spec.Sample.Store
  ( SampleStore (..)
  , loadSample
  , storeSample
  )
import Spec.Test.Simple (simpleTests)
import Spec.Test.Groups (groupsTests, testPermissionInheritance)

import Lib.Types.Id (GroupId, ActorId, SpaceId, EntityId, VersionId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption (..)
  , SinglePermission (Adjust)
  )
import Lib.Types.Store
  ( Shared
  , store
  , temp
  , toGroups
  , toSpaces
  , toEntities
  , toSpaces
  , toVersions
  , toActors
  )
import Lib.Types.Store.Groups (emptyGroup, nodes, members)
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Entity (space)
import Lib.Actions.Safe (emptyShared)
import Lib.Actions.Safe.Store (storeActor, storeSpace, storeGroup, addMember)
import Lib.Actions.Safe.Update.Group
  ( setSpacePermission
  , setUniversePermission
  , setOrganizationPermission
  , setRecruiterPermission
  , setGroupPermission
  , setMemberPermission
  , setEntityPermission
  )
import Lib.Actions.Safe.Verify.SpaceAndEntity (canReadSpace, canReadSpaceOld, canReadEntity)
import Lib.Actions.Safe.Verify.Group (canReadGroup)
import Lib.Actions.Safe.Verify.Member (canReadMember)
import Lib.Actions.Safe.Verify.Actor (canReadActor)
import Lib.Actions.Tabulation (resetTabulation, tempFromStore)

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Foldable (for_)
import Data.Maybe (isJust, isNothing)
import Control.Monad.Extra (unless, when)
import Control.Monad.State (MonadState, execState, get, evalState, State)
import Control.Lens ((^.), at, non)
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck
  ( property
  , forAll
  , elements
  )

readTests :: Spec
readTests = describe "Read" $ do
  describe "New vs. Old" $ do
    it "Spaces" $
      property $ \(xs :: SampleStore) ->
        let s = loadSample xs
        in  if null (s ^. store . toActors) || null (s ^. store . toSpaces) then property True else
            let genAId = elements . HM.keys $ s ^. store . toActors
                genSId = elements . HM.keys $ s ^. store . toSpaces
            in  forAll ((,) <$> genAId <*> genSId) $ \(aId, sId) ->
                  let resNew = evalState (canReadSpace aId sId) s
                      resOld = evalState (canReadSpaceOld aId sId) s
                  in  shouldSatisfy (s, aId, sId, resNew, resOld) $ \_ -> resNew == resOld
  describe "Should Succeed" $ do
    it "Spaces" $
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
              canReadSpace aId sId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == True
    it "Entity" $
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
              canReadEntity aId sId
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
              -- worked <- setEntityPermission adminActor Read gId sId
              -- unless worked $ error $ "Couldn't set entity permission " <> show gId
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
    it "Spaces when explicit" $
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
    it "Entity when explicit" $
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
              canReadEntity aId sId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Entity when implicit" $
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
              canReadEntity aId sId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == False
    it "Group when explicit" $
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
