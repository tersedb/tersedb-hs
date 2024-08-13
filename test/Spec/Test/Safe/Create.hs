module Spec.Test.Safe.Create where

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
import Lib.Actions.Unsafe.Store
  ( unsafeStoreGroup
  , unsafeStoreActor
  , unsafeAddMember
  )
import Lib.Actions.Unsafe.Update.Group
  ( unsafeAdjustUniversePermission
  , unsafeAdjustOrganizationPermission
  , unsafeAdjustRecruiterPermission
  , unsafeAdjustGroupPermission
  , unsafeAdjustEntityPermission
  , unsafeAdjustMemberPermission
  )
import Lib.Actions.Safe (emptyShared)
import Lib.Actions.Safe.Store
  ( storeActor
  , storeGroup
  , addMember
  , storeSpace
  , storeEntity
  , storeVersion
  )
import Lib.Actions.Safe.Update.Group
  ( setUniversePermission
  , setOrganizationPermission
  , setRecruiterPermission
  , setGroupPermission
  , setMemberPermission
  , setEntityPermission
  )
import Lib.Actions.Tabulation (resetTabulation, tempFromStore)

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Foldable (for_)
import Data.Maybe (isJust, isNothing)
import Control.Monad.Extra (unless, when)
import Control.Monad.State (MonadState, execState, get)
import Control.Lens ((^.), at, non)
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck
  ( property
  , forAll
  , elements
  )

createTests :: Spec
createTests = describe "Create" $ do
  describe "Should Succeed" $ do
    it "create a space via universe" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId sId
              worked <- setUniversePermission adminActor (CollectionPermissionWithExemption Create False) gId
              unless worked $ error $ "Couldn't grant universe create permissions " <> show gId
              worked <- storeSpace aId sId
              unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
        in  shouldSatisfy s' $ \_ -> isJust $ s' ^. store . toSpaces . at sId
    it "create an entity" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, eId :: EntityId, vId :: VersionId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId sId
              worked <- setUniversePermission adminActor (CollectionPermissionWithExemption Create False) gId
              unless worked $ error $ "Couldn't grant universe create permissions " <> show gId
              worked <- storeSpace aId sId
              unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
              worked <- setEntityPermission adminActor Create gId sId
              unless worked $ error $ "Couldn't set entity permission " <> show (gId, sId)
              worked <- storeEntity aId eId sId vId
              unless worked $ error $ "Couldn't store entity " <> show (eId, vId)
        in  shouldSatisfy s' $ \_ ->
              isJust (s' ^. store . toEntities . at eId)
                && isJust (s' ^. store . toVersions . at vId)
    it "append a version" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, eId :: EntityId, vId :: VersionId, vId' :: VersionId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId sId
              worked <- setUniversePermission adminActor (CollectionPermissionWithExemption Create False) gId
              unless worked $ error $ "Couldn't grant universe create permissions " <> show gId
              worked <- storeSpace aId sId
              unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
              worked <- setEntityPermission adminActor Update gId sId
              unless worked $ error $ "Couldn't set entity permission " <> show (gId, sId)
              worked <- storeEntity aId eId sId vId
              unless worked $ error $ "Couldn't store entity " <> show (eId, vId)
              mWorked <- storeVersion aId eId vId'
              case mWorked of
                Just (Right ()) -> pure ()
                _ -> error $ "Couldn't store version " <> show mWorked
        in  shouldSatisfy s' $ \_ ->
              isJust (s' ^. store . toVersions . at vId')
    it "create a group via organization" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, gId' :: GroupId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId sId
              worked <- setOrganizationPermission adminActor (CollectionPermissionWithExemption Create False) gId
              unless worked $ error $ "Couldn't set organization permission " <> show gId
              worked <- storeGroup aId gId'
              unless worked $ error $ "Couldn't store group " <> show gId'
        in  shouldSatisfy s' $ \_ ->
              isJust (s' ^. store . toGroups . nodes . at gId')
    it "create a actor via recruiter" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, aId' :: ActorId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId sId
              worked <- setRecruiterPermission adminActor Create gId
              unless worked $ error $ "Couldn't set recruiter permission " <> show gId
              worked <- storeActor aId aId'
              unless worked $ error $ "Couldn't store actor " <> show aId'
        in  shouldSatisfy s' $ \_ ->
              isJust (s' ^. store . toActors . at aId')
    it "add a member" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, aId' :: ActorId, gId') ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId sId
              worked <- storeActor adminActor aId'
              unless worked $ error $ "Couldn't store actor " <> show aId'
              worked <- storeGroup adminActor gId'
              unless worked $ error $ "Couldn't store group " <> show gId'
              worked <- addMember adminActor gId aId
              unless worked $ error $ "Couldn't add member " <> show (gId, aId)
              worked <- setMemberPermission adminActor Create gId gId'
              unless worked $ error $ "Couldn't grant member permission " <> show (gId, gId')
              worked <- setOrganizationPermission adminActor (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't grant read organization permission " <> show aId
              worked <- addMember aId gId' aId'
              unless worked $ error $ "Couldn't add member " <> show (gId', aId')
        in  shouldSatisfy s' $ \_ ->
              isJust (s' ^. store . toGroups . nodes . at gId' . non emptyGroup . members . at aId')
  describe "Should Fail" $ do
    it "create a space via universe" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId sId
              worked <- storeSpace aId sId
              when worked $ error $ "Could store space " <> show (aId, gId, sId)
        in  shouldSatisfy s' $ \_ -> isNothing $ s' ^. store . toSpaces . at sId
    it "create an entity" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, eId :: EntityId, vId :: VersionId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId sId
              worked <- setUniversePermission adminActor (CollectionPermissionWithExemption Create False) gId
              unless worked $ error $ "Couldn't grant universe create permissions " <> show gId
              worked <- storeSpace aId sId
              unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
              worked <- storeEntity aId eId sId vId
              when worked $ error $ "Could store entity " <> show (eId, vId)
        in  shouldSatisfy s' $ \_ ->
              isNothing (s' ^. store . toEntities . at eId)
                && isNothing (s' ^. store . toVersions . at vId)
    it "append a version" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, eId :: EntityId, vId :: VersionId, vId' :: VersionId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId sId
              worked <- setUniversePermission adminActor (CollectionPermissionWithExemption Create False) gId
              unless worked $ error $ "Couldn't grant universe create permissions " <> show gId
              worked <- storeSpace aId sId
              unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
              worked <- setEntityPermission adminActor Update gId sId
              unless worked $ error $ "Couldn't set entity permission " <> show (gId, sId)
              worked <- storeEntity aId eId sId vId
              unless worked $ error $ "Couldn't store entity " <> show (eId, vId)
              worked <- setEntityPermission adminActor Read gId sId
              unless worked $ error $ "Couldn't set entity permission " <> show (gId, sId)
              mWorked <- storeVersion aId eId vId'
              case mWorked of
                Just (Right ()) -> error $ "Could store version " <> show mWorked
                _ -> pure ()
        in  shouldSatisfy s' $ \_ ->
              isJust (s' ^. store . toEntities . at eId)
                && isNothing (s' ^. store . toVersions . at vId')
    it "create a group via organization" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, gId' :: GroupId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId sId
              worked <- storeGroup aId gId'
              when worked $ error $ "Could store group " <> show gId'
        in  shouldSatisfy s' $ \_ ->
              isNothing (s' ^. store . toGroups . nodes . at gId')
    it "create a actor via recruiter" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, aId' :: ActorId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId sId
              worked <- storeActor aId aId'
              when worked $ error $ "Could store actor " <> show aId'
        in  shouldSatisfy s' $ \_ ->
              isNothing (s' ^. store . toActors . at aId')
    it "add a member" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, aId' :: ActorId, gId') ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId sId
              worked <- storeActor adminActor aId'
              unless worked $ error $ "Couldn't store actor " <> show aId'
              worked <- storeGroup adminActor gId'
              unless worked $ error $ "Couldn't store group " <> show gId'
              worked <- addMember adminActor gId aId
              unless worked $ error $ "Couldn't add member " <> show (gId, aId)
              worked <- setOrganizationPermission adminActor (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't grant read organization permission " <> show aId
              worked <- addMember aId gId' aId'
              when worked $ error $ "Could add member " <> show (gId', aId')
        in  shouldSatisfy s' $ \_ ->
              isNothing (s' ^. store . toGroups . nodes . at gId' . non emptyGroup . members . at aId')


setStage :: MonadState Shared m => ActorId -> GroupId -> ActorId -> GroupId -> SpaceId -> m ()
setStage adminActor adminGroup aId gId sId = do
  worked <- storeActor adminActor aId
  unless worked $ error $ "Couldn't create actor " <> show aId
  worked <- storeGroup adminActor gId
  unless worked $ error $ "Couldn't create group " <> show gId
  worked <- setMemberPermission adminActor Create adminGroup gId
  unless worked $ error $ "Couldn't grant membership creation to admin group " <> show gId
  worked <- addMember adminActor gId aId
  unless worked $ error $ "Couldn't add member " <> show (gId, aId)
