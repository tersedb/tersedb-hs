module Spec.Test.Safe.Create where

import Spec.Sample.Store (arbitraryShared, arbitraryEmptyShared)
import Lib.Types.Id (GroupId, ActorId, SpaceId, EntityId, VersionId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption (..)
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
  , toReferencesFrom
  )
import Lib.Types.Store.Groups (emptyGroup, nodes, members)
import Lib.Actions.Safe (emptyShared)
import Lib.Actions.Safe.Store
  ( storeActor
  , storeGroup
  , addMember
  , storeSpace
  , storeEntity
  , storeNextVersion
  )
import Lib.Actions.Safe.Update.Group
  ( setUniversePermission
  , setOrganizationPermission
  , setRecruiterPermission
  , setMemberPermission
  , setEntityPermission
  )

import Data.Maybe (isJust, isNothing)
import Control.Monad.Extra (unless, when)
import Control.Monad.State (MonadState, execState)
import Control.Lens ((^.), at, non)
import Test.Syd (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck
  ( property
  , suchThat
  , arbitrary
  , forAll
  )

createTests :: Spec
createTests = describe "Create" $ do
  describe "Should Succeed" $ do
    it "Space via universe" $
      forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
        property $ \(aId :: ActorId, gId :: GroupId, sId :: SpaceId) ->
          let s' = flip execState s $ do
                setStage adminActor adminGroup aId gId
                worked <- setUniversePermission adminActor
                  (CollectionPermissionWithExemption Create False) gId
                unless worked $ error $ "Couldn't grant universe create permissions " <> show gId
                worked <- storeSpace aId sId
                unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
          in  shouldSatisfy (s' ^. store . toSpaces . at sId) isJust
    it "Entity" $
      forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
        property $
          \( aId :: ActorId
          , gId :: GroupId
          , sId :: SpaceId
          , eId :: EntityId
          , vId :: VersionId) -> do
            let s' = flip execState s $ do
                  setStage adminActor adminGroup aId gId
                  worked <- setUniversePermission adminActor
                    (CollectionPermissionWithExemption Create False) gId
                  unless worked $ error $ "Couldn't grant universe create permissions " <> show gId
                  worked <- storeSpace aId sId
                  unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
                  worked <- setEntityPermission adminActor Create gId sId
                  unless worked $ error $ "Couldn't set entity permission " <> show (gId, sId)
                  mWorked <- storeEntity aId eId sId vId Nothing
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
          , vId' :: VersionId) ->
          let s' = flip execState s $ do
                setStage adminActor adminGroup aId gId
                worked <- setUniversePermission adminActor (CollectionPermissionWithExemption Create False) gId
                unless worked $ error $ "Couldn't grant universe create permissions " <> show gId
                worked <- storeSpace aId sId
                unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
                worked <- setEntityPermission adminActor Update gId sId
                unless worked $ error $ "Couldn't set entity permission " <> show (gId, sId)
                mWorked <- storeEntity aId eId sId vId Nothing
                case mWorked of
                  Just (Right ()) -> pure ()
                  _ -> error $ "Couldn't store entity " <> show (eId, vId)
                mWorked <- storeNextVersion aId eId vId'
                case mWorked of
                  Just (Right ()) -> pure ()
                  _ -> error $ "Couldn't store version " <> show mWorked
          in  shouldSatisfy (s' ^. store . toVersions . at vId') isJust 
    it "Group via organization" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, gId' :: GroupId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId
              worked <- setOrganizationPermission adminActor (CollectionPermissionWithExemption Create False) gId
              unless worked $ error $ "Couldn't set organization permission " <> show gId
              worked <- storeGroup aId gId'
              unless worked $ error $ "Couldn't store group " <> show gId'
        in  shouldSatisfy s' $ \_ ->
              isJust (s' ^. store . toGroups . nodes . at gId')
    it "Actor via recruiter" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, aId' :: ActorId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId
              worked <- setRecruiterPermission adminActor Create gId
              unless worked $ error $ "Couldn't set recruiter permission " <> show gId
              worked <- storeActor aId aId'
              unless worked $ error $ "Couldn't store actor " <> show aId'
        in  shouldSatisfy s' $ \_ ->
              isJust (s' ^. store . toActors . at aId')
    it "Member" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, aId' :: ActorId, gId') ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId
              worked <- storeActor adminActor aId'
              unless worked $ error $ "Couldn't store actor " <> show aId'
              worked <- storeGroup adminActor gId'
              unless worked $ error $ "Couldn't store group " <> show gId'
              worked <- setMemberPermission adminActor Create gId gId'
              unless worked $ error $ "Couldn't grant member permission " <> show (gId, gId')
              worked <- setOrganizationPermission adminActor (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't grant read organization permission " <> show aId
              worked <- addMember aId gId' aId'
              unless worked $ error $ "Couldn't add member " <> show (gId', aId')
        in  shouldSatisfy s' $ \_ ->
              isJust (s' ^. store . toGroups . nodes . at gId' . non emptyGroup . members . at aId')
  describe "Should Fail" $ do
    it "Space via universe" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId
              worked <- storeSpace aId sId
              when worked $ error $ "Could store space " <> show (aId, gId, sId)
        in  shouldSatisfy s' $ \_ -> isNothing $ s' ^. store . toSpaces . at sId
    it "Entity" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, eId :: EntityId, vId :: VersionId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId
              worked <- setUniversePermission adminActor
                (CollectionPermissionWithExemption Create False) gId
              unless worked $ error $ "Couldn't grant universe create permissions " <> show gId
              worked <- storeSpace aId sId
              unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
              mWorked <- storeEntity aId eId sId vId Nothing
              case mWorked of
                Just (Right ()) -> error $ "Couldn't store entity " <> show (eId, vId)
                _ -> pure ()
        in  shouldSatisfy s' $ \_ ->
              isNothing (s' ^. store . toEntities . at eId)
                && isNothing (s' ^. store . toVersions . at vId)
    -- FIXME test for forking
    it "Next Version" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, eId :: EntityId, vId :: VersionId, vId' :: VersionId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId
              worked <- setUniversePermission adminActor (CollectionPermissionWithExemption Create False) gId
              unless worked $ error $ "Couldn't grant universe create permissions " <> show gId
              worked <- storeSpace aId sId
              unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
              worked <- setEntityPermission adminActor Update gId sId
              unless worked $ error $ "Couldn't set entity permission " <> show (gId, sId)
              mWorked <- storeEntity aId eId sId vId Nothing
              case mWorked of
                Just (Right ()) -> pure ()
                _ -> error $ "Couldn't store entity " <> show (eId, vId)
              worked <- setEntityPermission adminActor Read gId sId
              unless worked $ error $ "Couldn't set entity permission " <> show (gId, sId)
              mWorked <- storeNextVersion aId eId vId'
              case mWorked of
                Just (Right ()) -> error $ "Could store version " <> show mWorked
                _ -> pure ()
        in  shouldSatisfy s' $ \_ ->
              isJust (s' ^. store . toEntities . at eId)
                && isNothing (s' ^. store . toVersions . at vId')
    it "Group via organization" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, gId' :: GroupId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId
              worked <- storeGroup aId gId'
              when worked $ error $ "Could store group " <> show gId'
        in  shouldSatisfy s' $ \_ ->
              isNothing (s' ^. store . toGroups . nodes . at gId')
    it "Actor via recruiter" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, aId' :: ActorId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId
              worked <- storeActor aId aId'
              when worked $ error $ "Could store actor " <> show aId'
        in  shouldSatisfy s' $ \_ ->
              isNothing (s' ^. store . toActors . at aId')
    it "Member" $
      property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, aId' :: ActorId, gId' :: GroupId) ->
        let s = emptyShared adminActor adminGroup
            s' = flip execState s $ do
              setStage adminActor adminGroup aId gId
              worked <- storeActor adminActor aId'
              unless worked $ error $ "Couldn't store actor " <> show aId'
              worked <- storeGroup adminActor gId'
              unless worked $ error $ "Couldn't store group " <> show gId'
              worked <- setOrganizationPermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't grant read organization permission " <> show aId
              worked <- addMember aId gId' aId'
              when worked $ error $ "Could add member " <> show (gId', aId')
        in  shouldSatisfy s' $ \_ ->
              isNothing (s' ^. store . toGroups . nodes . at gId' . non emptyGroup . members . at aId')


setStage :: MonadState Shared m => ActorId -> GroupId -> ActorId -> GroupId -> m ()
setStage adminActor adminGroup aId gId = do
  worked <- storeActor adminActor aId
  unless worked $ error $ "Couldn't create actor " <> show aId
  worked <- storeGroup adminActor gId
  unless worked $ error $ "Couldn't create group " <> show gId
  worked <- setMemberPermission adminActor Create adminGroup gId
  unless worked $ error $ "Couldn't grant membership creation to admin group " <> show gId
  worked <- addMember adminActor gId aId
  unless worked $ error $ "Couldn't add member " <> show (gId, aId)
