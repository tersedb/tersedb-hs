module Spec.Test.Safe.Update where

import Spec.Sample.Store (SampleStore, storeSample)
import Lib.Types.Id (GroupId, ActorId, SpaceId, EntityId, VersionId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption (..)
  )
import Lib.Types.Store
  ( Shared
  , store
  , temp
  , toSpaces
  , toEntities
  , toSpaces
  , toGroups
  , toTabulatedGroups
  )
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Entity (space)
import Lib.Types.Store.Groups (next, prev, nodes)
import Lib.Types.Store.Tabulation.Group (hasLessOrEqualPermissionsTo)
import Lib.Actions.Safe (emptyShared)
import Lib.Actions.Safe.Store (storeActor, storeSpace, storeGroup, addMember, storeEntity)
import Lib.Actions.Safe.Update
  ( updateEntitySpace
  )
import Lib.Actions.Safe.Update.Group
  ( setUniversePermission
  , setMemberPermission
  , setEntityPermission
  , updateGroupParent
  , unlinkGroups
  , linkGroups
  )

import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.HashMap.Strict as HM
import Control.Monad.Extra (unless)
import Control.Monad.State (MonadState, execState, evalState)
import Control.Lens ((^.), (^?), at, non, ix, _Just)
import Test.Syd (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck
  ( property
  , elements
  , forAll
  )


updateTests :: Spec
updateTests = describe "Update" $ do
  describe "Space" $
    it "doesn't apply" True
  describe "Entity" $ do
    describe "Should Succeed" $ do
      it "Move An Entity to different Space" $
        property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, sId' :: SpaceId, eId :: EntityId, vId :: VersionId) ->
          let s = emptyShared adminActor adminGroup
              s' = flip execState s $ do
                setStage adminActor adminGroup aId gId
                worked <- setUniversePermission adminActor
                  (CollectionPermissionWithExemption Read False) gId
                unless worked $ error $ "Couldn't grant read universe permission " <> show aId
                worked <- storeSpace adminActor sId
                unless worked $ error $ "Couldn't store space " <> show sId
                worked <- storeSpace adminActor sId'
                unless worked $ error $ "Couldn't store space " <> show sId'
                worked <- setEntityPermission adminActor Create adminGroup sId
                unless worked $ error $ "Couldn't set entity permission " <> show sId
                worked <- storeEntity adminActor eId sId vId
                unless worked $ error $ "Couldn't store entity " <> show (eId, vId)
                worked <- setEntityPermission adminActor Create gId sId'
                unless worked $ error $ "Couldn't set entity permission " <> show sId'
                worked <- setEntityPermission adminActor Delete gId sId
                unless worked $ error $ "Couldn't set entity permission " <> show sId
                worked <- updateEntitySpace aId eId sId'
                unless worked $ error $ "Couldn't move entity " <> show eId
          in  shouldSatisfy s' $ \_ ->
                isNothing (s' ^. store . toSpaces . at sId . non mempty . entities . at eId)
                && isJust (s' ^. store . toSpaces . at sId' . non mempty . entities . at eId)
                && (s' ^? store . toEntities . ix eId . space) == Just sId'
    describe "Should Fail" $ do
      it "Move An Entity to different Space without delete access" $
        property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, sId' :: SpaceId, eId :: EntityId, vId :: VersionId) ->
          let s = emptyShared adminActor adminGroup
              go = do
                setStage adminActor adminGroup aId gId
                worked <- setUniversePermission adminActor
                  (CollectionPermissionWithExemption Read False) gId
                unless worked $ error $ "Couldn't grant read universe permission " <> show aId
                worked <- storeSpace adminActor sId
                unless worked $ error $ "Couldn't store space " <> show sId
                worked <- storeSpace adminActor sId'
                unless worked $ error $ "Couldn't store space " <> show sId'
                worked <- setEntityPermission adminActor Create adminGroup sId
                unless worked $ error $ "Couldn't set entity permission " <> show sId
                worked <- storeEntity adminActor eId sId vId
                unless worked $ error $ "Couldn't store entity " <> show (eId, vId)
                worked <- setEntityPermission adminActor Create gId sId'
                unless worked $ error $ "Couldn't set entity permission " <> show sId'
                updateEntitySpace aId eId sId'
              s' = execState go s
          in  shouldSatisfy s' $ \_ -> evalState go s == False
                && isJust (s' ^. store . toSpaces . at sId . non mempty . entities . at eId)
                && isNothing (s' ^. store . toSpaces . at sId' . non mempty . entities . at eId)
                && (s' ^? store . toEntities . ix eId . space) == Just sId
      it "Move An Entity to different Space without create access" $
        property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, sId' :: SpaceId, eId :: EntityId, vId :: VersionId) ->
          let s = emptyShared adminActor adminGroup
              go = do
                setStage adminActor adminGroup aId gId
                worked <- setUniversePermission adminActor
                  (CollectionPermissionWithExemption Read False) gId
                unless worked $ error $ "Couldn't grant read universe permission " <> show aId
                worked <- storeSpace adminActor sId
                unless worked $ error $ "Couldn't store space " <> show sId
                worked <- storeSpace adminActor sId'
                unless worked $ error $ "Couldn't store space " <> show sId'
                worked <- setEntityPermission adminActor Create adminGroup sId
                unless worked $ error $ "Couldn't set entity permission " <> show sId
                worked <- storeEntity adminActor eId sId vId
                unless worked $ error $ "Couldn't store entity " <> show (eId, vId)
                worked <- setEntityPermission adminActor Delete gId sId
                unless worked $ error $ "Couldn't set entity permission " <> show sId
                updateEntitySpace aId eId sId'
              s' = execState go s
          in  shouldSatisfy s' $ \_ -> evalState go s == False
                && isJust (s' ^. store . toSpaces . at sId . non mempty . entities . at eId)
                && isNothing (s' ^. store . toSpaces . at sId' . non mempty . entities . at eId)
                && (s' ^? store . toEntities . ix eId . space) == Just sId
    -- updating an entity occurs when you store a version or modify the set of an entity's versions
    -- or changing what space it belongs to (requires create/entity rights on target space)
  describe "Version" $ do
    -- updating a version occurs when you modify an existing one; still subject to modifying the
    -- entity by extension. Modifying a version - changing its references / subscriptions,
    -- changing what it forks from
    pure ()
  describe "Group" $ do
    -- updating what it inherits from / who it inherits to
    describe "Should Succeed" $ do
      it "deleting parent relationship should affect both nodes, and unrelate them" $
        property $ \(xs :: SampleStore, adminActor :: ActorId, adminGroup :: GroupId) ->
          let s = storeSample xs adminActor adminGroup
              gsWithParent = HM.filter (\g -> isJust (g ^. prev)) $ s ^. store . toGroups . nodes
          in  if null gsWithParent then property True else
              forAll (elements (HM.toList gsWithParent)) $ \(gId :: GroupId, g) ->
                let s' = flip execState s $ do
                      mE <- updateGroupParent adminActor gId Nothing
                      case mE of
                        Just (Right ()) -> pure ()
                        _ -> error $ "Couldn't set group parent " <> show (gId, mE)
                    parentId = fromJust $ g ^. prev
                in  shouldSatisfy (s', gId, parentId) $ \_ ->
                      (s' ^? store . toGroups . nodes . ix gId . prev . _Just) == Nothing
                      && (s' ^? store . toGroups . nodes . ix parentId . next . ix gId) == Nothing
      it "deleting child relationship should affect both nodes, and unrelate them" $
        property $ \(xs :: SampleStore, adminActor :: ActorId, adminGroup :: GroupId) ->
          let s = storeSample xs adminActor adminGroup
              gsWithParent = HM.filter (\g -> isJust (g ^. prev)) $ s ^. store . toGroups . nodes
          in  if null gsWithParent then property True else
              forAll (elements (HM.toList gsWithParent)) $ \(gId :: GroupId, g) ->
                let s' = flip execState s $ do
                      worked <- unlinkGroups adminActor parentId gId
                      unless worked $ error $ "Couldn't unlink groups " <> show (parentId, gId)
                    parentId = fromJust $ g ^. prev
                in  (s' ^? store . toGroups . nodes . ix gId . prev . _Just) == Nothing
                      && (s' ^? store . toGroups . nodes . ix parentId . next . ix gId) == Nothing
      it "adding child relationship should cause inheritance" $
        property $ \(xs :: SampleStore, adminActor :: ActorId, adminGroup :: GroupId, newGId :: GroupId) ->
          let s = storeSample xs adminActor adminGroup
              gsWithParent = HM.filter (\g -> isJust (g ^. prev)) $ s ^. store . toGroups . nodes
          in  if null gsWithParent then property True else
              forAll (elements (HM.toList gsWithParent)) $ \(gId :: GroupId, _) ->
                let s' = flip execState s $ do
                      worked <- storeGroup adminActor newGId
                      unless worked $ error $ "Couldn't store group " <> show newGId
                      mE <- linkGroups adminActor gId newGId
                      case mE of
                        Just (Right ()) -> pure ()
                        _ -> error $ "Couldn't set child " <> show (gId, newGId, mE)
                in  shouldSatisfy (s', gId, newGId) $ \_ ->
                      fromJust (s' ^? temp . toTabulatedGroups . ix gId)
                        `hasLessOrEqualPermissionsTo`
                          fromJust (s' ^? temp . toTabulatedGroups . ix newGId)
  describe "Member" $
    it "doesn't apply" True
  describe "Actor" $
    it "doesn't apply" True


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
