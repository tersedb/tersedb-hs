{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , RecordWildCards
  , FlexibleInstances
  , NamedFieldPuns
  #-}

import Spec.Sample.Tree (SampleGroupTree (..))
import Spec.Sample.Store
  ( SampleStore (..)
  , loadSample
  , storeSample
  )
import Spec.Test.Simple (simpleTests)
import Spec.Test.Groups (groupsTests, testPermissionInheritance)

import Lib.Types.Id (GroupId, ActorId, SpaceId)
import Lib.Types.Permission
  ( CollectionPermission (..)
  , CollectionPermissionWithExemption (..)
  , SinglePermission (Adjust)
  )
import Lib.Types.Store
  ( store
  , temp
  , toGroups
  , toSpaces
  , toEntities
  , toSpaces
  )
import Lib.Types.Store.Groups (nodes)
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
  )
import Lib.Actions.Safe.Update.Group
  ( setUniversePermission
  , setMemberPermission
  )
import Lib.Actions.Tabulation (resetTabulation, tempFromStore)

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Control.Monad.Extra (unless)
import Control.Monad.State (execState, get)
import Control.Lens ((^.), at)
import Test.Syd (sydTest, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck
  ( property
  , forAll
  , elements
  )


main :: IO ()
main = sydTest $ do
  describe "Simple" simpleTests
  describe "Groups" groupsTests
  describe "Store" $ do
    it "temp data is reproducible from store" $
      property $ \(xs :: SampleStore) ->
        let s = loadSample xs
            sTemp = s ^. temp
            sFrom = tempFromStore (s ^. store)
        in  shouldSatisfy (s ^. store, sTemp, sFrom) $ \_ -> sTemp == sFrom
    it "all descendants are supersets of roots - build all permissions" $
      property $ \(xs :: SampleStore) ->
        let groups = sampleGroups xs
        in  testPermissionInheritance (current groups) (children groups) (loadSample xs)
    it "all spaces are disjoint" $
      property $ \(xs :: SampleStore) ->
        let s = loadSample xs
        in  foldr HS.intersection mempty (fmap (^. entities) (s ^. store . toSpaces))
              `shouldBe` mempty
    it "all elements exist in their space" $
      property $ \(xs :: SampleStore) ->
        let s = loadSample xs
        in  if null (s ^. store . toEntities)
            then property True
            else forAll (elements . HM.toList $ s ^. store . toEntities) $ \(eId, e) ->
                  (s, eId, HM.lookup (e ^. space) (s ^. store . toSpaces)) `shouldSatisfy` (\(_,_,mSpace) ->
                    maybe False (\space -> HS.member eId (space ^. entities)) mSpace
                  )
    describe "Safe" $ do
      it "should be identical to unsafe" $
        property $ \(xs :: SampleStore, adminActor :: ActorId, adminGroup :: GroupId) ->
          let safeStore = storeSample xs adminActor adminGroup
              unsafeStore = flip execState (loadSample xs) $ do
                -- setup admin
                unsafeStoreGroup adminGroup
                unsafeAdjustUniversePermission
                  (const $ CollectionPermissionWithExemption Delete True)
                  adminGroup
                unsafeAdjustOrganizationPermission
                  (const $ CollectionPermissionWithExemption Delete True)
                  adminGroup
                unsafeAdjustRecruiterPermission (const Delete) adminGroup
                unsafeStoreActor adminActor
                unsafeAddMember adminGroup adminActor
                -- "backdate" the granting of group adjust rights to admin group
                s <- get
                for_ (HM.keys $ s ^. store . toGroups . nodes) $ \gId ->
                  unless (gId == adminGroup) $ do
                    unsafeAdjustGroupPermission (const (Just Adjust)) adminGroup gId
                    unsafeAdjustMemberPermission (const Create) adminGroup gId
                -- "backdate" the granting of space create rights to admin group
                s <- get
                for_ (HM.keys $ s ^. store . toSpaces) $ \sId ->
                  unsafeAdjustEntityPermission (const Update) adminGroup sId
                resetTabulation
          in  safeStore `shouldBe` unsafeStore
      describe "Permissions" $ do
        describe "Create" $ do
          it "create a space via universe" $
            property $ \(adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId) ->
              let s = emptyShared adminActor adminGroup
                  s' = flip execState s $ do
                    worked <- storeActor adminActor aId
                    unless worked $ error $ "Couldn't create actor " <> show aId
                    worked <- storeGroup adminActor gId
                    unless worked $ error $ "Couldn't create group " <> show gId
                    worked <- setMemberPermission adminActor Create adminGroup gId
                    unless worked $ error $ "Couldn't grant membership creation to admin group " <> show gId
                    worked <- addMember adminActor gId aId
                    unless worked $ error $ "Couldn't add member " <> show (gId, aId)
                    worked <- setUniversePermission adminActor (CollectionPermissionWithExemption Create False) gId
                    unless worked $ error $ "Couldn't grant universe create permissions " <> show gId
                    worked <- storeSpace aId sId
                    unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
              in  shouldSatisfy s' $ \_ -> isJust $ s' ^. store . toSpaces . at sId
          -- it "create an entity" $
            -- property $ \(xs :: SampleStore, adminActor :: ActorId, adminGroup :: GroupId, aId :: ActorId, gId :: GroupId, sId :: SpaceId, eId :: EntityId) ->
              -- let s = storeSample xs adminActor adminGroup
                  -- (s', mE) = flip runState s $ do
                    -- worked <- storeActor adminActor aId
                    -- unless worked $ error $ "Couldn't create actor " <> show aId
              -- in

