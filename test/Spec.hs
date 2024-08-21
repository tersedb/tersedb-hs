{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens ((^.))
import Control.Monad.Extra (unless)
import Control.Monad.State (execState, get)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Lib.Actions.Tabulation (resetTabulation, tempFromStore)
import Lib.Actions.Unsafe.Store (
    unsafeAddMember,
    unsafeStoreActor,
    unsafeStoreGroup,
 )
import Lib.Actions.Unsafe.Update.Group (
    unsafeAdjustEntityPermission,
    unsafeAdjustGroupPermission,
    unsafeAdjustMemberPermission,
    unsafeAdjustOrganizationPermission,
    unsafeAdjustRecruiterPermission,
    unsafeAdjustUniversePermission,
 )
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (
    CollectionPermission (..),
    CollectionPermissionWithExemption (..),
    SinglePermission (Adjust),
 )
import Lib.Types.Store (
    store,
    temp,
    toEntities,
    toGroups,
    toSpaces,
 )
import Lib.Types.Store.Entity (space)
import Lib.Types.Store.Groups (nodes)
import Lib.Types.Store.Space (entities)
import Spec.Sample.Store (
    SampleStore (..),
    loadSample,
    storeSample,
 )
import Spec.Sample.Tree (SampleGroupTree (..))
import Spec.Test.Groups (groupsTests, testPermissionInheritance)
import Spec.Test.Safe.Create (createTests)
import Spec.Test.Safe.Delete (removeTests)
import Spec.Test.Safe.Read (readTests)
import Spec.Test.Safe.Update (updateTests)
import Spec.Test.Simple (simpleTests)
import Test.QuickCheck (
    elements,
    forAll,
    property,
 )
import Test.Syd (describe, it, shouldBe, shouldSatisfy, sydTest)

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
                 in shouldSatisfy (s ^. store, sTemp, sFrom) $ \_ -> sTemp == sFrom
        it "all descendants are supersets of roots - build all permissions" $
            property $ \(xs :: SampleStore) ->
                let groups = sampleGroups xs
                 in testPermissionInheritance (current groups) (children groups) (loadSample xs)
        it "all spaces are disjoint" $
            property $ \(xs :: SampleStore) ->
                let s = loadSample xs
                 in foldr HS.intersection mempty (fmap (^. entities) (s ^. store . toSpaces))
                        `shouldBe` mempty
        it "all elements exist in their space" $
            property $ \(xs :: SampleStore) ->
                let s = loadSample xs
                 in if null (s ^. store . toEntities)
                        then property True
                        else forAll (elements . HM.toList $ s ^. store . toEntities) $ \(eId, e) ->
                            (s, eId, HM.lookup (e ^. space) (s ^. store . toSpaces))
                                `shouldSatisfy` ( \(_, _, mSpace) ->
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
                     in safeStore `shouldBe` unsafeStore
            describe "Permissions" $ do
                readTests
                createTests
                updateTests
                removeTests
