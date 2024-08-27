module Spec.Sync where

import Control.Lens (ix, (^.), (^?!))
import Control.Monad.Extra (unless)
import Control.Monad.State (execState, get)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Lib.Sync.Actions.Tabulation (resetTabulation, tempFromStore)
import Lib.Sync.Actions.Unsafe.Store (
  unsafeAddMember,
  unsafeStoreActor,
  unsafeStoreGroup,
 )
import Lib.Sync.Actions.Unsafe.Update.Group (
  unsafeAdjustEntityPermission,
  unsafeAdjustGroupPermission,
  unsafeAdjustMemberPermission,
  unsafeAdjustOrganizationPermission,
  unsafeAdjustRecruiterPermission,
  unsafeAdjustUniversePermission,
 )
import Lib.Sync.Types.Store (
  store,
  temp,
  toEntities,
  toGroups,
  toSpaceOf,
  toSpaces,
 )
import Lib.Sync.Types.Monad (TerseM)
import Lib.Sync.Types.Store.Groups (nodes)
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (
  CollectionPermission (..),
  CollectionPermissionWithExemption (..),
  SinglePermission (Adjust),
 )
import Spec.Sync.Sample.Store (
  SampleStore (..),
  loadSample,
  storeSample,
 )
import Spec.Sync.Sample.Tree (SampleGroupTree (..))
import Spec.Sync.Test.Groups (groupsTests, testPermissionInheritance)
import Spec.Sync.Test.Joint (jointTests)
import Spec.Sync.Test.Safe.Create (createTests)
import Spec.Sync.Test.Safe.Delete (removeTests)
import Spec.Sync.Test.Safe.Read (readTests)
import Spec.Sync.Test.Safe.Update (updateTests)
import Spec.Sync.Test.Simple (simpleSyncTests)
import Test.QuickCheck (
  elements,
  forAll,
  property,
 )
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)
import Spec.Generic (genericTests)
import Data.Data (Proxy (..))

syncTests :: Spec
syncTests = do
  describe "Generics" (genericTests (Proxy @(TerseM IO)))
  -- describe "Simple" simpleSyncTests
  -- describe "Groups" groupsTests
  -- describe "Store" $ do
  --   it "temp data is reproducible from store" $
  --     property $ \(xs :: SampleStore) ->
  --       let s = loadSample xs
  --           sTemp = s ^. temp
  --           sFrom = tempFromStore (s ^. store)
  --        in shouldSatisfy (s ^. store, sTemp, sFrom) $ \_ -> sTemp == sFrom
  --   it "all descendants are supersets of roots - build all permissions" $
  --     property $ \(xs :: SampleStore) ->
  --       let groups = sampleGroups xs
  --        in testPermissionInheritance (current groups) (children groups) (loadSample xs)
  --   it "all spaces are disjoint" $
  --     property $ \(xs :: SampleStore) ->
  --       let s = loadSample xs
  --        in foldr HS.intersection mempty (s ^. store . toSpaces)
  --             `shouldBe` mempty
  --   it "all elements exist in their space" $
  --     property $ \(xs :: SampleStore) ->
  --       let s = loadSample xs
  --        in if null (s ^. store . toEntities)
  --             then property True
  --             else forAll (elements . HM.toList $ s ^. store . toEntities) $ \(eId, e) ->
  --               (s, eId, HM.lookup (s ^?! temp . toSpaceOf . ix eId) (s ^. store . toSpaces))
  --                 `shouldSatisfy` ( \(_, _, mSpace) ->
  --                                     maybe False (eId `HS.member`) mSpace
  --                                 )
  --   describe "Safe" $ do
  --     it "should be identical to unsafe" $
  --       property $ \(xs :: SampleStore, adminActor :: ActorId, adminGroup :: GroupId) ->
  --         let safeStore = storeSample xs adminActor adminGroup
  --             unsafeStore = flip execState (loadSample xs) $ do
  --               -- setup admin
  --               unsafeStoreGroup adminGroup
  --               unsafeAdjustUniversePermission
  --                 (const $ CollectionPermissionWithExemption Delete True)
  --                 adminGroup
  --               unsafeAdjustOrganizationPermission
  --                 (const $ CollectionPermissionWithExemption Delete True)
  --                 adminGroup
  --               unsafeAdjustRecruiterPermission (const Delete) adminGroup
  --               unsafeStoreActor adminActor
  --               unsafeAddMember adminGroup adminActor
  --               -- "backdate" the granting of group adjust rights to admin group
  --               s <- get
  --               for_ (HM.keys $ s ^. store . toGroups . nodes) $ \gId ->
  --                 unless (gId == adminGroup) $ do
  --                   unsafeAdjustGroupPermission (const (Just Adjust)) adminGroup gId
  --                   unsafeAdjustMemberPermission (const Create) adminGroup gId
  --               -- "backdate" the granting of space create rights to admin group
  --               s <- get
  --               for_ (HM.keys $ s ^. store . toSpaces) $ \sId ->
  --                 unsafeAdjustEntityPermission (const Update) adminGroup sId
  --               resetTabulation
  --          in safeStore `shouldBe` unsafeStore
  --     describe "Permissions" $ do
  --       readTests
  --       createTests
  --       updateTests
  --       removeTests
  --   jointTests
