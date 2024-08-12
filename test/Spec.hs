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

import Lib.Types.Id (GroupId, ActorId)
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
import Lib.Actions.Tabulation (resetTabulation, tempFromStore)

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Foldable (for_)
import Control.Monad.Extra (unless)
import Control.Monad.State (execState, get)
import Control.Lens ((^.))
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
        in  s ^. temp `shouldBe` tempFromStore (s ^. store)
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


