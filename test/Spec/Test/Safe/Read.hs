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
  , setGroupPermission
  , setMemberPermission
  )
import Lib.Actions.Safe.Verify.SpaceAndEntity (canReadSpace, canReadSpaceOld)
import Lib.Actions.Tabulation (resetTabulation, tempFromStore)

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Foldable (for_)
import Data.Maybe (isJust, isNothing)
import Control.Monad.Extra (unless, when)
import Control.Monad.State (MonadState, execState, get, evalState)
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
            go = do
              worked <- storeSpace adminActor sId
              unless worked $ error $ "Couldn't make space " <> show sId
              worked <- storeActor adminActor aId
              unless worked $ error $ "Couldn't make actor " <> show aId
              worked <- storeGroup adminActor gId
              unless worked $ error $ "Couldn't make group " <> show gId
              worked <- setMemberPermission adminActor Create adminGroup gId
              unless worked $ error $ "Couldn't set group permission " <> show gId
              worked <- addMember adminActor gId aId
              unless worked $ error $ "Couldn't add member " <> show (gId, aId)
              -- worked <- setSpacePermission adminActor gId Exists sId
              -- unless worked $ error $ "Couldn't set space permission " <> show (gId, sId)
              worked <- setUniversePermission adminActor
                (CollectionPermissionWithExemption Read False) gId
              unless worked $ error $ "Couldn't set universe permission " <> show gId
              canReadSpace aId sId
            s' = execState go s
        in  shouldSatisfy (s', aId, gId, sId) $ \_ -> evalState go s == True

-- FIXME each group or space or something should have a set of shit it specifically _can't_ see?
-- i.e., the shit it's blind to? Er, NonExists within the set, assuming read rights are present?
