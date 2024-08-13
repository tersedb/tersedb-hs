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
    -- it "Groups" $
    --   property $ \(xs :: SampleStore) ->
    --     let s = loadSample xs
    --     in  if null (s ^. store . toActors)
    --              || null (s ^. store . toGroups . nodes) then property True else
    --         let genAId = elements . HM.keys $ s ^. store . toActors
    --             genGId = elements . HM.keys $ s ^. store . toGroups . nodes
    --         in  forAll ((,) <$> genAId <*> genGId) $ \(aId, gId) ->
    --               let resNew = evalState (canReadGroup aId gId) s
    --                   resOld = evalState (canReadGroupOld aId gId) s
    --               in  shouldSatisfy (s, aId, gId, resNew, resOld) $ \_ -> resNew == resOld


-- FIXME each group or space or something should have a set of shit it specifically _can't_ see?
-- i.e., the shit it's blind to? Er, NonExists within the set, assuming read rights are present?
