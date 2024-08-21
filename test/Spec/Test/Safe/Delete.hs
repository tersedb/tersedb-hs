module Spec.Test.Safe.Delete where

import Control.Lens (at, ix, non, (^.), (^?), _Just)
import Control.Monad.Extra (unless)
import Control.Monad.State (MonadState, evalState, execState)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (elemIndex)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Lib.Actions.Safe (emptyShared)
import Lib.Actions.Safe.Remove
  ( removeEntity,
    removeVersion,
  )
import Lib.Actions.Safe.Store (addMember, storeActor, storeEntity, storeGroup, storeSpace)
import Lib.Actions.Safe.Update.Group
  ( linkGroups,
    setEntityPermission,
    setMemberPermission,
    setUniversePermission,
    unlinkGroups,
    updateGroupParent,
  )
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission
  ( CollectionPermission (..),
    CollectionPermissionWithExemption (..),
  )
import Lib.Types.Store
  ( Shared,
    store,
    temp,
    toEntities,
    toForksFrom,
    toGroups,
    toReferencesFrom,
    toSpaces,
    toSubscriptionsFrom,
    toTabulatedGroups,
    toVersions,
  )
import Lib.Types.Store.Entity (fork, space, versions)
import Lib.Types.Store.Groups (next, nodes, prev)
import Lib.Types.Store.Space (entities)
import Lib.Types.Store.Tabulation.Group (hasLessOrEqualPermissionsTo)
import Lib.Types.Store.Version (entity, references, subscriptions)
import Spec.Sample.Store (SampleStore, arbitraryEmptyShared, arbitraryShared, storeSample)
import Test.QuickCheck
  ( arbitrary,
    chooseInt,
    elements,
    forAll,
    property,
    suchThat,
    suchThatMap,
  )
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)

removeTests :: Spec
removeTests = describe "Remove" $ do
  it "Version" $
    let gen = suchThat arbitraryShared $ \(s, _, _) ->
          not . null . HM.filter (\e -> length (e ^. versions) > 1) $ s ^. store . toEntities
     in forAll gen $ \(s, adminActor, adminGroup) ->
          let entsWith2OrMoreVersions =
                HM.filter (\e -> length (e ^. versions) > 1) $
                  s ^. store . toEntities
           in forAll (elements $ HM.toList entsWith2OrMoreVersions) $ \(eId, e) ->
                forAll (elements . NE.toList $ e ^. versions) $ \vId -> do
                  let s' = flip execState s $ do
                        mE <- removeVersion adminActor vId
                        case mE of
                          Just (Right ()) -> pure ()
                          _ -> error $ "Couldn't remove version " <> show (vId, mE)
                  (s' ^? store . toVersions . ix vId) `shouldBe` Nothing
                  ( filter (== vId) . fromMaybe mempty . fmap NE.toList $
                      s' ^? store . toEntities . ix eId . versions
                    )
                    `shouldBe` mempty
                  (s' ^? temp . toReferencesFrom . ix vId) `shouldBe` Nothing
  describe "Entity" $
    it "should delete all versions" $
      let gen = suchThat arbitraryShared $ \(s, _, _) ->
            not . null $ s ^. store . toEntities
       in forAll gen $ \(s, adminActor, adminGroup) ->
            let genE = elements . HM.toList $ s ^. store . toEntities
             in forAll genE $ \(eId, e) -> do
                  let s' = flip execState s $ do
                        worked <- setEntityPermission adminActor Delete adminGroup (e ^. space)
                        unless worked $
                          error $
                            "Couldn't set delete permission for entities " <> show (e ^. space)
                        eWorked <- removeEntity adminActor eId
                        case eWorked of
                          Just (Right ()) -> pure ()
                          _ -> error $ "Couldn't remove entity " <> show (eId, eWorked)
                  (s' ^. store . toEntities . at eId) `shouldBe` Nothing
                  for_ (e ^. versions) $ \vId ->
                    (s' ^. store . toVersions . at vId) `shouldBe` Nothing

-- TODO verify that subscriptions are also removed
-- describe "Space" $
--   it "should delete all entities" $
--     let gen = suchThat arbitraryShared $ \(s, _, _) ->
--           not . null $ s ^. store . toSpaces
--      in forAll gen $ \(s, adminActor, adminGroup) ->
--           let genS = elements . HS.toList $ s ^. store . toSpaces
--            in forAll genS $ \(sId, sp) -> do
--                 let s' = flip execState s $ do
--                       eWorked <- removeSpace adminActor sId
--                       case eWorked of
--                         Just (Right ()) -> pure ()
--                         _ -> error $ "Couldn't remove space " <> (sId, eWorked)
--                 (s' ^. store . toSpaces . at sId) `shouldBe` Nothing
--                 for_ (sp ^. entities) $ \eId ->
--                   (s' ^. store . toEntities . at eId) `shouldBe` Nothing
