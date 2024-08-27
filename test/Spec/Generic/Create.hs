module Spec.Generic.Create where

import Lib.Class (TerseDB (genSyncShared, commit), TerseDBGen (runTerseDB), storeSpace, setUniversePermission, storeActor, storeGroup, setMemberPermission, addMember)
import Data.Data (Proxy(Proxy))
import Test.Syd (Spec, describe, it, shouldSatisfy)
import Lib.Types.Id (ActorId, GroupId, SpaceId)
import Test.QuickCheck (forAll, Testable (property))
import Spec.Sync.Sample.Store (arbitraryEmptyShared)
import qualified Lib.Sync.Types.Store as Sync
import Control.Lens ((^.), At (at))
import Control.Monad (unless)
import qualified Data.List.NonEmpty as NE
import Lib.Types.Permission (CollectionPermissionWithExemption(CollectionPermissionWithExemption), CollectionPermission (Create))
import Data.Maybe (isJust)

createTests :: forall m n. (TerseDB n m, TerseDBGen n) => Proxy m -> Spec
createTests Proxy = do
  describe "Should Succeed" $ do
    it "Space via universe" $
      forAll arbitraryEmptyShared $ \(s, adminActor, adminGroup) ->
        property $ \(aId :: ActorId, gId :: GroupId, sId :: SpaceId) -> do
          let go :: m Sync.Shared
              go = do
                setStage adminActor adminGroup aId gId
                worked <-
                  setUniversePermission
                    (NE.singleton adminActor)
                    (CollectionPermissionWithExemption Create False)
                    gId
                unless worked $
                  error $
                    "Couldn't grant universe create permissions " <> show gId
                worked <- storeSpace (NE.singleton aId) sId
                unless worked $ error $ "Couldn't store space " <> show (aId, gId, sId)
                genSyncShared
          s' <- runTerseDB (commit go) s
          shouldSatisfy (s' ^. Sync.store . Sync.toSpaces . at sId) isJust

setStage
  :: (TerseDB n m) => ActorId -> GroupId -> ActorId -> GroupId -> m ()
setStage adminActor adminGroup aId gId = do
  worked <- storeActor (NE.singleton adminActor) aId
  unless worked $ error $ "Couldn't create actor " <> show aId
  worked <- storeGroup (NE.singleton adminActor) gId
  unless worked $ error $ "Couldn't create group " <> show gId
  worked <- setMemberPermission (NE.singleton adminActor) Create adminGroup gId
  unless worked $
    error $
      "Couldn't grant membership creation to admin group " <> show gId
  worked <- addMember (NE.singleton adminActor) gId aId
  unless worked $ error $ "Couldn't add member " <> show (gId, aId)
