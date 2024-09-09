module Lib.Class where

import Control.Concurrent.STM (STM, atomically)
import Control.Lens (ix, (^.), (^?), _Just)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Extra (andM, anyM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State (evalStateT, get, put)
import Control.Monad.Trans.Control (liftBaseWith)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust)
import Data.Maybe.HT (toMaybe)
import DeferredFolds.UnfoldlM (UnfoldlM)
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import Lib.Actions.Safe.Utils (conditionally)
import qualified Lib.Async.Actions.Safe.Verify as Async
import qualified Lib.Async.Actions.Tabulation as Async
import qualified Lib.Async.Actions.Unsafe.Read as Async
import qualified Lib.Async.Actions.Unsafe.Remove as Async
import qualified Lib.Async.Actions.Unsafe.Store as Async
import qualified Lib.Async.Actions.Unsafe.Update as Async
import qualified Lib.Async.Actions.Unsafe.Update.Group as Async
import qualified Lib.Async.Types.Monad as Async
import qualified Lib.Async.Types.Store as Async
import Lib.Async.Types.Store.Iso (
  genSyncStore,
  genSyncTemp,
  loadSyncStore,
  loadSyncTemp,
 )
import qualified Lib.Async.Types.Tabulation as Async
import qualified Lib.Sync.Actions.Safe.Verify as Sync
import qualified Lib.Sync.Actions.Tabulation as Sync
import qualified Lib.Sync.Actions.Unsafe.Read as Sync
import qualified Lib.Sync.Actions.Unsafe.Remove as Sync
import qualified Lib.Sync.Actions.Unsafe.Store as Sync
import qualified Lib.Sync.Actions.Unsafe.Update as Sync
import qualified Lib.Sync.Actions.Unsafe.Update.Group as Sync
import qualified Lib.Sync.Types.Monad as Sync
import qualified Lib.Sync.Types.Store as Sync
import qualified Lib.Sync.Types.Store.Entity as Sync
import qualified Lib.Sync.Types.Store.Groups as Sync
import qualified Lib.Sync.Types.Store.Tabulation.Group as Sync
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission (Read),
  CollectionPermissionWithExemption (CollectionPermissionWithExemption),
  SinglePermission (Exists),
 )
import ListT (ListT)
import qualified ListT
import qualified StmContainers.Map as Map
import qualified StmContainers.Multimap as Multimap
import qualified StmContainers.Set as Set
import System.Random.Stateful (Uniform (uniformM), globalStdGen)

generateWithAuthority
  :: ( MonadIO m
     , Uniform a
     )
  => (a -> m Bool)
  -> m (Maybe a)
generateWithAuthority perform = do
  ident <- uniformM globalStdGen
  worked <- perform ident
  pure (toMaybe worked ident)

class (Monad m) => TerseDBGen m where
  runTerseDB :: m a -> Sync.Shared -> IO a
  newActor :: NonEmpty ActorId -> m (Maybe ActorId)
  newGroup :: NonEmpty ActorId -> m (Maybe GroupId)
  newSpace :: NonEmpty ActorId -> m (Maybe SpaceId)
  newEntity
    :: NonEmpty ActorId
    -> SpaceId
    -> Maybe VersionId
    -> m (Maybe (EntityId, VersionId))
  newVersion :: NonEmpty ActorId -> EntityId -> m (Maybe VersionId)

instance TerseDBGen (Sync.TerseM IO) where
  runTerseDB = evalStateT
  newGroup = generateWithAuthority . storeGroup
  newActor = generateWithAuthority . storeActor
  newSpace = generateWithAuthority . storeSpace
  newEntity creator sId mFork =
    generateWithAuthority (\(eId, vId) -> storeEntity creator eId sId vId mFork)
  newVersion creator eId =
    generateWithAuthority (storeNextVersion creator eId)

instance TerseDBGen (Async.TerseM IO) where
  runTerseDB x s = do
    emptyS <- atomically Async.newShared
    let y = do
          hoist atomically $ loadSyncShared s
          x
    runReaderT y emptyS
  newGroup creator = generateWithAuthority $ \gId -> do
    s <- ask
    liftBase . atomically $ runReaderT (storeGroup creator gId) s
  newActor creator = generateWithAuthority $ \aId -> do
    s <- ask
    liftBase . atomically $ runReaderT (storeActor creator aId) s
  newSpace creator = generateWithAuthority $ \sId -> do
    s <- ask
    liftBase . atomically $ runReaderT (storeSpace creator sId) s
  newEntity creator sId mFork = generateWithAuthority $ \(eId, vId) -> do
    s <- ask
    liftBase . atomically $ runReaderT (storeEntity creator eId sId vId mFork) s
  newVersion creator eId = generateWithAuthority $ \vId -> do
    s <- ask
    liftBase . atomically $ runReaderT (storeNextVersion creator eId vId) s

class (Monad m) => TerseDB n m | m -> n where
  commit :: m a -> n a
  loadSyncShared :: Sync.Shared -> m ()
  genSyncShared :: m Sync.Shared
  resetTabulation :: m ()
  anyCanReadActor :: NonEmpty ActorId -> m Bool
  anyCanCreateActor :: NonEmpty ActorId -> m Bool
  anyCanUpdateActor :: NonEmpty ActorId -> ActorId -> m Bool
  anyCanDeleteActor :: NonEmpty ActorId -> ActorId -> m Bool
  anyCanReadGroup :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanCreateGroup :: NonEmpty ActorId -> m Bool
  anyCanUpdateGroup :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanDeleteGroup :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanReadMember :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanCreateMember :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanUpdateMember :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanDeleteMember :: NonEmpty ActorId -> GroupId -> m Bool
  anyCanReadSpace :: NonEmpty ActorId -> SpaceId -> m Bool
  anyCanReadSpaceOld :: NonEmpty ActorId -> SpaceId -> m Bool
  anyCanCreateSpace :: NonEmpty ActorId -> m Bool
  anyCanUpdateSpace :: NonEmpty ActorId -> SpaceId -> m Bool
  anyCanDeleteSpace :: NonEmpty ActorId -> SpaceId -> m Bool
  anyCanReadEntity :: NonEmpty ActorId -> EntityId -> m Bool
  anyCanReadAllEntities :: NonEmpty ActorId -> SpaceId -> m Bool
  anyCanCreateEntity :: NonEmpty ActorId -> SpaceId -> m Bool
  anyCanUpdateEntity :: NonEmpty ActorId -> EntityId -> m Bool
  anyCanDeleteEntity :: NonEmpty ActorId -> EntityId -> m Bool
  anyCanReadVersion :: NonEmpty ActorId -> VersionId -> m Bool
  anyCanCreateVersion :: NonEmpty ActorId -> EntityId -> m Bool
  anyCanUpdateVersion :: NonEmpty ActorId -> VersionId -> m Bool
  anyCanDeleteVersion :: NonEmpty ActorId -> VersionId -> m Bool
  hasUniversePermission :: ActorId -> CollectionPermissionWithExemption -> m Bool
  hasOrganizationPermission
    :: ActorId -> CollectionPermissionWithExemption -> m Bool
  hasRecruiterPermission :: ActorId -> CollectionPermission -> m Bool
  hasSpacePermission :: ActorId -> SpaceId -> SinglePermission -> m Bool
  hasEntityPermission :: ActorId -> SpaceId -> CollectionPermission -> m Bool
  hasGroupPermission :: ActorId -> GroupId -> SinglePermission -> m Bool
  hasMemberPermission :: ActorId -> GroupId -> CollectionPermission -> m Bool
  unsafeActorExists :: ActorId -> m Bool
  unsafeGroupExists :: GroupId -> m Bool
  unsafeMemberExists :: GroupId -> ActorId -> m Bool
  unsafeSpaceExists :: SpaceId -> m Bool
  unsafeEntityExists :: EntityId -> m Bool
  unsafeVersionExists :: VersionId -> m Bool
  unsafeReadUniversePermission :: GroupId -> m CollectionPermissionWithExemption
  unsafeReadOrganizationPermission
    :: GroupId -> m CollectionPermissionWithExemption
  unsafeReadRecruiterPermission :: GroupId -> m CollectionPermission
  unsafeReadSpacePermission :: GroupId -> SpaceId -> m (Maybe SinglePermission)
  unsafeReadEntityPermission
    :: GroupId -> SpaceId -> m (Maybe CollectionPermission)
  unsafeReadGroupPermission :: GroupId -> GroupId -> m (Maybe SinglePermission)
  unsafeReadMemberPermission
    :: GroupId -> GroupId -> m (Maybe CollectionPermission)
  unsafeReadTabUniversePermission
    :: GroupId -> m CollectionPermissionWithExemption
  unsafeReadTabOrganizationPermission
    :: GroupId -> m CollectionPermissionWithExemption
  unsafeReadTabRecruiterPermission :: GroupId -> m CollectionPermission
  unsafeReadTabSpacePermission :: GroupId -> SpaceId -> m CollectionPermission
  unsafeReadTabEntityPermission :: GroupId -> SpaceId -> m CollectionPermission
  unsafeReadTabGroupPermission :: GroupId -> GroupId -> m CollectionPermission
  unsafeReadTabMemberPermission :: GroupId -> GroupId -> m CollectionPermission
  unsafeReadAllSpacePermissionEager
    :: GroupId -> m (UnfoldlM m (SpaceId, SinglePermission))
  unsafeReadAllSpacePermissionLazy
    :: GroupId -> m (ListT m (SpaceId, SinglePermission))
  unsafeReadAllEntityPermissionEager
    :: GroupId -> m (UnfoldlM m (SpaceId, CollectionPermission))
  unsafeReadAllEntityPermissionLazy
    :: GroupId -> m (ListT m (SpaceId, CollectionPermission))
  unsafeReadAllGroupPermissionEager
    :: GroupId -> m (UnfoldlM m (GroupId, SinglePermission))
  unsafeReadAllGroupPermissionLazy
    :: GroupId -> m (ListT m (GroupId, SinglePermission))
  unsafeReadAllMemberPermissionEager
    :: GroupId -> m (UnfoldlM m (GroupId, CollectionPermission))
  unsafeReadAllMemberPermissionLazy
    :: GroupId -> m (ListT m (GroupId, CollectionPermission))
  unsafeReadRootGroupsEager :: m (UnfoldlM m GroupId)
  unsafeReadRootGroupsLazy :: m (ListT m GroupId)
  unsafeReadPrevGroup :: GroupId -> m (Maybe GroupId)
  unsafeReadNextGroupsEager :: GroupId -> m (UnfoldlM m GroupId)
  unsafeReadNextGroupsLazy :: GroupId -> m (ListT m GroupId)
  unsafeReadReferencesEager :: VersionId -> m (UnfoldlM m VersionId)
  unsafeReadReferencesLazy :: VersionId -> m (ListT m VersionId)
  unsafeReadReferencesFromEager :: VersionId -> m (UnfoldlM m VersionId)
  unsafeReadReferencesFromLazy :: VersionId -> m (ListT m VersionId)
  unsafeReadForkOf :: EntityId -> m (Maybe VersionId)
  unsafeReadForkedByEager :: VersionId -> m (UnfoldlM m EntityId)
  unsafeReadForkedByLazy :: VersionId -> m (ListT m EntityId)
  unsafeReadSubscriptionsEager :: VersionId -> m (UnfoldlM m EntityId)
  unsafeReadSubscriptionsLazy :: VersionId -> m (ListT m EntityId)
  unsafeReadSubscriptionsFromEager :: EntityId -> m (UnfoldlM m VersionId)
  unsafeReadSubscriptionsFromLazy :: EntityId -> m (ListT m VersionId)
  unsafeReadActorsEager :: m (UnfoldlM m ActorId)
  unsafeReadActorsLazy :: m (ListT m ActorId)
  unsafeReadGroupsEager :: m (UnfoldlM m GroupId)
  unsafeReadGroupsLazy :: m (ListT m GroupId)
  unsafeReadMembersEager :: GroupId -> m (UnfoldlM m ActorId)
  unsafeReadMembersLazy :: GroupId -> m (ListT m ActorId)
  unsafeReadMembersOfEager :: ActorId -> m (UnfoldlM m GroupId)
  unsafeReadMembersOfLazy :: ActorId -> m (ListT m GroupId)
  unsafeReadSpacesEager :: m (UnfoldlM m SpaceId)
  unsafeReadSpacesLazy :: m (ListT m SpaceId)
  unsafeReadEntitiesEager :: SpaceId -> m (UnfoldlM m EntityId)
  unsafeReadEntitiesLazy :: SpaceId -> m (ListT m EntityId)
  unsafeReadEntitySpace :: EntityId -> m (Maybe SpaceId)
  unsafeReadVersionsEager :: EntityId -> m (UnfoldlM m VersionId)
  unsafeReadVersionsLazy :: EntityId -> m (ListT m VersionId)
  unsafeStoreGroup :: GroupId -> m ()
  unsafeStoreActor :: ActorId -> m ()
  unsafeAddMember :: GroupId -> ActorId -> m ()
  unsafeStoreSpace :: SpaceId -> m ()
  unsafeStoreEntity :: EntityId -> SpaceId -> VersionId -> Maybe VersionId -> m ()
  unsafeStoreVersion :: EntityId -> VersionId -> m ()
  unsafeLinkGroups :: GroupId -> GroupId -> m ()
  unsafeUnlinkGroups :: GroupId -> GroupId -> m ()
  unsafeAdjustUniversePermission
    :: (CollectionPermissionWithExemption -> CollectionPermissionWithExemption)
    -> GroupId
    -> m ()
  unsafeAdjustOrganizationPermission
    :: (CollectionPermissionWithExemption -> CollectionPermissionWithExemption)
    -> GroupId
    -> m ()
  unsafeAdjustRecruiterPermission
    :: (CollectionPermission -> CollectionPermission) -> GroupId -> m ()
  unsafeAdjustSpacePermission
    :: (Maybe SinglePermission -> Maybe SinglePermission) -> GroupId -> SpaceId -> m ()
  unsafeAdjustEntityPermission
    :: (CollectionPermission -> CollectionPermission) -> GroupId -> SpaceId -> m ()
  unsafeAdjustGroupPermission
    :: (Maybe SinglePermission -> Maybe SinglePermission) -> GroupId -> GroupId -> m ()
  unsafeAdjustMemberPermission
    :: (CollectionPermission -> CollectionPermission) -> GroupId -> GroupId -> m ()
  unsafeAddReference :: VersionId -> VersionId -> m ()
  unsafeRemoveReference :: VersionId -> VersionId -> m ()
  unsafeAddSubscription :: VersionId -> EntityId -> m ()
  unsafeRemoveSubscription :: VersionId -> EntityId -> m ()
  unsafeUpdateFork :: EntityId -> Maybe VersionId -> m ()
  unsafeMoveEntity :: EntityId -> SpaceId -> m ()
  unsafeOffsetVersionIndex :: VersionId -> Int -> m ()
  unsafeSetVersionIndex :: VersionId -> Int -> m ()
  unsafeRemoveVersion :: VersionId -> m ()
  unsafeRemoveEntity :: EntityId -> m ()
  unsafeRemoveSpace :: SpaceId -> m ()
  unsafeRemoveMember :: GroupId -> ActorId -> m ()
  unsafeRemoveActor :: ActorId -> m ()
  unsafeRemoveGroup :: GroupId -> m ()

instance (MonadThrow m) => TerseDB (Sync.TerseM m) (Sync.TerseM m) where
  commit = id
  loadSyncShared = put
  genSyncShared = get
  resetTabulation = Sync.resetTabulation
  anyCanReadActor = Sync.anyCanReadActor
  anyCanCreateActor = Sync.anyCanCreateActor
  anyCanUpdateActor = Sync.anyCanUpdateActor
  anyCanDeleteActor = Sync.anyCanDeleteActor
  anyCanReadGroup = Sync.anyCanReadGroup
  anyCanCreateGroup = Sync.anyCanCreateGroup
  anyCanUpdateGroup = Sync.anyCanUpdateGroup
  anyCanDeleteGroup = Sync.anyCanDeleteGroup
  anyCanReadMember = Sync.anyCanReadMember
  anyCanCreateMember = Sync.anyCanCreateMember
  anyCanUpdateMember = Sync.anyCanUpdateMember
  anyCanDeleteMember = Sync.anyCanDeleteMember
  anyCanReadSpace = Sync.anyCanReadSpace
  anyCanReadSpaceOld = Sync.anyCanReadSpaceOld
  anyCanCreateSpace = Sync.anyCanCreateSpace
  anyCanUpdateSpace = Sync.anyCanUpdateSpace
  anyCanDeleteSpace = Sync.anyCanDeleteSpace
  anyCanReadEntity = Sync.anyCanReadEntity
  anyCanReadAllEntities = Sync.anyCanReadAllEntities
  anyCanCreateEntity = Sync.anyCanCreateEntity
  anyCanUpdateEntity = Sync.anyCanUpdateEntity
  anyCanDeleteEntity = Sync.anyCanDeleteEntity
  anyCanReadVersion = Sync.anyCanReadVersion
  anyCanCreateVersion = Sync.anyCanCreateVersion
  anyCanUpdateVersion = Sync.anyCanUpdateVersion
  anyCanDeleteVersion = Sync.anyCanDeleteVersion
  hasUniversePermission = Sync.hasUniversePermission
  hasOrganizationPermission = Sync.hasOrganizationPermission
  hasRecruiterPermission = Sync.hasRecruiterPermission
  hasSpacePermission = Sync.hasSpacePermission
  hasEntityPermission = Sync.hasEntityPermission
  hasGroupPermission = Sync.hasGroupPermission
  hasMemberPermission = Sync.hasMemberPermission
  unsafeActorExists aId = do
    s <- get
    pure . isJust $ s ^? Sync.store . Sync.toActors . ix aId
  unsafeGroupExists gId = do
    s <- get
    pure . isJust $ s ^? Sync.store . Sync.toGroups . Sync.nodes . ix gId
  unsafeMemberExists gId aId = do
    s <- get
    pure . isJust $
      s ^? Sync.store . Sync.toGroups . Sync.nodes . ix gId . Sync.members . ix aId
  unsafeSpaceExists sId = do
    s <- get
    pure . isJust $ s ^? Sync.store . Sync.toSpaces . ix sId
  unsafeEntityExists eId = do
    s <- get
    pure . isJust $ s ^? Sync.store . Sync.toEntities . ix eId
  unsafeVersionExists vId = do
    s <- get
    pure . isJust $ s ^? Sync.store . Sync.toVersions . ix vId
  unsafeReadRootGroupsEager = do
    s <- get
    let gs =
          s ^. Sync.store . Sync.toGroups . Sync.roots
    pure $ UnfoldlM.foldable gs
  unsafeReadRootGroupsLazy = do
    s <- get
    let gs =
          s ^. Sync.store . Sync.toGroups . Sync.roots
    pure $ ListT.fromFoldable gs
  unsafeReadPrevGroup gId = do
    s <- get
    pure $ s ^? Sync.store . Sync.toGroups . Sync.nodes . ix gId . Sync.prev . _Just
  unsafeReadNextGroupsEager gId = do
    s <- get
    let ns =
          fromMaybe mempty $
            s ^? Sync.store . Sync.toGroups . Sync.nodes . ix gId . Sync.next
    pure $ UnfoldlM.foldable ns
  unsafeReadNextGroupsLazy gId = do
    s <- get
    let ns =
          fromMaybe mempty $
            s ^? Sync.store . Sync.toGroups . Sync.nodes . ix gId . Sync.next
    pure $ ListT.fromFoldable ns
  unsafeReadUniversePermission gId = do
    s <- get
    pure . fromMaybe minBound $
      s ^? Sync.store . Sync.toGroups . Sync.nodes . ix gId . Sync.universePermission
  unsafeReadOrganizationPermission gId = do
    s <- get
    pure . fromMaybe minBound $
      s
        ^? Sync.store . Sync.toGroups . Sync.nodes . ix gId . Sync.organizationPermission
  unsafeReadRecruiterPermission gId = do
    s <- get
    pure . fromMaybe minBound $
      s ^? Sync.store . Sync.toGroups . Sync.nodes . ix gId . Sync.recruiterPermission
  unsafeReadSpacePermission gId sId = do
    s <- get
    pure $ s ^? Sync.store . Sync.toSpacePermissions . ix gId . ix sId
  unsafeReadEntityPermission gId sId = do
    s <- get
    pure $ s ^? Sync.store . Sync.toEntityPermissions . ix gId . ix sId
  unsafeReadGroupPermission gId gId' = do
    s <- get
    pure $ s ^? Sync.store . Sync.toGroupPermissions . ix gId . ix gId'
  unsafeReadMemberPermission gId gId' = do
    s <- get
    pure $ s ^? Sync.store . Sync.toMemberPermissions . ix gId . ix gId'
  unsafeReadTabUniversePermission gId = do
    s <- get
    pure . fromMaybe minBound $
      s ^? Sync.temp . Sync.toTabulatedGroups . ix gId . Sync.forUniverse
  unsafeReadTabOrganizationPermission gId = do
    s <- get
    pure . fromMaybe minBound $
      s ^? Sync.temp . Sync.toTabulatedGroups . ix gId . Sync.forOrganization
  unsafeReadTabRecruiterPermission gId = do
    s <- get
    pure . fromMaybe minBound $
      s ^? Sync.temp . Sync.toTabulatedGroups . ix gId . Sync.forRecruiter
  unsafeReadTabSpacePermission gId sId = do
    s <- get
    pure . fromMaybe minBound $
      s ^? Sync.temp . Sync.toTabulatedGroups . ix gId . Sync.forSpaces . ix sId
  unsafeReadTabEntityPermission gId sId = do
    s <- get
    pure . fromMaybe minBound $
      s ^? Sync.temp . Sync.toTabulatedGroups . ix gId . Sync.forEntities . ix sId
  unsafeReadTabGroupPermission gId gId' = do
    s <- get
    pure . fromMaybe minBound $
      s ^? Sync.temp . Sync.toTabulatedGroups . ix gId . Sync.forGroups . ix gId'
  unsafeReadTabMemberPermission gId gId' = do
    s <- get
    pure . fromMaybe minBound $
      s ^? Sync.temp . Sync.toTabulatedGroups . ix gId . Sync.forMembers . ix gId'
  unsafeReadAllSpacePermissionEager gId = do
    s <- get
    pure . UnfoldlM.foldable . maybe [] HM.toList $
      s ^? Sync.store . Sync.toSpacePermissions . ix gId
  unsafeReadAllSpacePermissionLazy gId = do
    s <- get
    pure . ListT.fromFoldable . maybe [] HM.toList $
      s ^? Sync.store . Sync.toSpacePermissions . ix gId
  unsafeReadAllEntityPermissionEager gId = do
    s <- get
    pure . UnfoldlM.foldable . maybe [] HM.toList $
      s ^? Sync.store . Sync.toEntityPermissions . ix gId
  unsafeReadAllEntityPermissionLazy gId = do
    s <- get
    pure . ListT.fromFoldable . maybe [] HM.toList $
      s ^? Sync.store . Sync.toEntityPermissions . ix gId
  unsafeReadAllGroupPermissionEager gId = do
    s <- get
    pure . UnfoldlM.foldable . maybe [] HM.toList $
      s ^? Sync.store . Sync.toGroupPermissions . ix gId
  unsafeReadAllGroupPermissionLazy gId = do
    s <- get
    pure . ListT.fromFoldable . maybe [] HM.toList $
      s ^? Sync.store . Sync.toGroupPermissions . ix gId
  unsafeReadAllMemberPermissionEager gId = do
    s <- get
    pure . UnfoldlM.foldable . maybe [] HM.toList $
      s ^? Sync.store . Sync.toMemberPermissions . ix gId
  unsafeReadAllMemberPermissionLazy gId = do
    s <- get
    pure . ListT.fromFoldable . maybe [] HM.toList $
      s ^? Sync.store . Sync.toMemberPermissions . ix gId
  unsafeReadReferencesEager = Sync.unsafeReadReferencesEager
  unsafeReadReferencesLazy = Sync.unsafeReadReferencesLazy
  unsafeReadReferencesFromEager = Sync.unsafeReadReferencesFromEager
  unsafeReadReferencesFromLazy = Sync.unsafeReadReferencesFromLazy
  unsafeReadSubscriptionsEager = Sync.unsafeReadSubscriptionsEager
  unsafeReadSubscriptionsLazy = Sync.unsafeReadSubscriptionsLazy
  unsafeReadSubscriptionsFromEager = Sync.unsafeReadSubscriptionsFromEager
  unsafeReadSubscriptionsFromLazy = Sync.unsafeReadSubscriptionsFromLazy
  unsafeReadForkOf eId = do
    s <- get
    pure $ s ^? Sync.store . Sync.toEntities . ix eId . Sync.fork . _Just
  unsafeReadForkedByEager = Sync.unsafeReadForkedByEager
  unsafeReadForkedByLazy = Sync.unsafeReadForkedByLazy
  unsafeReadActorsEager = Sync.unsafeReadActorsEager
  unsafeReadActorsLazy = Sync.unsafeReadActorsLazy
  unsafeReadGroupsEager = Sync.unsafeReadGroupsEager
  unsafeReadGroupsLazy = Sync.unsafeReadGroupsLazy
  unsafeReadMembersEager = Sync.unsafeReadMembersEager
  unsafeReadMembersLazy = Sync.unsafeReadMembersLazy
  unsafeReadMembersOfEager = Sync.unsafeReadMembersOfEager
  unsafeReadMembersOfLazy = Sync.unsafeReadMembersOfLazy
  unsafeReadSpacesEager = Sync.unsafeReadSpacesEager
  unsafeReadSpacesLazy = Sync.unsafeReadSpacesLazy
  unsafeReadEntitiesEager = Sync.unsafeReadEntitiesEager
  unsafeReadEntitiesLazy = Sync.unsafeReadEntitiesLazy
  unsafeReadEntitySpace eId = do
    s <- get
    pure $ s ^? Sync.temp . Sync.toSpaceOf . ix eId
  unsafeReadVersionsEager = Sync.unsafeReadVersionsEager
  unsafeReadVersionsLazy = Sync.unsafeReadVersionsLazy
  unsafeStoreGroup = Sync.unsafeStoreGroup
  unsafeStoreActor = Sync.unsafeStoreActor
  unsafeAddMember = Sync.unsafeAddMember
  unsafeStoreSpace = Sync.unsafeStoreSpace
  unsafeStoreEntity = Sync.unsafeStoreEntity
  unsafeStoreVersion = Sync.unsafeStoreVersion
  unsafeLinkGroups = Sync.unsafeLinkGroups
  unsafeUnlinkGroups = Sync.unsafeUnlinkGroups
  unsafeAdjustUniversePermission = Sync.unsafeAdjustUniversePermission
  unsafeAdjustOrganizationPermission = Sync.unsafeAdjustOrganizationPermission
  unsafeAdjustRecruiterPermission = Sync.unsafeAdjustRecruiterPermission
  unsafeAdjustSpacePermission = Sync.unsafeAdjustSpacePermission
  unsafeAdjustEntityPermission = Sync.unsafeAdjustEntityPermission
  unsafeAdjustGroupPermission = Sync.unsafeAdjustGroupPermission
  unsafeAdjustMemberPermission = Sync.unsafeAdjustMemberPermission
  unsafeAddReference = Sync.unsafeAddReference
  unsafeRemoveReference = Sync.unsafeRemoveReference
  unsafeAddSubscription = Sync.unsafeAddSubscription
  unsafeRemoveSubscription = Sync.unsafeRemoveSubscription
  unsafeUpdateFork = Sync.unsafeUpdateFork
  unsafeMoveEntity = Sync.unsafeMoveEntity
  unsafeOffsetVersionIndex = Sync.unsafeOffsetVersionIndex
  unsafeSetVersionIndex = Sync.unsafeSetVersionIndex
  unsafeRemoveVersion = Sync.unsafeRemoveVersion
  unsafeRemoveEntity = Sync.unsafeRemoveEntity
  unsafeRemoveSpace = Sync.unsafeRemoveSpace
  unsafeRemoveMember = Sync.unsafeRemoveMember
  unsafeRemoveActor = Sync.unsafeRemoveActor
  unsafeRemoveGroup = Sync.unsafeRemoveGroup

instance TerseDB (Async.TerseM IO) (Async.TerseM STM) where
  commit = hoist atomically
  loadSyncShared s = do
    loadSyncStore (s ^. Sync.store)
    loadSyncTemp (s ^. Sync.temp)
  genSyncShared = Sync.Shared <$> genSyncStore <*> genSyncTemp
  resetTabulation = Async.resetTabulation
  anyCanReadActor = Async.anyCanReadActor
  anyCanCreateActor = Async.anyCanCreateActor
  anyCanUpdateActor = Async.anyCanUpdateActor
  anyCanDeleteActor = Async.anyCanDeleteActor
  anyCanReadGroup = Async.anyCanReadGroup
  anyCanCreateGroup = Async.anyCanCreateGroup
  anyCanUpdateGroup = Async.anyCanUpdateGroup
  anyCanDeleteGroup = Async.anyCanDeleteGroup
  anyCanReadMember = Async.anyCanReadMember
  anyCanCreateMember = Async.anyCanCreateMember
  anyCanUpdateMember = Async.anyCanUpdateMember
  anyCanDeleteMember = Async.anyCanDeleteMember
  anyCanReadSpace = Async.anyCanReadSpace
  anyCanReadSpaceOld = Async.anyCanReadSpaceOld
  anyCanCreateSpace = Async.anyCanCreateSpace
  anyCanUpdateSpace = Async.anyCanUpdateSpace
  anyCanDeleteSpace = Async.anyCanDeleteSpace
  anyCanReadEntity = Async.anyCanReadEntity
  anyCanReadAllEntities = Async.anyCanReadAllEntities
  anyCanCreateEntity = Async.anyCanCreateEntity
  anyCanUpdateEntity = Async.anyCanUpdateEntity
  anyCanDeleteEntity = Async.anyCanDeleteEntity
  anyCanReadVersion = Async.anyCanReadVersion
  anyCanCreateVersion = Async.anyCanCreateVersion
  anyCanUpdateVersion = Async.anyCanUpdateVersion
  anyCanDeleteVersion = Async.anyCanDeleteVersion
  hasUniversePermission = Async.hasUniversePermission
  hasOrganizationPermission = Async.hasOrganizationPermission
  hasRecruiterPermission = Async.hasRecruiterPermission
  hasSpacePermission = Async.hasSpacePermission
  hasEntityPermission = Async.hasEntityPermission
  hasGroupPermission = Async.hasGroupPermission
  hasMemberPermission = Async.hasMemberPermission
  unsafeActorExists aId = do
    s <- ask
    liftBase $ Set.lookup aId (s ^. Async.store . Async.toActors)
  unsafeGroupExists gId = do
    s <- ask
    liftBase $ Set.lookup gId (s ^. Async.store . Async.toGroups)
  unsafeMemberExists gId aId = do
    s <- ask
    liftBase $ Multimap.lookup aId gId (s ^. Async.store . Async.toMembers)
  unsafeSpaceExists sId = do
    s <- ask
    liftBase $ Set.lookup sId (s ^. Async.store . Async.toSpaces)
  unsafeEntityExists eId = do
    s <- ask
    liftBase $ isJust <$> Map.lookup eId (s ^. Async.store . Async.toEntities)
  unsafeVersionExists vId = do
    s <- ask
    liftBase $ Set.lookup vId (s ^. Async.store . Async.toVersions)
  unsafeReadRootGroupsEager = do
    s <- ask
    let x = Set.unfoldlM (s ^. Async.store . Async.toRoots)
    pure $ UnfoldlM.hoist liftBase (`runReaderT` s) x
  unsafeReadRootGroupsLazy = do
    s <- ask
    let x :: ListT STM GroupId
        x = Set.listT (s ^. Async.store . Async.toRoots)
    pure $ hoist liftBase x
  unsafeReadPrevGroup gId = do
    s <- ask
    liftBase $ Map.lookup gId (s ^. Async.store . Async.toGroupsPrev)
  unsafeReadUniversePermission gId = do
    s <- ask
    liftBase
      . fmap (fromMaybe minBound)
      . Map.lookup gId
      $ s ^. Async.store . Async.toPermUniverse
  unsafeReadOrganizationPermission gId = do
    s <- ask
    liftBase
      . fmap (fromMaybe minBound)
      . Map.lookup gId
      $ s ^. Async.store . Async.toPermOrganization
  unsafeReadRecruiterPermission gId = do
    s <- ask
    liftBase
      . fmap (fromMaybe minBound)
      . Map.lookup gId
      $ s ^. Async.store . Async.toPermRecruiter
  unsafeReadSpacePermission gId sId = do
    s <- ask
    liftBase $ do
      mPerm <- Map.lookup gId $ s ^. Async.store . Async.toPermOther
      case mPerm of
        Nothing -> pure Nothing
        Just perm -> Map.lookup sId $ perm ^. Async.spacePermission
  unsafeReadEntityPermission gId sId = do
    s <- ask
    liftBase $ do
      mPerm <- Map.lookup gId $ s ^. Async.store . Async.toPermOther
      case mPerm of
        Nothing -> pure Nothing
        Just perm -> Map.lookup sId $ perm ^. Async.entityPermission
  unsafeReadGroupPermission gId gId' = do
    s <- ask
    liftBase $ do
      mPerm <- Map.lookup gId $ s ^. Async.store . Async.toPermOther
      case mPerm of
        Nothing -> pure Nothing
        Just perm -> Map.lookup gId' $ perm ^. Async.groupPermission
  unsafeReadMemberPermission gId gId' = do
    s <- ask
    liftBase $ do
      mPerm <- Map.lookup gId $ s ^. Async.store . Async.toPermOther
      case mPerm of
        Nothing -> pure Nothing
        Just perm -> Map.lookup gId' $ perm ^. Async.memberPermission
  unsafeReadTabUniversePermission gId = do
    s <- ask
    liftBase $
      fmap (fromMaybe minBound) $
        Map.lookup gId $
          s ^. Async.temp . Async.toTabUniverse
  unsafeReadTabOrganizationPermission gId = do
    s <- ask
    liftBase $
      fmap (fromMaybe minBound) $
        Map.lookup gId $
          s ^. Async.temp . Async.toTabOrganization
  unsafeReadTabRecruiterPermission gId = do
    s <- ask
    liftBase $
      fmap (fromMaybe minBound) $
        Map.lookup gId $
          s ^. Async.temp . Async.toTabRecruiter
  unsafeReadTabSpacePermission gId sId = do
    s <- ask
    liftBase $ fmap (fromMaybe minBound) $ do
      mTab <- Map.lookup gId $ s ^. Async.temp . Async.toTabOther
      case mTab of
        Nothing -> pure Nothing
        Just tab -> Map.lookup sId $ tab ^. Async.forSpaces
  unsafeReadTabEntityPermission gId sId = do
    s <- ask
    liftBase $ fmap (fromMaybe minBound) $ do
      mTab <- Map.lookup gId $ s ^. Async.temp . Async.toTabOther
      case mTab of
        Nothing -> pure Nothing
        Just tab -> Map.lookup sId $ tab ^. Async.forEntities
  unsafeReadTabGroupPermission gId gId' = do
    s <- ask
    liftBase $ fmap (fromMaybe minBound) $ do
      mTab <- Map.lookup gId $ s ^. Async.temp . Async.toTabOther
      case mTab of
        Nothing -> pure Nothing
        Just tab -> Map.lookup gId' $ tab ^. Async.forGroups
  unsafeReadTabMemberPermission gId gId' = do
    s <- ask
    liftBase $ fmap (fromMaybe minBound) $ do
      mTab <- Map.lookup gId $ s ^. Async.temp . Async.toTabOther
      case mTab of
        Nothing -> pure Nothing
        Just tab -> Map.lookup gId' $ tab ^. Async.forMembers
  unsafeReadAllSpacePermissionEager gId = do
    s <- ask
    liftBaseWith $ \runInBase -> do
      mPerm <- Map.lookup gId $ s ^. Async.store . Async.toPermOther
      case mPerm of
        Nothing -> pure (UnfoldlM.foldable [])
        Just perm ->
          pure
            . UnfoldlM.hoist liftBase runInBase
            . Map.unfoldlM
            $ perm ^. Async.spacePermission
  unsafeReadAllSpacePermissionLazy gId = do
    s <- ask
    liftBase $ do
      mPerm <- Map.lookup gId $ s ^. Async.store . Async.toPermOther
      case mPerm of
        Nothing -> pure (ListT.fromFoldable [])
        Just perm ->
          pure . hoist liftBase . Map.listT $ perm ^. Async.spacePermission
  unsafeReadAllEntityPermissionEager gId = do
    s <- ask
    liftBaseWith $ \runInBase -> do
      mPerm <- Map.lookup gId $ s ^. Async.store . Async.toPermOther
      case mPerm of
        Nothing -> pure (UnfoldlM.foldable [])
        Just perm ->
          pure
            . UnfoldlM.hoist liftBase runInBase
            . Map.unfoldlM
            $ perm ^. Async.entityPermission
  unsafeReadAllEntityPermissionLazy gId = do
    s <- ask
    liftBase $ do
      mPerm <- Map.lookup gId $ s ^. Async.store . Async.toPermOther
      case mPerm of
        Nothing -> pure (ListT.fromFoldable [])
        Just perm ->
          pure . hoist liftBase . Map.listT $ perm ^. Async.entityPermission
  unsafeReadAllGroupPermissionEager gId = do
    s <- ask
    liftBaseWith $ \runInBase -> do
      mPerm <- Map.lookup gId $ s ^. Async.store . Async.toPermOther
      case mPerm of
        Nothing -> pure (UnfoldlM.foldable [])
        Just perm ->
          pure
            . UnfoldlM.hoist liftBase runInBase
            . Map.unfoldlM
            $ perm ^. Async.groupPermission
  unsafeReadAllGroupPermissionLazy gId = do
    s <- ask
    liftBase $ do
      mPerm <- Map.lookup gId $ s ^. Async.store . Async.toPermOther
      case mPerm of
        Nothing -> pure (ListT.fromFoldable [])
        Just perm ->
          pure . hoist liftBase . Map.listT $ perm ^. Async.groupPermission
  unsafeReadAllMemberPermissionEager gId = do
    s <- ask
    liftBaseWith $ \runInBase -> do
      mPerm <- Map.lookup gId $ s ^. Async.store . Async.toPermOther
      case mPerm of
        Nothing -> pure (UnfoldlM.foldable [])
        Just perm ->
          pure
            . UnfoldlM.hoist liftBase runInBase
            . Map.unfoldlM
            $ perm ^. Async.memberPermission
  unsafeReadAllMemberPermissionLazy gId = do
    s <- ask
    liftBase $ do
      mPerm <- Map.lookup gId $ s ^. Async.store . Async.toPermOther
      case mPerm of
        Nothing -> pure (ListT.fromFoldable [])
        Just perm ->
          pure . hoist liftBase . Map.listT $ perm ^. Async.memberPermission
  unsafeReadNextGroupsEager = Async.unsafeReadNextGroupsEager
  unsafeReadNextGroupsLazy = Async.unsafeReadNextGroupsLazy
  unsafeReadReferencesEager = Async.unsafeReadReferencesEager
  unsafeReadReferencesLazy = Async.unsafeReadReferencesLazy
  unsafeReadReferencesFromEager = Async.unsafeReadReferencesFromEager
  unsafeReadReferencesFromLazy = Async.unsafeReadReferencesFromLazy
  unsafeReadSubscriptionsEager = Async.unsafeReadSubscriptionsEager
  unsafeReadSubscriptionsLazy = Async.unsafeReadSubscriptionsLazy
  unsafeReadSubscriptionsFromEager = Async.unsafeReadSubscriptionsFromEager
  unsafeReadSubscriptionsFromLazy = Async.unsafeReadSubscriptionsFromLazy
  unsafeReadForkOf eId = do
    s <- ask
    liftBase . Map.lookup eId $ s ^. Async.store . Async.toForks
  unsafeReadForkedByEager = Async.unsafeReadForkedByEager
  unsafeReadForkedByLazy = Async.unsafeReadForkedByLazy
  unsafeReadActorsEager = Async.unsafeReadActorsEager
  unsafeReadActorsLazy = Async.unsafeReadActorsLazy
  unsafeReadGroupsEager = Async.unsafeReadGroupsEager
  unsafeReadGroupsLazy = Async.unsafeReadGroupsLazy
  unsafeReadMembersEager = Async.unsafeReadMembersEager
  unsafeReadMembersLazy = Async.unsafeReadMembersLazy
  unsafeReadMembersOfEager = Async.unsafeReadMembersOfEager
  unsafeReadMembersOfLazy = Async.unsafeReadMembersOfLazy
  unsafeReadSpacesEager = Async.unsafeReadSpacesEager
  unsafeReadSpacesLazy = Async.unsafeReadSpacesLazy
  unsafeReadEntitiesEager = Async.unsafeReadEntitiesEager
  unsafeReadEntitiesLazy = Async.unsafeReadEntitiesLazy
  unsafeReadEntitySpace eId = do
    s <- ask
    liftBase $ Map.lookup eId $ s ^. Async.temp . Async.toSpaceOf
  unsafeReadVersionsEager = Async.unsafeReadVersionsEager
  unsafeReadVersionsLazy = Async.unsafeReadVersionsLazy
  unsafeStoreGroup = Async.unsafeStoreGroup
  unsafeStoreActor = Async.unsafeStoreActor
  unsafeAddMember = Async.unsafeAddMember
  unsafeStoreSpace = Async.unsafeStoreSpace
  unsafeStoreEntity = Async.unsafeStoreEntity
  unsafeStoreVersion = Async.unsafeStoreVersion
  unsafeLinkGroups = Async.unsafeLinkGroups
  unsafeUnlinkGroups = Async.unsafeUnlinkGroups
  unsafeAdjustUniversePermission = Async.unsafeAdjustUniversePermission
  unsafeAdjustOrganizationPermission = Async.unsafeAdjustOrganizationPermission
  unsafeAdjustRecruiterPermission = Async.unsafeAdjustRecruiterPermission
  unsafeAdjustSpacePermission = Async.unsafeAdjustSpacePermission
  unsafeAdjustEntityPermission = Async.unsafeAdjustEntityPermission
  unsafeAdjustGroupPermission = Async.unsafeAdjustGroupPermission
  unsafeAdjustMemberPermission = Async.unsafeAdjustMemberPermission
  unsafeAddReference = Async.unsafeAddReference
  unsafeRemoveReference = Async.unsafeRemoveReference
  unsafeAddSubscription = Async.unsafeAddSubscription
  unsafeRemoveSubscription = Async.unsafeRemoveSubscription
  unsafeUpdateFork = Async.unsafeUpdateFork
  unsafeMoveEntity = Async.unsafeMoveEntity
  unsafeOffsetVersionIndex = Async.unsafeOffsetVersionIndex
  unsafeSetVersionIndex = Async.unsafeSetVersionIndex
  unsafeRemoveVersion = Async.unsafeRemoveVersion
  unsafeRemoveEntity = Async.unsafeRemoveEntity
  unsafeRemoveSpace = Async.unsafeRemoveSpace
  unsafeRemoveMember = Async.unsafeRemoveMember
  unsafeRemoveActor = Async.unsafeRemoveActor
  unsafeRemoveGroup = Async.unsafeRemoveGroup

-- * Read

-- ** Exists

actorExists :: (TerseDB n m) => NonEmpty ActorId -> ActorId -> m (Maybe Bool)
actorExists readers aId = do
  canAdjust <- anyCanReadActor readers
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeActorExists aId

groupExists :: (TerseDB n m) => NonEmpty ActorId -> GroupId -> m (Maybe Bool)
groupExists readers gId = do
  canAdjust <- anyCanReadGroup readers gId
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeGroupExists gId

memberExists
  :: (TerseDB n m) => NonEmpty ActorId -> GroupId -> ActorId -> m (Maybe Bool)
memberExists readers gId aId = do
  canAdjust <- anyCanReadMember readers gId
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeActorExists aId

spaceExists :: (TerseDB n m) => NonEmpty ActorId -> SpaceId -> m (Maybe Bool)
spaceExists readers sId = do
  canAdjust <- anyCanReadSpace readers sId
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeSpaceExists sId

entityExists :: (TerseDB n m) => NonEmpty ActorId -> EntityId -> m (Maybe Bool)
entityExists readers eId = do
  canAdjust <- anyCanReadEntity readers eId
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeEntityExists eId

versionExists
  :: (TerseDB n m) => NonEmpty ActorId -> VersionId -> m (Maybe Bool)
versionExists readers vId = do
  canAdjust <- anyCanReadVersion readers vId
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeVersionExists vId

-- ** Details

readUniversePermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> m (Maybe CollectionPermissionWithExemption)
readUniversePermission readers gId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead then pure Nothing else Just <$> unsafeReadUniversePermission gId
readOrganizationPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> m (Maybe CollectionPermissionWithExemption)
readOrganizationPermission readers gId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadOrganizationPermission gId
readRecruiterPermission
  :: (TerseDB n m) => NonEmpty ActorId -> GroupId -> m (Maybe CollectionPermission)
readRecruiterPermission readers gId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead then pure Nothing else Just <$> unsafeReadRecruiterPermission gId
readSpacePermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> SpaceId
  -> m (Maybe (Maybe SinglePermission))
readSpacePermission readers gId sId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead then pure Nothing else Just <$> unsafeReadSpacePermission gId sId
readEntityPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> SpaceId
  -> m (Maybe (Maybe CollectionPermission))
readEntityPermission readers gId sId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadEntityPermission gId sId
readGroupPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> GroupId
  -> m (Maybe (Maybe SinglePermission))
readGroupPermission readers gId gId' = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadGroupPermission gId gId'
readMemberPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> GroupId
  -> m (Maybe (Maybe CollectionPermission))
readMemberPermission readers gId gId' = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadMemberPermission gId gId'

readTabUniversePermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> m (Maybe CollectionPermissionWithExemption)
readTabUniversePermission readers gId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadTabUniversePermission gId
readTabOrganizationPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> m (Maybe CollectionPermissionWithExemption)
readTabOrganizationPermission readers gId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadTabOrganizationPermission gId
readTabRecruiterPermission
  :: (TerseDB n m) => NonEmpty ActorId -> GroupId -> m (Maybe CollectionPermission)
readTabRecruiterPermission readers gId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadTabRecruiterPermission gId
readTabSpacePermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> SpaceId
  -> m (Maybe CollectionPermission)
readTabSpacePermission readers gId sId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadTabSpacePermission gId sId
readTabEntityPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> SpaceId
  -> m (Maybe CollectionPermission)
readTabEntityPermission readers gId sId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadTabEntityPermission gId sId
readTabGroupPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> GroupId
  -> m (Maybe CollectionPermission)
readTabGroupPermission readers gId gId' = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadTabGroupPermission gId gId'
readTabMemberPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> GroupId
  -> m (Maybe CollectionPermission)
readTabMemberPermission readers gId gId' = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadTabMemberPermission gId gId'

readAllSpacePermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> m (Maybe (UnfoldlM m (SpaceId, SinglePermission)))
readAllSpacePermission readers gId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadAllSpacePermissionEager gId
readAllEntityPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> m (Maybe (UnfoldlM m (SpaceId, CollectionPermission)))
readAllEntityPermission readers gId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadAllEntityPermissionEager gId
readAllGroupPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> m (Maybe (UnfoldlM m (GroupId, SinglePermission)))
readAllGroupPermission readers gId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadAllGroupPermissionEager gId
readAllMemberPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> m (Maybe (UnfoldlM m (GroupId, CollectionPermission)))
readAllMemberPermission readers gId = do
  canRead <- anyCanReadGroup readers gId
  if not canRead
    then pure Nothing
    else Just <$> unsafeReadAllMemberPermissionEager gId

readRootGroups
  :: (TerseDB n m) => NonEmpty ActorId -> m (UnfoldlM m GroupId)
readRootGroups readers = do
  UnfoldlM.filter (anyCanReadGroup readers)
    <$> unsafeReadRootGroupsEager

readPrevGroup
  :: (TerseDB n m) => NonEmpty ActorId -> GroupId -> m (Maybe (Maybe GroupId))
readPrevGroup readers gId = do
  canAdjust <- anyCanReadGroup readers gId
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeReadPrevGroup gId

readNextGroups
  :: (TerseDB n m) => NonEmpty ActorId -> GroupId -> m (Maybe (UnfoldlM m GroupId))
readNextGroups readers gId = do
  canAdjust <- anyCanReadGroup readers gId
  if not canAdjust
    then pure Nothing
    else
      Just . UnfoldlM.filter (anyCanReadGroup readers)
        <$> unsafeReadNextGroupsEager gId

readReferences
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> VersionId
  -> m (Maybe (UnfoldlM m VersionId))
readReferences readers vId = do
  canAdjust <- anyCanReadVersion readers vId
  if not canAdjust
    then pure Nothing
    else
      Just . UnfoldlM.filter (anyCanReadVersion readers)
        <$> unsafeReadReferencesEager vId

readReferencesFrom
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> VersionId
  -> m (Maybe (UnfoldlM m VersionId))
readReferencesFrom readers vId = do
  canAdjust <- anyCanReadVersion readers vId
  if not canAdjust
    then pure Nothing
    else
      Just . UnfoldlM.filter (anyCanReadVersion readers)
        <$> unsafeReadReferencesFromEager vId

readSubscriptions
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> VersionId
  -> m (Maybe (UnfoldlM m EntityId))
readSubscriptions readers vId = do
  canAdjust <- anyCanReadVersion readers vId
  if not canAdjust
    then pure Nothing
    else
      Just . UnfoldlM.filter (anyCanReadEntity readers)
        <$> unsafeReadSubscriptionsEager vId

readSubscriptionsFrom
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> EntityId
  -> m (Maybe (UnfoldlM m VersionId))
readSubscriptionsFrom readers eId = do
  canAdjust <- anyCanReadEntity readers eId
  if not canAdjust
    then pure Nothing
    else
      Just . UnfoldlM.filter (anyCanReadVersion readers)
        <$> unsafeReadSubscriptionsFromEager eId

readForkOf
  :: (TerseDB n m) => NonEmpty ActorId -> EntityId -> m (Maybe (Maybe VersionId))
readForkOf readers eId = do
  canAdjust <- anyCanReadEntity readers eId
  if not canAdjust
    then pure Nothing
    else do
      mFork <- unsafeReadForkOf eId
      case mFork of
        Nothing -> pure (Just Nothing)
        Just fork -> do
          canRead <- anyCanReadVersion readers fork
          pure $ if not canRead then Nothing else Just (Just fork)

readForkedBy
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> VersionId
  -> m (Maybe (UnfoldlM m EntityId))
readForkedBy readers vId = do
  canAdjust <- anyCanReadVersion readers vId
  if not canAdjust
    then pure Nothing
    else
      Just . UnfoldlM.filter (anyCanReadEntity readers)
        <$> unsafeReadForkedByEager vId

-- ** Collections

readActors
  :: (TerseDB n m) => NonEmpty ActorId -> m (Maybe (UnfoldlM m ActorId))
readActors readers = do
  canAdjust <- anyCanReadActor readers
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeReadActorsEager

readGroups
  :: (TerseDB n m) => NonEmpty ActorId -> m (Maybe (UnfoldlM m GroupId))
readGroups readers = do
  canAdjust <-
    anyM
      ( \reader ->
          hasOrganizationPermission reader (CollectionPermissionWithExemption Read False)
      )
      (NE.toList readers)
  if not canAdjust
    then pure Nothing
    else
      Just . UnfoldlM.filter (anyCanReadGroup readers)
        <$> unsafeReadGroupsEager

readMembers
  :: (TerseDB n m) => NonEmpty ActorId -> GroupId -> m (Maybe (UnfoldlM m ActorId))
readMembers readers gId = do
  canAdjust <-
    andM
      [ anyCanReadGroup readers gId
      , anyCanReadActor readers
      ]
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeReadMembersEager gId

readMembersOf
  :: (TerseDB n m) => NonEmpty ActorId -> ActorId -> m (Maybe (UnfoldlM m GroupId))
readMembersOf readers aId = do
  canAdjust <- anyCanReadActor readers
  if not canAdjust
    then pure Nothing
    else
      Just . UnfoldlM.filter (anyCanReadGroup readers)
        <$> unsafeReadMembersOfEager aId

readSpaces
  :: (TerseDB n m) => NonEmpty ActorId -> m (Maybe (UnfoldlM m SpaceId))
readSpaces readers = do
  canAdjust <-
    anyM
      ( \reader -> hasUniversePermission reader (CollectionPermissionWithExemption Read False)
      )
      (NE.toList readers)
  if not canAdjust
    then pure Nothing
    else
      Just . UnfoldlM.filter (anyCanReadSpace readers)
        <$> unsafeReadSpacesEager

readEntities
  :: (TerseDB n m) => NonEmpty ActorId -> SpaceId -> m (Maybe (UnfoldlM m EntityId))
readEntities readers sId = do
  canAdjust <- anyCanReadAllEntities readers sId
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeReadEntitiesEager sId

readEntitySpace
  :: (TerseDB n m) => NonEmpty ActorId -> EntityId -> m (Maybe (Maybe SpaceId))
readEntitySpace readers eId = do
  canAdjust <- anyCanReadEntity readers eId
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeReadEntitySpace eId

readVersions
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> EntityId
  -> m (Maybe (UnfoldlM m VersionId))
readVersions readers eId = do
  canAdjust <- anyCanReadEntity readers eId
  if not canAdjust
    then pure Nothing
    else Just <$> unsafeReadVersionsEager eId

-- * Store

storeGroup
  :: (TerseDB n m)
  => NonEmpty ActorId
  -- ^ actor storing the group
  -> GroupId
  -- ^ group being stored
  -> m Bool
storeGroup creator gId =
  anyCanCreateGroup creator
    >>= conditionally (unsafeStoreGroup gId)

storeActor
  :: (TerseDB n m)
  => NonEmpty ActorId
  -- ^ actor storing the created actor
  -> ActorId
  -- ^ created actor being stored
  -> m Bool
storeActor creator aId =
  anyCanCreateActor creator
    >>= conditionally (unsafeStoreActor aId)

addMember
  :: (TerseDB n m)
  => NonEmpty ActorId
  -- ^ actor creating membership
  -> GroupId
  -- ^ group gaining a member
  -> ActorId
  -- ^ new member
  -> m Bool
addMember creator gId aId = do
  anyCanCreateMember creator gId
    >>= conditionally (unsafeAddMember gId aId)

storeSpace
  :: (TerseDB n m)
  => NonEmpty ActorId
  -- ^ actor storing the space
  -> SpaceId
  -- ^ space being created
  -> m Bool
storeSpace creator sId =
  anyCanCreateSpace creator
    >>= conditionally (unsafeStoreSpace sId)

storeEntity
  :: (TerseDB n m)
  => NonEmpty ActorId
  -- ^ actor storing the entity
  -> EntityId
  -- ^ entity being stored
  -> SpaceId
  -- ^ space in which entity is being stored
  -> VersionId
  -- ^ initial version
  -> Maybe VersionId
  -- ^ forked version
  -> m Bool
storeEntity creator eId sId vId mFork = do
  canAdjust <-
    andM
      [ anyCanCreateEntity creator sId
      , maybe (pure True) (anyCanReadVersion creator) mFork
      ]
  conditionally (unsafeStoreEntity eId sId vId mFork) canAdjust

storeNextVersion
  :: (TerseDB n m)
  => NonEmpty ActorId
  -- ^ actor attempting to store a version
  -> EntityId
  -- ^ entity receiving a new version
  -> VersionId
  -- ^ version being stored
  -> m Bool
storeNextVersion creator eId vId = do
  anyCanUpdateEntity creator eId
    >>= conditionally (unsafeStoreVersion eId vId)

-- * Update

linkGroups
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> GroupId
  -> m Bool
linkGroups updater gId childId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup updater gId
      , anyCanUpdateGroup updater childId
      ]
  conditionally (unsafeLinkGroups gId childId) canAdjust

unlinkGroups
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> GroupId
  -> m Bool
unlinkGroups updater gId childId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup updater gId
      , anyCanUpdateGroup updater childId
      ]
  conditionally (unsafeUnlinkGroups gId childId) canAdjust

updateGroupParent
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> Maybe GroupId
  -> m Bool
updateGroupParent updater gId mPrevId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup updater gId
      , case mPrevId of
          Nothing -> pure True
          Just prevId -> anyCanUpdateGroup updater prevId
      ]
  if not canAdjust
    then pure False
    else do
      mOldPrevId <- unsafeReadPrevGroup gId
      if mOldPrevId == mPrevId
        then pure True
        else do
          worked <- case mOldPrevId of
            Nothing -> pure True
            Just prevId -> unlinkGroups updater prevId gId
          if not worked
            then pure False
            else case mPrevId of
              Nothing -> pure True
              Just prevId -> linkGroups updater prevId gId

-- | Will only update the group if the actor has same or greater permission
setUniversePermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> CollectionPermissionWithExemption
  -- ^ permission being set
  -> GroupId
  -- ^ group subject to new permission
  -> m Bool
setUniversePermission creator p gId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator gId
      , anyM (`hasUniversePermission` p) (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustUniversePermission (const p) gId)
    canAdjust

setOrganizationPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> CollectionPermissionWithExemption
  -- ^ permission being set
  -> GroupId
  -- ^ group subject to new permission
  -> m Bool
setOrganizationPermission creator p gId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator gId
      , anyM (`hasOrganizationPermission` p) (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustOrganizationPermission (const p) gId)
    canAdjust

setRecruiterPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> CollectionPermission
  -- ^ permission being set
  -> GroupId
  -- ^ group subject to new permission
  -> m Bool
setRecruiterPermission creator p gId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator gId
      , anyM (`hasRecruiterPermission` p) (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustRecruiterPermission (const p) gId)
    canAdjust

setSpacePermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> Maybe SinglePermission
  -- ^ permission being set
  -> GroupId
  -- ^ group subject to new permission
  -> SpaceId
  -- ^ relevant to this space
  -> m Bool
setSpacePermission creator p gId sId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator gId
      , anyM (\c -> hasSpacePermission c sId (fromMaybe Exists p)) (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustSpacePermission (const p) gId sId)
    canAdjust

setEntityPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> CollectionPermission
  -- ^ permission being set
  -> GroupId
  -- ^ group subject to new permission
  -> SpaceId
  -- ^ relevant to this space
  -> m Bool
setEntityPermission creator p gId sId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator gId
      , anyM (\c -> hasEntityPermission c sId p) (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustEntityPermission (const p) gId sId)
    canAdjust

setGroupPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> Maybe SinglePermission
  -- ^ permission being set
  -> GroupId
  -- ^ group subject to new permission
  -> GroupId
  -- ^ relevant to this group (grants one group to NEAO other groups)
  -> m Bool
setGroupPermission creator p gId towardGId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator gId
      , anyM
          (\c -> hasGroupPermission c towardGId (fromMaybe Exists p))
          (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustGroupPermission (const p) gId towardGId)
    canAdjust

setMemberPermission
  :: (TerseDB n m)
  => NonEmpty ActorId
  -- ^ actor attempting to set permission
  -> CollectionPermission
  -- ^ the permission being granted
  -> GroupId
  -- ^ the group gaining the permission
  -> GroupId
  -- ^ the group that can have their members manipulated
  -> m Bool
setMemberPermission creator p manipulatorGId manipulatedGId = do
  canAdjust <-
    andM
      [ anyCanUpdateGroup creator manipulatorGId
      , anyM (\c -> hasMemberPermission c manipulatedGId p) (NE.toList creator)
      ]
  conditionally
    (unsafeAdjustMemberPermission (const p) manipulatorGId manipulatedGId)
    canAdjust

-- | Moving an entity between spaces requires delete authority on the current space, and create authority on the destination space
moveEntity
  :: (TerseDB n m) => NonEmpty ActorId -> EntityId -> SpaceId -> m Bool
moveEntity updater eId newSId = do
  canAdjust <-
    andM
      [ anyCanDeleteEntity updater eId
      , anyCanCreateEntity updater newSId
      ]
  conditionally (unsafeMoveEntity eId newSId) canAdjust

addReference
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> VersionId
  -> VersionId
  -> m Bool
addReference updater vId refId = do
  canAdjust <-
    andM
      [ anyCanReadVersion updater refId
      , anyCanUpdateVersion updater vId
      ]
  conditionally (unsafeAddReference vId refId) canAdjust

removeReference
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> VersionId
  -> VersionId
  -> m Bool
removeReference updater vId refId = do
  canAdjust <-
    andM
      [ anyCanReadVersion updater refId
      , anyCanUpdateVersion updater vId
      ]
  conditionally (unsafeRemoveReference vId refId) canAdjust

addSubscription
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> VersionId
  -> EntityId
  -> m Bool
addSubscription updater vId subId = do
  canAdjust <-
    andM
      [ anyCanReadEntity updater subId
      , anyCanUpdateVersion updater vId
      ]
  conditionally (unsafeAddSubscription vId subId) canAdjust

removeSubscription
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> VersionId
  -> EntityId
  -> m Bool
removeSubscription updater vId subId = do
  canAdjust <-
    andM
      [ anyCanReadEntity updater subId
      , anyCanUpdateVersion updater vId
      ]
  conditionally (unsafeRemoveSubscription vId subId) canAdjust

updateFork
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> EntityId
  -> Maybe VersionId
  -> m Bool
updateFork updater eId mFork = do
  canAdjust <-
    andM
      [ anyCanUpdateEntity updater eId
      , maybe (pure True) (anyCanReadVersion updater) mFork
      ]
  conditionally (unsafeUpdateFork eId mFork) canAdjust

offsetVersionIndex
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> VersionId
  -> Int
  -> m Bool
offsetVersionIndex updater vId offset = do
  anyCanUpdateVersion updater vId
    >>= conditionally (unsafeOffsetVersionIndex vId offset)

setVersionIndex
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> VersionId
  -> Int
  -> m Bool
setVersionIndex updater vId idx = do
  anyCanUpdateVersion updater vId
    >>= conditionally (unsafeSetVersionIndex vId idx)

-- * Remove

removeVersion
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> VersionId
  -> m Bool
removeVersion remover vId = do
  canAdjust <-
    andM
      [ anyCanDeleteVersion remover vId
      , let go False _ = pure Nothing
            go True referrerId = Just <$> anyCanUpdateVersion remover referrerId
         in ListT.foldMaybe go True =<< unsafeReadReferencesFromLazy vId
      ]
  conditionally (unsafeRemoveVersion vId) canAdjust

removeEntity
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> EntityId
  -> m Bool
removeEntity remover eId = do
  canAdjust <-
    andM
      [ anyCanDeleteEntity remover eId
      , let go False _ = pure Nothing
            go True subscriberId = Just <$> anyCanUpdateVersion remover subscriberId
         in ListT.foldMaybe go True =<< unsafeReadSubscriptionsFromLazy eId
      ]
  conditionally (unsafeRemoveEntity eId) canAdjust

removeSpace
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> SpaceId
  -> m Bool
removeSpace remover sId = do
  canAdjust <-
    andM
      [ anyCanDeleteSpace remover sId
      , let go False _ = pure Nothing
            go True eId = Just <$> anyCanDeleteEntity remover eId
         in ListT.foldMaybe go True =<< unsafeReadEntitiesLazy sId
      ]
  conditionally (unsafeRemoveSpace sId) canAdjust

removeMember
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> ActorId
  -> m Bool
removeMember remover gId aId =
  anyCanDeleteMember remover gId >>= conditionally (unsafeRemoveMember gId aId)

removeActor
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> ActorId
  -> m Bool
removeActor remover aId =
  anyCanDeleteActor remover aId >>= conditionally (unsafeRemoveActor aId)

removeGroup
  :: (TerseDB n m)
  => NonEmpty ActorId
  -> GroupId
  -> m Bool
removeGroup remover gId =
  anyCanDeleteGroup remover gId >>= conditionally (unsafeRemoveGroup gId)
