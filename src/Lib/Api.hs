module Lib.Api where

import Data.Vector (Vector)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId, AnyId (..))
import Lib.Types.Permission (
  CollectionPermission,
  CollectionPermissionWithExemption,
  SinglePermission,
 )
import Data.List.NonEmpty (NonEmpty)
import Lib.Class (TerseDB (anyCanReadActor, commit), readEntities, actorExists, readReferences, readSubscriptions, readSubscriptionsFrom, readReferencesFrom, readPrevGroup, groupExists, memberExists, spaceExists, entityExists, versionExists, readActors, readGroups, readMembersOf, readMembers, readSpaces, readVersions, readNextGroups, readForkOf, readForkedBy, TerseDBGen, storeActor, storeGroup, addMember, storeSpace, storeEntity, storeNextVersion, addReference, removeReference, addSubscription, removeSubscription, updateFork, moveEntity, offsetVersionIndex, setVersionIndex, setUniversePermission, setOrganizationPermission, setRecruiterPermission, setSpacePermission, setEntityPermission, setGroupPermission, setMemberPermission, linkGroups, unlinkGroups, updateGroupParent)
import qualified Data.Vector as V
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import Data.Data (Proxy(Proxy))
import System.Random.Stateful (Uniform(uniformM), globalStdGen)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow(throwM))
import Lib.Types.Errors (UnauthorizedAction(UnauthorizedAction))

data ReadAction
  = ReadAllActors
  | ReadActor ActorId
  | ReadAllGroups
  | ReadGroup GroupId
  | ReadAllMembers GroupId
  | ReadAllMemberOf ActorId
  | ReadMember GroupId ActorId
  | ReadPrevGroup GroupId
  | ReadNextGroups GroupId
  | ReadAllSpaces
  | ReadSpace SpaceId
  | ReadAllEntities SpaceId
  | ReadEntity EntityId
  | ReadAllVersions EntityId
  | ReadVersion VersionId
  | ReadReferences VersionId
  | ReadReferencesOf VersionId
  | ReadSubscriptions VersionId
  | ReadSubscriptionsOf EntityId
  | ReadForkOf EntityId
  | ReadForkedBy VersionId
  deriving (Eq, Show, Read)

data CreateAction
  = CreateActor
  | CreateGroup
  | CreateMember GroupId ActorId
  | CreateSpace
  | CreateEntity SpaceId (Maybe VersionId)
  | CreateVersion EntityId
  deriving (Eq, Show, Read)

data StoreAction
  = StoreActor ActorId
  | StoreGroup GroupId
  | StoreMember GroupId ActorId
  | StoreSpace SpaceId
  | StoreEntity SpaceId (Maybe VersionId) EntityId VersionId
  | StoreVersion EntityId VersionId
  deriving (Eq, Show, Read)

data UpdateAction
  = AddReference VersionId VersionId
  | RemoveReference VersionId VersionId
  | AddSubscription VersionId EntityId
  | RemoveSubscription VersionId EntityId
  | ChangeFork EntityId (Maybe VersionId)
  | MoveEntity EntityId SpaceId
  | OffsetVersion VersionId Int
  | SetVersionIndex VersionId Int
  | SetUniversePermission GroupId CollectionPermissionWithExemption
  | SetOrganizationPermission GroupId CollectionPermissionWithExemption
  | SetRecruiterPermission GroupId CollectionPermission
  | SetSpacePermission GroupId SpaceId (Maybe SinglePermission)
  | SetEntityPermission GroupId SpaceId CollectionPermission
  | SetGroupPermission GroupId GroupId (Maybe SinglePermission)
  | SetMemberPermission GroupId GroupId CollectionPermission
  | LinkGroups GroupId GroupId
  | UnlinkGroups GroupId GroupId
  | UpdateGroupPrev GroupId (Maybe GroupId)
  deriving (Eq, Show, Read)

data DeleteAction
  = DeleteVersion VersionId
  | DeleteEntity EntityId
  | DeleteSpace SpaceId
  | DeleteMember GroupId ActorId
  | DeleteGroup GroupId
  | DeleteActor ActorId
  deriving (Eq, Show, Read)

data Action
  = ReadAction ReadAction
  | CreateAction CreateAction
  | UpdateAction UpdateAction
  | DeleteAction DeleteAction
  deriving (Eq, Show, Read)

data MutableAction
  = StoreMutableAction StoreAction
  | UpdateMutableAction UpdateAction
  | DeleteMutableAction DeleteAction
  deriving (Eq, Show, Read)

toMutate :: Action -> Maybe AnyId -> Maybe MutableAction
toMutate action mId = case action of
  ReadAction _ -> Nothing
  CreateAction x -> StoreMutableAction <$> case (x, mId) of
    (CreateActor, Just (AnyIdActor aId)) -> Just (StoreActor aId)
    (CreateGroup, Just (AnyIdGroup gId)) -> Just (StoreGroup gId)
    (CreateMember gId aId, _) -> Just (StoreMember gId aId)
    (CreateSpace, Just (AnyIdSpace sId)) -> Just (StoreSpace sId)
    (CreateEntity sId mFork, Just (AnyIdEntity eId vId)) -> Just (StoreEntity sId mFork eId vId)
    (CreateVersion eId, Just (AnyIdVersion vId)) -> Just (StoreVersion eId vId)
    _ -> Nothing
  UpdateAction x -> Just (UpdateMutableAction x)
  DeleteAction x -> Just (DeleteMutableAction x)
  

data ReadResponse
  = DoesExist
  | DoesNotExist
  | Actors (Vector ActorId)
  | Groups (Vector GroupId)
  | Members (Vector ActorId)
  | MemberOf (Vector GroupId)
  | PrevGroup (Maybe GroupId)
  | NextGroups (Vector GroupId)
  | Spaces (Vector SpaceId)
  | Entities (Vector EntityId)
  | Versions (Vector VersionId)
  | References (Vector VersionId)
  | ReferencesOf (Vector VersionId)
  | Subscriptions (Vector EntityId)
  | SubscriptionsOf (Vector VersionId)
  | ForkOf (Maybe VersionId)
  | ForkedBy (Vector EntityId)
  deriving (Eq, Show, Read)

data CreateResponse
  = NewActor ActorId
  | NewGroup GroupId
  | NewSpace SpaceId
  | NewEntity EntityId VersionId
  | NewVersion VersionId
  deriving (Eq, Show, Read)

toCreateResponse :: CreateAction -> Maybe AnyId -> Maybe (Either () CreateResponse)
toCreateResponse action id = case (action, id) of
  (CreateActor, Just (AnyIdActor aId)) -> Just . Right $ NewActor aId
  (CreateGroup, Just (AnyIdGroup gId)) -> Just . Right $ NewGroup gId
  (CreateMember _ _, _) -> Just $ Left ()
  (CreateSpace, Just (AnyIdSpace sId)) -> Just . Right $ NewSpace sId
  (CreateEntity _ _, Just (AnyIdEntity eId vId)) -> Just . Right $ NewEntity eId vId
  (CreateVersion _, Just (AnyIdVersion vId)) -> Just . Right $ NewVersion vId
  _ -> Nothing

data Response
  = ReadResponse ReadResponse
  | CreateResponse CreateResponse
  deriving (Eq, Show, Read)

data Authorize a
  = Authorized a
  | Unauthorized
  deriving (Functor)


-- | If one action is unauthorized, continue
actMany :: forall m n. (TerseDB n m, MonadIO n) => Proxy m -> NonEmpty ActorId -> [Action] -> n [Authorize (Maybe Response)]
actMany Proxy actors xs = do
  ids <- traverse genId xs
  let ys :: m [Authorize (Maybe Response)]
      ys = traverse (uncurry (act actors)) (zip xs ids)
  commit ys

-- | If one action is unauthorized, abandon all transactions
actManyStrict :: forall m n. (TerseDB n m, MonadIO n, MonadThrow m) => Proxy m -> NonEmpty ActorId -> [Action] -> n [Maybe Response]
actManyStrict Proxy actors xs = do
  ids <- traverse genId xs
  let ys :: m [Maybe Response]
      ys = traverse go (zip xs ids)
        where
          go (x, mId) = do
            mAuth <- act actors x mId
            case mAuth of
              Unauthorized -> throwM UnauthorizedAction
              Authorized y -> pure y
  commit ys

genId :: forall n. MonadIO n => Action -> n (Maybe AnyId)
genId action = case action of
  CreateAction x -> case x of
    CreateActor -> Just . AnyIdActor <$> gen
    CreateGroup -> Just . AnyIdGroup <$> gen
    CreateSpace -> Just . AnyIdSpace <$> gen
    CreateEntity _ _ -> Just <$> (AnyIdEntity <$> gen <*> gen)
    CreateVersion _ -> Just . AnyIdVersion <$> gen
    _ -> pure Nothing
  _ -> pure Nothing
  where
    gen :: forall a. Uniform a => n a
    gen = uniformM globalStdGen

mutate :: forall m n. TerseDB n m => NonEmpty ActorId -> MutableAction -> m (Authorize ())
mutate actors action = do
  worked <- case action of
    StoreMutableAction x -> goStore x
    UpdateMutableAction x -> goUpdate x
    DeleteMutableAction x -> goDelete x
  pure $ if not worked then Unauthorized else Authorized ()
  where
    goStore x = case x of
      StoreActor aId -> storeActor actors aId
      StoreGroup gId -> storeGroup actors gId
      StoreMember gId aId -> addMember actors gId aId
      StoreSpace sId -> storeSpace actors sId
      StoreEntity sId mFork eId vId -> storeEntity actors eId sId vId mFork
      StoreVersion eId vId -> storeNextVersion actors eId vId

    goUpdate x = case x of
      AddReference vId refId -> addReference actors vId refId
      RemoveReference vId refId -> removeReference actors vId refId
      AddSubscription vId subId -> addSubscription actors vId subId
      RemoveSubscription vId subId -> removeSubscription actors vId subId
      ChangeFork eId mForkId -> updateFork actors eId mForkId
      MoveEntity eId sId -> moveEntity actors eId sId
      OffsetVersion vId offset -> offsetVersionIndex actors vId offset
      SetVersionIndex vId idx -> setVersionIndex actors vId idx
      SetUniversePermission gId p -> setUniversePermission actors p gId
      SetOrganizationPermission gId p -> setOrganizationPermission actors p gId
      SetRecruiterPermission gId p -> setRecruiterPermission actors p gId
      SetSpacePermission gId sId p -> setSpacePermission actors p gId sId
      SetEntityPermission gId sId p -> setEntityPermission actors p gId sId
      SetGroupPermission gId gId' p -> setGroupPermission actors p gId gId'
      SetMemberPermission gId gId' p -> setMemberPermission actors p gId gId'
      LinkGroups gId gId' -> linkGroups actors gId gId'
      UnlinkGroups gId gId' -> unlinkGroups actors gId gId'
      UpdateGroupPrev gId mPrevId -> updateGroupParent actors gId mPrevId

    goDelete x = case x of
      DeleteActor aId -> undefined
      DeleteGroup gId -> undefined
      DeleteMember gId aId -> undefined
      DeleteSpace sId -> undefined
      DeleteEntity eId -> undefined
      DeleteVersion vId -> undefined

act :: forall m n. TerseDB n m => NonEmpty ActorId -> Action -> Maybe AnyId -> m (Authorize (Maybe Response))
act actors action mId = case action of
  ReadAction x -> goRead x
  _ -> case toMutate action mId of
    Just action' ->
      let mkResponse :: () -> Maybe Response
          mkResponse () = case action of
            CreateAction x -> case toCreateResponse x mId of
              Nothing -> error $ "Couldn't make CreateResponse: " <> show x <> ", " <> show mId
              Just (Left ()) -> Nothing
              Just (Right y) -> Just (CreateResponse y)
            _ -> Nothing
      in  fmap mkResponse <$> mutate actors action'
    _ -> error $ "Couldn't generate mutatable action: " <> show action <> ", id: " <> show mId
  where
    goRead x = do
      (res :: Authorize ReadResponse) <- maybeToAuthorized <$> case x of
        ReadAllActors ->
          traverse (fmap Actors . unfoldlMToVector) =<< readActors actors
        ReadActor aId ->
          fmap boolToExists <$> actorExists actors aId
        ReadAllGroups ->
          traverse (fmap Groups . unfoldlMToVector) =<< readGroups actors
        ReadGroup gId ->
          fmap boolToExists <$> groupExists actors gId
        ReadAllMembers gId ->
          traverse (fmap Members . unfoldlMToVector) =<< readMembers actors gId
        ReadAllMemberOf aId ->
          traverse (fmap MemberOf . unfoldlMToVector) =<< readMembersOf actors aId
        ReadMember gId aId ->
          fmap boolToExists <$> memberExists actors gId aId
        ReadPrevGroup gId ->
          fmap PrevGroup <$> readPrevGroup actors gId
        ReadNextGroups gId ->
          traverse (fmap NextGroups . unfoldlMToVector) =<< readNextGroups actors gId
        ReadAllSpaces ->
          traverse (fmap Spaces . unfoldlMToVector) =<< readSpaces actors
        ReadSpace sId ->
          fmap boolToExists <$> spaceExists actors sId
        ReadAllEntities sId ->
          traverse (fmap Entities . unfoldlMToVector) =<< readEntities actors sId
        ReadEntity eId ->
          fmap boolToExists <$> entityExists actors eId
        ReadAllVersions eId ->
          traverse (fmap Versions . unfoldlMToVector) =<< readVersions actors eId
        ReadVersion vId ->
          fmap boolToExists <$> versionExists actors vId
        ReadReferences vId ->
          traverse (fmap References . unfoldlMToVector) =<< readReferences actors vId
        ReadReferencesOf vId ->
          traverse (fmap ReferencesOf . unfoldlMToVector) =<< readReferencesFrom actors vId
        ReadSubscriptions vId ->
          traverse (fmap Subscriptions . unfoldlMToVector) =<< readSubscriptions actors vId
        ReadSubscriptionsOf eId ->
          traverse (fmap SubscriptionsOf . unfoldlMToVector) =<< readSubscriptionsFrom actors eId
        ReadForkOf eId ->
          fmap ForkOf <$> readForkOf actors eId
        ReadForkedBy vId ->
          traverse (fmap ForkedBy . unfoldlMToVector) =<< readForkedBy actors vId
      pure $ Just . ReadResponse <$> res



unfoldlMToVector :: Monad m => UnfoldlM.UnfoldlM m a -> m (Vector a)
unfoldlMToVector = UnfoldlM.foldlM' (\acc x -> pure (acc `V.snoc` x)) mempty

maybeToAuthorized :: Maybe a -> Authorize a
maybeToAuthorized Nothing = Unauthorized
maybeToAuthorized (Just x) = Authorized x

boolToExists :: Bool -> ReadResponse
boolToExists False = DoesNotExist
boolToExists True = DoesExist
