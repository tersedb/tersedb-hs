module Lib.Api where

import Data.Vector (Vector)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId, AnyId (..))
import Lib.Types.Permission (
  CollectionPermission,
  CollectionPermissionWithExemption,
  SinglePermission,
 )
import Data.List.NonEmpty (NonEmpty)
import Lib.Class (TerseDB (anyCanReadActor, commit), readEntities, actorExists, readReferences, readSubscriptions, readSubscriptionsFrom, readReferencesFrom, readPrevGroup, groupExists, memberExists, spaceExists, entityExists, versionExists, readActors, readGroups, readMembersOf, readMembers, readSpaces, readVersions, readNextGroups, readForkOf, readForkedBy, TerseDBGen, storeActor, storeGroup, addMember, storeSpace, storeEntity, storeNextVersion)
import qualified Data.Vector as V
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import Data.Data (Proxy(Proxy))
import System.Random.Stateful (Uniform(uniformM), globalStdGen)
import Control.Monad.IO.Class (MonadIO)

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

data UpdateAction
  = AddReference VersionId VersionId
  | RemoveReference VersionId VersionId
  | AddSubscription VersionId EntityId
  | RemoveSubscription EntityId VersionId
  | ChangeFork (Maybe EntityId)
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

data Response
  = ReadResponse ReadResponse
  | CreateResponse CreateResponse
  | Success
  | Unauthorized
  deriving (Eq, Show, Read)


act :: forall m n. (TerseDB n m, MonadIO n) => Proxy m -> NonEmpty ActorId -> [Action] -> n [Response]
act Proxy actors xs = do
  ids <- traverse genId xs
  commit (traverse (uncurry go) (zip xs ids))
  where
    genId :: Action -> n (Maybe AnyId)
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
    go :: Action -> Maybe AnyId -> m Response
    go action mId = case action of
      ReadAction x -> goRead x
      CreateAction x -> goCreate x mId
      UpdateAction x -> goUpdate x
      DeleteAction x -> goDelete x
    goRead x = case x of
      ReadAllActors -> do
        mXs <- readActors actors
        case mXs of
          Nothing -> pure Unauthorized
          Just xs -> ReadResponse . Actors <$> unfoldlMToVector xs
      ReadActor aId -> do
        mExists <- actorExists actors aId
        pure $ case mExists of
          Nothing -> Unauthorized
          Just y -> ReadResponse $ if y then DoesExist else DoesNotExist
      ReadAllGroups -> do
        mXs <- readGroups actors
        case mXs of
          Nothing -> pure Unauthorized
          Just xs -> ReadResponse . Groups <$> unfoldlMToVector xs
      ReadGroup gId -> do
        mExists <- groupExists actors gId
        pure $ case mExists of
          Nothing -> Unauthorized
          Just y -> ReadResponse $ if y then DoesExist else DoesNotExist
      ReadAllMembers gId -> do
        mXs <- readMembers actors gId
        case mXs of
          Nothing -> pure Unauthorized
          Just xs -> ReadResponse . Members <$> unfoldlMToVector xs
      ReadAllMemberOf aId -> do
        mXs <- readMembersOf actors aId
        case mXs of
          Nothing -> pure Unauthorized
          Just xs -> ReadResponse . MemberOf <$> unfoldlMToVector xs
      ReadMember gId aId -> do
        mExists <- memberExists actors gId aId
        pure $ case mExists of
          Nothing -> Unauthorized
          Just y -> ReadResponse $ if y then DoesExist else DoesNotExist
      ReadPrevGroup gId -> do
        mMPrev <- readPrevGroup actors gId
        pure $ case mMPrev of
          Nothing -> Unauthorized
          Just mPrev -> ReadResponse (PrevGroup mPrev)
      ReadNextGroups gId -> do
        mXs <- readNextGroups actors gId
        case mXs of
          Nothing -> pure Unauthorized
          Just xs -> ReadResponse . NextGroups <$> unfoldlMToVector xs
      ReadAllSpaces -> do
        mXs <- readSpaces actors
        case mXs of
          Nothing -> pure Unauthorized
          Just xs -> ReadResponse . Spaces <$> unfoldlMToVector xs
      ReadSpace sId -> do
        mExists <- spaceExists actors sId
        pure $ case mExists of
          Nothing -> Unauthorized
          Just y -> ReadResponse $ if y then DoesExist else DoesNotExist
      ReadAllEntities sId -> do
        mY <- readEntities actors sId
        case mY of
          Nothing -> pure Unauthorized
          Just y -> ReadResponse . Entities <$> unfoldlMToVector y
      ReadEntity eId -> do
        mExists <- entityExists actors eId
        pure $ case mExists of
          Nothing -> Unauthorized
          Just y -> ReadResponse $ if y then DoesExist else DoesNotExist
      ReadAllVersions eId -> do
        mY <- readVersions actors eId
        case mY of
          Nothing -> pure Unauthorized
          Just y -> ReadResponse . Versions <$> unfoldlMToVector y
      ReadVersion vId -> do
        mExists <- versionExists actors vId
        pure $ case mExists of
          Nothing -> Unauthorized
          Just y -> ReadResponse $ if y then DoesExist else DoesNotExist
      ReadReferences vId -> do
        mY <- readReferences actors vId
        case mY of
          Nothing -> pure Unauthorized
          Just y -> ReadResponse . References <$> unfoldlMToVector y
      ReadReferencesOf vId -> do
        mY <- readReferencesFrom actors vId
        case mY of
          Nothing -> pure Unauthorized
          Just y -> ReadResponse . ReferencesOf <$> unfoldlMToVector y
      ReadSubscriptions vId -> do
        mY <- readSubscriptions actors vId
        case mY of
          Nothing -> pure Unauthorized
          Just y -> ReadResponse . Subscriptions <$> unfoldlMToVector y
      ReadSubscriptionsOf eId -> do
        mY <- readSubscriptionsFrom actors eId
        case mY of
          Nothing -> pure Unauthorized
          Just y -> ReadResponse . SubscriptionsOf <$> unfoldlMToVector y
      ReadForkOf eId -> do
        mY <- readForkOf actors eId
        case mY of
          Nothing -> pure Unauthorized
          Just y -> pure . ReadResponse $ ForkOf y
      ReadForkedBy vId -> do
        mY <- readForkedBy actors vId
        case mY of
          Nothing -> pure Unauthorized
          Just y -> ReadResponse . ForkedBy <$> unfoldlMToVector y
    goCreate CreateActor (Just (AnyIdActor aId)) = do
      worked <- storeActor actors aId
      pure $ if not worked then Unauthorized else CreateResponse (NewActor aId)
    goCreate CreateGroup (Just (AnyIdGroup gId)) = do
      worked <- storeGroup actors gId
      pure $ if not worked then Unauthorized else CreateResponse (NewGroup gId)
    goCreate (CreateMember gId aId) Nothing = do
      worked <- addMember actors gId aId
      pure $ if not worked then Unauthorized else Success
    goCreate CreateSpace (Just (AnyIdSpace sId)) = do
      worked <- storeSpace actors sId
      pure $ if not worked then Unauthorized else CreateResponse (NewSpace sId)
    goCreate (CreateEntity sId mFork) (Just (AnyIdEntity eId vId)) = do
      worked <- storeEntity actors eId sId vId mFork
      pure $ if not worked then Unauthorized else CreateResponse (NewEntity eId vId)
    goCreate (CreateVersion eId) (Just (AnyIdVersion vId)) = do
      worked <- storeNextVersion actors eId vId
      pure $ if not worked then Unauthorized else CreateResponse (NewVersion vId)
    goCreate x mId = error $ "Mismatched ID generation: " <> show x <> ", " <> show mId
      
    goUpdate = undefined
    goDelete = undefined


unfoldlMToVector :: Monad m => UnfoldlM.UnfoldlM m a -> m (Vector a)
unfoldlMToVector = UnfoldlM.foldlM' (\acc x -> pure (acc `V.snoc` x)) mempty
