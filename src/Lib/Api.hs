module Lib.Api where

import Data.Vector (Vector)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission,
  CollectionPermissionWithExemption,
  SinglePermission,
 )
import Data.List.NonEmpty (NonEmpty)
import Lib.Class (TerseDB (anyCanReadActor), readEntities, actorExists, readReferences, readSubscriptions, readSubscriptionsFrom, readReferencesFrom, readPrevGroup)
import qualified Data.Vector as V
import qualified DeferredFolds.UnfoldlM as UnfoldlM

data ReadAction
  = ReadAllActors
  | ReadActor ActorId
  | ReadAllGroups
  | ReadGroup GroupId
  | ReadAllMembers ActorId
  | ReadAllMemberOf GroupId
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
  | ReadFork EntityId
  | ReadForksOf VersionId
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
  | Fork (Maybe VersionId)
  | ForksOf (Vector EntityId)
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


act :: (TerseDB n m) => NonEmpty ActorId -> [Action] -> m [Response]
act actors = traverse go
  where
    go action = case action of
      ReadAction x -> goRead x
      CreateAction x -> goCreate x
      UpdateAction x -> goUpdate x
      DeleteAction x -> goDelete x
    goRead x = case x of
      ReadAllActors -> undefined
      ReadActor aId -> do
        mExists <- actorExists actors aId
        pure $ case mExists of
          Nothing -> Unauthorized
          Just y -> ReadResponse $ if y then DoesExist else DoesNotExist
      ReadAllGroups -> undefined
      ReadGroup gId -> undefined
      ReadAllMembers aId -> undefined
      ReadAllMemberOf gId -> undefined
      ReadMember gId aId -> undefined
      ReadPrevGroup gId -> do
        mMPrev <- readPrevGroup actors gId
        pure $ case mMPrev of
          Nothing -> Unauthorized
          Just mPrev -> ReadResponse (PrevGroup mPrev)
      ReadNextGroups gId -> undefined
      ReadAllSpaces -> undefined
      ReadSpace sId -> undefined
      ReadAllEntities sId -> do
        mY <- readEntities actors sId
        case mY of
          Nothing -> pure Unauthorized
          Just y -> ReadResponse . Entities <$> unfoldlMToVector y
      ReadEntity eId -> undefined
      ReadAllVersions eId -> undefined
      ReadVersion vId -> undefined
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
      ReadFork eId -> undefined
      ReadForksOf vId -> undefined
    goCreate = undefined
    goUpdate = undefined
    goDelete = undefined


unfoldlMToVector :: Monad m => UnfoldlM.UnfoldlM m a -> m (Vector a)
unfoldlMToVector = UnfoldlM.foldlM' (\acc x -> pure (acc `V.snoc` x)) mempty
