module Lib.Api where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  Value (String),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson.Types (typeMismatch)
import Data.Data (Proxy (Proxy))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import GHC.Generics (Generic)
import Lib.Class (
  TerseDB (commit),
  actorExists,
  addMember,
  addReference,
  addSubscription,
  entityExists,
  groupExists,
  linkGroups,
  memberExists,
  moveEntity,
  offsetVersionIndex,
  readActors,
  readEntities,
  readEntityPermission,
  readForkOf,
  readForkedBy,
  readGroupPermission,
  readGroups,
  readMemberPermission,
  readMembers,
  readMembersOf,
  readNextGroups,
  readOrganizationPermission,
  readPrevGroup,
  readRecruiterPermission,
  readReferences,
  readReferencesFrom,
  readSpacePermission,
  readSpaces,
  readSubscriptions,
  readSubscriptionsFrom,
  readUniversePermission,
  readVersions,
  removeActor,
  removeEntity,
  removeGroup,
  removeMember,
  removeReference,
  removeSpace,
  removeSubscription,
  removeVersion,
  setEntityPermission,
  setGroupPermission,
  setMemberPermission,
  setOrganizationPermission,
  setRecruiterPermission,
  setSpacePermission,
  setUniversePermission,
  setVersionIndex,
  spaceExists,
  storeActor,
  storeEntity,
  storeGroup,
  storeNextVersion,
  storeSpace,
  unlinkGroups,
  updateFork,
  updateGroupParent,
  versionExists,
 )
import Lib.Types.Errors (UnauthorizedAction (UnauthorizedAction))
import Lib.Types.Id (ActorId, AnyId (..), EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission,
  CollectionPermissionWithExemption,
  SinglePermission,
 )
import System.Random.Stateful (Uniform (uniformM), globalStdGen)
import Test.QuickCheck (Arbitrary (arbitrary), oneof)

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
  | ReadUniversePermission GroupId
  | ReadOrganizationPermission GroupId
  | ReadRecruiterPermission GroupId
  | ReadSpacePermission GroupId SpaceId
  | ReadEntityPermission GroupId SpaceId
  | ReadGroupPermission GroupId GroupId
  | ReadMemberPermission GroupId GroupId
  deriving (Eq, Show, Read, Generic)
instance Arbitrary ReadAction where
  arbitrary =
    oneof
      [ pure ReadAllActors
      , ReadActor <$> arbitrary
      , pure ReadAllGroups
      , ReadGroup <$> arbitrary
      , ReadAllMembers <$> arbitrary
      , ReadAllMemberOf <$> arbitrary
      , ReadMember <$> arbitrary <*> arbitrary
      , ReadPrevGroup <$> arbitrary
      , ReadNextGroups <$> arbitrary
      , pure ReadAllSpaces
      , ReadSpace <$> arbitrary
      , ReadAllEntities <$> arbitrary
      , ReadEntity <$> arbitrary
      , ReadAllVersions <$> arbitrary
      , ReadVersion <$> arbitrary
      , ReadReferences <$> arbitrary
      , ReadReferencesOf <$> arbitrary
      , ReadSubscriptions <$> arbitrary
      , ReadSubscriptionsOf <$> arbitrary
      , ReadForkOf <$> arbitrary
      , ReadForkedBy <$> arbitrary
      , ReadUniversePermission <$> arbitrary
      , ReadOrganizationPermission <$> arbitrary
      , ReadRecruiterPermission <$> arbitrary
      , ReadSpacePermission <$> arbitrary <*> arbitrary
      , ReadEntityPermission <$> arbitrary <*> arbitrary
      , ReadGroupPermission <$> arbitrary <*> arbitrary
      , ReadMemberPermission <$> arbitrary <*> arbitrary
      ]
instance ToJSON ReadAction where
  toJSON x = case x of
    ReadAllActors -> String "a"
    ReadActor aId -> object ["a" .= aId]
    ReadAllGroups -> String "g"
    ReadGroup gId -> object ["g" .= gId]
    ReadAllMembers gId -> object ["m" .= gId]
    ReadAllMemberOf aId -> object ["m" .= aId]
    ReadMember gId aId -> object ["m" .= object ["g" .= gId, "a" .= aId]]
    ReadPrevGroup gId -> object ["g" .= object ["p" .= gId]]
    ReadNextGroups gId -> object ["g" .= object ["n" .= gId]]
    ReadAllSpaces -> String "s"
    ReadSpace sId -> object ["s" .= sId]
    ReadAllEntities sId -> object ["e" .= sId]
    ReadEntity eId -> object ["e" .= eId]
    ReadAllVersions eId -> object ["v" .= eId]
    ReadVersion vId -> object ["v" .= vId]
    ReadReferences vId -> object ["ref" .= vId]
    ReadReferencesOf vId -> object ["refOf" .= vId]
    ReadSubscriptions vId -> object ["sub" .= vId]
    ReadSubscriptionsOf eId -> object ["sub" .= eId]
    ReadForkOf eId -> object ["f" .= eId]
    ReadForkedBy vId -> object ["f" .= vId]
    ReadUniversePermission gId -> object ["u" .= gId]
    ReadOrganizationPermission gId -> object ["o" .= gId]
    ReadRecruiterPermission gId -> object ["r" .= gId]
    ReadSpacePermission gId sId -> object ["s" .= object ["g" .= gId, "s" .= sId]]
    ReadEntityPermission gId sId -> object ["e" .= object ["g" .= gId, "s" .= sId]]
    ReadGroupPermission gId gId' -> object ["g" .= object ["g" .= gId, "s" .= gId']]
    ReadMemberPermission gId gId' -> object ["m" .= object ["g" .= gId, "s" .= gId']]
instance FromJSON ReadAction where
  parseJSON (String t) = case t of
    "a" -> pure ReadAllActors
    "g" -> pure ReadAllGroups
    "s" -> pure ReadAllSpaces
    _ -> typeMismatch "ReadAction" (String t)
  parseJSON json = flip (withObject "ReadAction") json $ \o ->
    (ReadActor <$> o .: "a")
      <|> (ReadReferences <$> o .: "ref")
      <|> (ReadReferencesOf <$> o .: "refOf")
      <|> (ReadUniversePermission <$> o .: "u")
      <|> (ReadOrganizationPermission <$> o .: "o")
      <|> (ReadRecruiterPermission <$> o .: "r")
      <|> (onS =<< o .: "s")
      <|> (onG =<< o .: "g")
      <|> (onM =<< o .: "m")
      <|> (onE =<< o .: "e")
      <|> (onV =<< o .: "v")
      <|> (onSub =<< o .: "sub")
      <|> (onF =<< o .: "f")
   where
    onS json =
      (ReadSpace <$> parseJSON json)
        <|> ((\o -> ReadSpacePermission <$> o .: "g" <*> o .: "s") =<< parseJSON json)
    onG json =
      (ReadGroup <$> parseJSON json)
        <|> (onObj =<< parseJSON json)
     where
      onObj o =
        (ReadPrevGroup <$> o .: "p")
          <|> (ReadNextGroups <$> o .: "n")
          <|> (ReadGroupPermission <$> o .: "g" <*> o .: "s")
    onM json =
      (ReadAllMembers <$> parseJSON json)
        <|> (ReadAllMemberOf <$> parseJSON json)
        <|> (onObj =<< parseJSON json)
     where
      onObj o =
        (ReadMemberPermission <$> o .: "g" <*> o .: "s")
          <|> (ReadMember <$> o .: "g" <*> o .: "a")
    onE json =
      (ReadAllEntities <$> parseJSON json)
        <|> (ReadEntity <$> parseJSON json)
        <|> ((\o -> ReadEntityPermission <$> o .: "g" <*> o .: "s") =<< parseJSON json)
    onV json =
      (ReadAllVersions <$> parseJSON json)
        <|> (ReadVersion <$> parseJSON json)
    onSub json =
      (ReadSubscriptions <$> parseJSON json)
        <|> (ReadSubscriptionsOf <$> parseJSON json)
    onF json =
      (ReadForkOf <$> parseJSON json)
        <|> (ReadForkedBy <$> parseJSON json)

data CreateAction
  = CreateActor
  | CreateGroup
  | CreateMember GroupId ActorId
  | CreateSpace
  | CreateEntity SpaceId (Maybe VersionId)
  | CreateVersion EntityId
  deriving (Eq, Show, Read, Generic)
instance Arbitrary CreateAction where
  arbitrary =
    oneof
      [ pure CreateActor
      , pure CreateGroup
      , CreateMember <$> arbitrary <*> arbitrary
      , pure CreateSpace
      , CreateEntity <$> arbitrary <*> arbitrary
      , CreateVersion <$> arbitrary
      ]
instance ToJSON CreateAction where
  toJSON x = case x of
    CreateActor -> String "a"
    CreateGroup -> String "g"
    CreateMember gId aId -> object ["m" .= object ["g" .= gId, "a" .= aId]]
    CreateSpace -> String "s"
    CreateEntity sId mFork -> object ["e" .= object ["s" .= sId, "f" .= mFork]]
    CreateVersion eId -> object ["v" .= eId]
instance FromJSON CreateAction where
  parseJSON (String t) = case t of
    "a" -> pure CreateActor
    "g" -> pure CreateGroup
    "s" -> pure CreateSpace
    _ -> typeMismatch "CreateAction" (String t)
  parseJSON json = flip (withObject "CreateAction") json $ \o ->
    ((\o -> CreateMember <$> o .: "g" <*> o .: "a") =<< o .: "m")
      <|> ((\o -> CreateEntity <$> o .: "s" <*> o .: "f") =<< o .: "e")
      <|> (CreateVersion <$> o .: "v")

data StoreAction
  = StoreActor ActorId
  | StoreGroup GroupId
  | StoreMember GroupId ActorId
  | StoreSpace SpaceId
  | StoreEntity SpaceId (Maybe VersionId) EntityId VersionId
  | StoreVersion EntityId VersionId
  deriving (Eq, Show, Read, Generic)
instance Arbitrary StoreAction where
  arbitrary =
    oneof
      [ StoreActor <$> arbitrary
      , StoreGroup <$> arbitrary
      , StoreMember <$> arbitrary <*> arbitrary
      , StoreSpace <$> arbitrary
      , StoreEntity <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , StoreVersion <$> arbitrary <*> arbitrary
      ]
instance ToJSON StoreAction where
  toJSON x = case x of
    StoreActor aId -> object ["a" .= aId]
    StoreGroup gId -> object ["g" .= gId]
    StoreMember gId aId -> object ["m" .= object ["g" .= gId, "a" .= aId]]
    StoreSpace sId -> object ["s" .= sId]
    StoreEntity sId mFork eId vId -> object ["e" .= object ["s" .= sId, "f" .= mFork, "e" .= eId, "v" .= vId]]
    StoreVersion eId vId -> object ["v" .= object ["e" .= eId, "v" .= vId]]
instance FromJSON StoreAction where
  parseJSON = withObject "StoreAction" $ \o ->
    (StoreActor <$> o .: "a")
      <|> (StoreGroup <$> o .: "g")
      <|> ((\o -> StoreMember <$> o .: "g" <*> o .: "a") =<< o .: "m")
      <|> (StoreSpace <$> o .: "s")
      <|> ( (\o -> StoreEntity <$> o .: "s" <*> o .: "f" <*> o .: "e" <*> o .: "v")
              =<< o .: "e"
          )
      <|> ((\o -> StoreVersion <$> o .: "e" <*> o .: "v") =<< o .: "v")

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
  deriving (Eq, Show, Read, Generic)
instance Arbitrary UpdateAction where
  arbitrary =
    oneof
      [ AddReference <$> arbitrary <*> arbitrary
      , RemoveReference <$> arbitrary <*> arbitrary
      , AddSubscription <$> arbitrary <*> arbitrary
      , RemoveSubscription <$> arbitrary <*> arbitrary
      , ChangeFork <$> arbitrary <*> arbitrary
      , MoveEntity <$> arbitrary <*> arbitrary
      , OffsetVersion <$> arbitrary <*> arbitrary
      , SetVersionIndex <$> arbitrary <*> arbitrary
      , SetUniversePermission <$> arbitrary <*> arbitrary
      , SetOrganizationPermission <$> arbitrary <*> arbitrary
      , SetRecruiterPermission <$> arbitrary <*> arbitrary
      , SetSpacePermission <$> arbitrary <*> arbitrary <*> arbitrary
      , SetEntityPermission <$> arbitrary <*> arbitrary <*> arbitrary
      , SetGroupPermission <$> arbitrary <*> arbitrary <*> arbitrary
      , SetMemberPermission <$> arbitrary <*> arbitrary <*> arbitrary
      , LinkGroups <$> arbitrary <*> arbitrary
      , UnlinkGroups <$> arbitrary <*> arbitrary
      , UpdateGroupPrev <$> arbitrary <*> arbitrary
      ]
instance ToJSON UpdateAction where
  toJSON x = case x of
    AddReference vId ref -> object ["addR" .= object ["v" .= vId, "r" .= ref]]
    RemoveReference vId ref -> object ["rmR" .= object ["v" .= vId, "r" .= ref]]
    AddSubscription vId sub -> object ["addS" .= object ["v" .= vId, "s" .= sub]]
    RemoveSubscription vId sub -> object ["rmS" .= object ["v" .= vId, "s" .= sub]]
    ChangeFork eId mFork -> object ["chF" .= object ["e" .= eId, "f" .= mFork]]
    MoveEntity eId sId -> object ["mvE" .= object ["e" .= eId, "s" .= sId]]
    OffsetVersion vId offset -> object ["ofV" .= object ["v" .= vId, "o" .= offset]]
    SetVersionIndex vId idx -> object ["stV" .= object ["v" .= vId, "i" .= idx]]
    SetUniversePermission gId p -> object ["stU" .= object ["g" .= gId, "p" .= p]]
    SetOrganizationPermission gId p -> object ["stO" .= object ["g" .= gId, "p" .= p]]
    SetRecruiterPermission gId p -> object ["stR" .= object ["g" .= gId, "p" .= p]]
    SetSpacePermission gId sId mP -> object ["stS" .= object ["g" .= gId, "s" .= sId, "p" .= mP]]
    SetEntityPermission gId sId p -> object ["stE" .= object ["g" .= gId, "s" .= sId, "p" .= p]]
    SetGroupPermission gId gId' mP -> object ["stG" .= object ["g" .= gId, "s" .= gId', "p" .= mP]]
    SetMemberPermission gId gId' p -> object ["stM" .= object ["g" .= gId, "s" .= gId', "p" .= p]]
    LinkGroups gId gId' -> object ["lkG" .= object ["f" .= gId, "t" .= gId']]
    UnlinkGroups gId gId' -> object ["unG" .= object ["f" .= gId, "t" .= gId']]
    UpdateGroupPrev gId mPrev -> object ["pvG" .= object ["g" .= gId, "p" .= mPrev]]
instance FromJSON UpdateAction where
  parseJSON = withObject "UpdateAction" $ \o ->
    ((\o -> AddReference <$> o .: "v" <*> o .: "r") =<< o .: "addR")
      <|> ((\o -> RemoveReference <$> o .: "v" <*> o .: "r") =<< o .: "rmR")
      <|> ((\o -> AddSubscription <$> o .: "v" <*> o .: "s") =<< o .: "addS")
      <|> ((\o -> RemoveSubscription <$> o .: "v" <*> o .: "s") =<< o .: "rmS")
      <|> ((\o -> ChangeFork <$> o .: "e" <*> o .: "f") =<< o .: "chF")
      <|> ((\o -> MoveEntity <$> o .: "e" <*> o .: "s") =<< o .: "mvE")
      <|> ((\o -> OffsetVersion <$> o .: "v" <*> o .: "o") =<< o .: "ofV")
      <|> ((\o -> SetVersionIndex <$> o .: "v" <*> o .: "i") =<< o .: "stV")
      <|> ((\o -> SetUniversePermission <$> o .: "g" <*> o .: "p") =<< o .: "stU")
      <|> ((\o -> SetOrganizationPermission <$> o .: "g" <*> o .: "p") =<< o .: "stO")
      <|> ((\o -> SetRecruiterPermission <$> o .: "g" <*> o .: "p") =<< o .: "stR")
      <|> ( (\o -> SetSpacePermission <$> o .: "g" <*> o .: "s" <*> o .: "p") =<< o .: "stS"
          )
      <|> ( (\o -> SetEntityPermission <$> o .: "g" <*> o .: "s" <*> o .: "p")
              =<< o .: "stE"
          )
      <|> ( (\o -> SetGroupPermission <$> o .: "g" <*> o .: "s" <*> o .: "p") =<< o .: "stG"
          )
      <|> ( (\o -> SetMemberPermission <$> o .: "g" <*> o .: "s" <*> o .: "p")
              =<< o .: "stM"
          )
      <|> ((\o -> LinkGroups <$> o .: "f" <*> o .: "t") =<< o .: "lkG")
      <|> ((\o -> UnlinkGroups <$> o .: "f" <*> o .: "t") =<< o .: "unG")
      <|> ((\o -> UpdateGroupPrev <$> o .: "g" <*> o .: "p") =<< o .: "pvG")

data DeleteAction
  = DeleteVersion VersionId
  | DeleteEntity EntityId
  | DeleteSpace SpaceId
  | DeleteMember GroupId ActorId
  | DeleteGroup GroupId
  | DeleteActor ActorId
  deriving (Eq, Show, Read, Generic)
instance Arbitrary DeleteAction where
  arbitrary =
    oneof
      [ DeleteVersion <$> arbitrary
      , DeleteEntity <$> arbitrary
      , DeleteSpace <$> arbitrary
      , DeleteMember <$> arbitrary <*> arbitrary
      , DeleteGroup <$> arbitrary
      , DeleteActor <$> arbitrary
      ]
instance ToJSON DeleteAction where
  toJSON x = case x of
    DeleteVersion y -> object ["v" .= y]
    DeleteEntity y -> object ["e" .= y]
    DeleteSpace y -> object ["s" .= y]
    DeleteMember y z -> object ["m" .= object ["g" .= y, "a" .= z]]
    DeleteGroup y -> object ["g" .= y]
    DeleteActor y -> object ["a" .= y]
instance FromJSON DeleteAction where
  parseJSON = withObject "DeleteAction" $ \o ->
    (DeleteVersion <$> o .: "v")
      <|> (DeleteEntity <$> o .: "e")
      <|> (DeleteSpace <$> o .: "s")
      <|> ((\o -> DeleteMember <$> o .: "g" <*> o .: "a") =<< o .: "m")
      <|> (DeleteGroup <$> o .: "g")
      <|> (DeleteActor <$> o .: "a")

data Action
  = ReadAction ReadAction
  | CreateAction CreateAction
  | UpdateAction UpdateAction
  | DeleteAction DeleteAction
  deriving (Eq, Show, Read, Generic)
instance Arbitrary Action where
  arbitrary =
    oneof
      [ ReadAction <$> arbitrary
      , CreateAction <$> arbitrary
      , UpdateAction <$> arbitrary
      , DeleteAction <$> arbitrary
      ]
instance ToJSON Action where
  toJSON x = case x of
    ReadAction y -> object ["r" .= y]
    CreateAction y -> object ["c" .= y]
    UpdateAction y -> object ["u" .= y]
    DeleteAction y -> object ["d" .= y]
instance FromJSON Action where
  parseJSON = withObject "Action" $ \o ->
    (ReadAction <$> o .: "r")
      <|> (CreateAction <$> o .: "c")
      <|> (UpdateAction <$> o .: "u")
      <|> (DeleteAction <$> o .: "d")

isMutable :: Action -> Bool
isMutable x = case x of
  CreateAction _ -> True
  UpdateAction _ -> True
  DeleteAction _ -> True
  _ -> False

isCreate :: Action -> Bool
isCreate x = case x of
  CreateAction _ -> True
  _ -> False

data MutableAction
  = StoreMutableAction StoreAction
  | UpdateMutableAction UpdateAction
  | DeleteMutableAction DeleteAction
  deriving (Eq, Show, Read)
instance Arbitrary MutableAction where
  arbitrary =
    oneof
      [ StoreMutableAction <$> arbitrary
      , UpdateMutableAction <$> arbitrary
      , DeleteMutableAction <$> arbitrary
      ]
instance ToJSON MutableAction where
  toJSON x = case x of
    StoreMutableAction y -> object ["s" .= y]
    UpdateMutableAction y -> object ["u" .= y]
    DeleteMutableAction y -> object ["d" .= y]
instance FromJSON MutableAction where
  parseJSON = withObject "MutableAction" $ \o ->
    (StoreMutableAction <$> o .: "s")
      <|> (UpdateMutableAction <$> o .: "u")
      <|> (DeleteMutableAction <$> o .: "d")

toMutate :: Action -> Maybe AnyId -> Maybe MutableAction
toMutate action mId = case action of
  ReadAction _ -> Nothing
  CreateAction x ->
    StoreMutableAction <$> case (x, mId) of
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
  | PermissionWithExemption CollectionPermissionWithExemption
  | Permission CollectionPermission
  deriving (Eq, Show, Read)
instance Arbitrary ReadResponse where
  arbitrary =
    oneof
      [ pure DoesExist
      , pure DoesNotExist
      , Actors <$> arbitrary
      , Groups <$> arbitrary
      , Members <$> arbitrary
      , MemberOf <$> arbitrary
      , PrevGroup <$> arbitrary
      , NextGroups <$> arbitrary
      , Spaces <$> arbitrary
      , Entities <$> arbitrary
      , Versions <$> arbitrary
      , References <$> arbitrary
      , ReferencesOf <$> arbitrary
      , Subscriptions <$> arbitrary
      , SubscriptionsOf <$> arbitrary
      , ForkOf <$> arbitrary
      , ForkedBy <$> arbitrary
      , PermissionWithExemption <$> arbitrary
      , Permission <$> arbitrary
      ]
instance ToJSON ReadResponse where
  toJSON x = case x of
    DoesExist -> String "y"
    DoesNotExist -> String "n"
    Actors as -> object ["a" .= as]
    Groups gs -> object ["g" .= gs]
    Members as -> object ["m" .= as]
    MemberOf gs -> object ["m" .= gs]
    PrevGroup mG -> object ["g" .= object ["p" .= mG]]
    NextGroups gs -> object ["g" .= object ["n" .= gs]]
    Spaces ss -> object ["s" .= ss]
    Entities es -> object ["e" .= es]
    Versions vs -> object ["v" .= vs]
    References vs -> object ["ref" .= vs]
    ReferencesOf vs -> object ["refOf" .= vs]
    Subscriptions es -> object ["sub" .= es]
    SubscriptionsOf vs -> object ["sub" .= vs]
    ForkOf mV -> object ["f" .= mV]
    ForkedBy es -> object ["f" .= es]
    PermissionWithExemption p -> object ["p" .= p]
    Permission p -> object ["p" .= p]
instance FromJSON ReadResponse where
  parseJSON (String t) = case t of
    "y" -> pure DoesExist
    "n" -> pure DoesNotExist
    _ -> typeMismatch "ReadResponse" (String t)
  parseJSON json = flip (withObject "ReadResponse") json $ \o ->
    (Actors <$> o .: "a")
      <|> (Actors <$> o .: "a")
      <|> (Spaces <$> o .: "s")
      <|> (Entities <$> o .: "e")
      <|> (Versions <$> o .: "v")
      <|> (References <$> o .: "ref")
      <|> (ReferencesOf <$> o .: "refOf")
      <|> (goG =<< o .: "g")
      <|> (goM =<< o .: "m")
      <|> (goSub =<< o .: "sub")
      <|> (goF =<< o .: "f")
      <|> (goP =<< o .: "p")
   where
    goG json =
      (Groups <$> parseJSON json)
        <|> ((\o -> PrevGroup <$> o .: "p") =<< parseJSON json)
        <|> ((\o -> NextGroups <$> o .: "n") =<< parseJSON json)
    goM json =
      (Members <$> parseJSON json)
        <|> (MemberOf <$> parseJSON json)
    goSub json =
      (Subscriptions <$> parseJSON json)
        <|> (SubscriptionsOf <$> parseJSON json)
    goF json =
      (ForkOf <$> parseJSON json)
        <|> (ForkedBy <$> parseJSON json)
    goP json =
      (PermissionWithExemption <$> parseJSON json)
        <|> (Permission <$> parseJSON json)

data CreateResponse
  = NewActor ActorId
  | NewGroup GroupId
  | NewSpace SpaceId
  | NewEntity EntityId VersionId
  | NewVersion VersionId
  deriving (Eq, Show, Read)
instance Arbitrary CreateResponse where
  arbitrary =
    oneof
      [ NewActor <$> arbitrary
      , NewGroup <$> arbitrary
      , NewSpace <$> arbitrary
      , NewEntity <$> arbitrary <*> arbitrary
      , NewVersion <$> arbitrary
      ]
instance ToJSON CreateResponse where
  toJSON x = case x of
    NewActor aId -> object ["a" .= aId]
    NewGroup gId -> object ["g" .= gId]
    NewSpace sId -> object ["s" .= sId]
    NewEntity eId vId -> object ["e" .= object ["e" .= eId, "v" .= vId]]
    NewVersion vId -> object ["v" .= vId]
instance FromJSON CreateResponse where
  parseJSON = withObject "CreateResponse" $ \o ->
    (NewActor <$> o .: "a")
      <|> (NewGroup <$> o .: "g")
      <|> (NewSpace <$> o .: "s")
      <|> ((\o -> NewEntity <$> o .: "e" <*> o .: "v") =<< o .: "e")
      <|> (NewVersion <$> o .: "v")

toCreateResponse
  :: CreateAction -> Maybe AnyId -> Maybe (Either () CreateResponse)
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
instance Arbitrary Response where
  arbitrary =
    oneof
      [ ReadResponse <$> arbitrary
      , CreateResponse <$> arbitrary
      ]
instance ToJSON Response where
  toJSON x = case x of
    ReadResponse y -> object ["r" .= y]
    CreateResponse y -> object ["c" .= y]
instance FromJSON Response where
  parseJSON = withObject "Response" $ \o ->
    (ReadResponse <$> o .: "r")
      <|> (CreateResponse <$> o .: "c")

data Authorize a
  = Authorized a
  | Unauthorized
  deriving (Functor, Generic, Show, Read, Eq)
instance (Arbitrary a) => Arbitrary (Authorize a) where
  arbitrary =
    oneof
      [ pure Unauthorized
      , Authorized <$> arbitrary
      ]
instance (ToJSON a) => ToJSON (Authorize a) where
  toJSON x = case x of
    Unauthorized -> String "x"
    Authorized y -> object ["x" .= y]
instance (FromJSON a) => FromJSON (Authorize a) where
  parseJSON (String t) = case t of
    "x" -> pure Unauthorized
    _ -> typeMismatch "Authorize" (String t)
  parseJSON json = flip (withObject "Authorize") json $ \o ->
    Authorized <$> o .: "x"

-- | If one action is unauthorized, continue
actMany
  :: forall m n
   . (TerseDB n m, MonadIO n)
  => Proxy m
  -> (m ()) -- ^ Conditional action to perform when mutating
  -> (MutableAction -> n ())
  -- ^ Each mutation performed that's authorized
  -> NonEmpty ActorId
  -> [Action]
  -> n [Authorize (Maybe Response)]
actMany Proxy beforeMutate onAuthorized actors xs = do
  ids <- traverse genId xs
  let ys :: m [Authorize (Maybe Response, Maybe MutableAction)]
      ys = traverse go (zip xs ids)
       where
        go
          :: (Action, Maybe AnyId) -> m (Authorize (Maybe Response, Maybe MutableAction))
        go (x, mId) = makeResponse actors x mId
  rs <- commit $ do
    when (any isMutable xs) $
      beforeMutate
    ys
  for rs $ \mAuth -> do
    case mAuth of
      Authorized (_, Just action') -> onAuthorized action'
      _ -> pure ()
    pure (fst <$> mAuth)

act
  :: forall m n
   . (TerseDB n m, MonadIO n)
  => Proxy m
  -> (m ())
  -> (MutableAction -> n ())
  -> NonEmpty ActorId
  -> Action
  -> n (Authorize (Maybe Response))
act Proxy beforeMutate onAuthorized actors x = do
  mId <- genId x
  let y :: m (Authorize (Maybe Response, Maybe MutableAction))
      y = makeResponse actors x mId
  mAuth <- commit $ do
    when (isMutable x) $
      beforeMutate
    y
  case mAuth of
    Authorized (_, Just action') -> onAuthorized action'
    _ -> pure ()
  pure (fst <$> mAuth)

-- | If one action is unauthorized, abandon all transactions
actManyStrict
  :: forall m n
   . (TerseDB n m, MonadIO n, MonadThrow m)
  => Proxy m
  -> (m ()) -- ^ Conditional action to perform when mutating
  -> ([MutableAction] -> n ())
  -- ^ Mutations performed, in the same order as the actions supplied
  -> NonEmpty ActorId
  -> [Action]
  -> n [Maybe Response]
actManyStrict Proxy beforeMutate onAuthorized actors xs = do
  ids <- traverse genId xs
  let ys :: m [(Maybe Response, Maybe MutableAction)]
      ys = traverse go (zip xs ids)
       where
        go :: (Action, Maybe AnyId) -> m (Maybe Response, Maybe MutableAction)
        go (x, mId) = do
          mAuth <- makeResponse actors x mId
          case mAuth of
            Unauthorized -> throwM UnauthorizedAction
            Authorized y -> pure y
  (rs, as) <- fmap unzip . commit $ do
    when (any isMutable xs) $
      beforeMutate
    ys
  onAuthorized (catMaybes as)
  pure rs

actStrict
  :: forall m n
   . (TerseDB n m, MonadIO n, MonadThrow m)
  => Proxy m
  -> (m ())
  -> (MutableAction -> n ())
  -> NonEmpty ActorId
  -> Action
  -> n (Maybe Response)
actStrict Proxy beforeMutate onAuthorized actors x = do
  mId <- genId x
  let y :: m ((Maybe Response, Maybe MutableAction))
      y = do
        mAuth <- makeResponse actors x mId
        case mAuth of
          Unauthorized -> throwM UnauthorizedAction
          Authorized z -> pure z
  (r, mA) <- commit $ do
    when (isMutable x) $
      beforeMutate
    y
  case mA of
    Nothing -> pure ()
    Just a -> onAuthorized a
  pure r

genId :: forall n. (MonadIO n) => Action -> n (Maybe AnyId)
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
  gen :: forall a. (Uniform a) => n a
  gen = uniformM globalStdGen

mutate
  :: forall m n
   . (TerseDB n m)
  => NonEmpty ActorId
  -> MutableAction
  -> m (Authorize ())
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
    DeleteActor aId -> removeActor actors aId
    DeleteGroup gId -> removeGroup actors gId
    DeleteMember gId aId -> removeMember actors gId aId
    DeleteSpace sId -> removeSpace actors sId
    DeleteEntity eId -> removeEntity actors eId
    DeleteVersion vId -> removeVersion actors vId

makeResponse
  :: forall m n
   . (TerseDB n m)
  => NonEmpty ActorId
  -> Action
  -> Maybe AnyId
  -> m (Authorize (Maybe Response, Maybe MutableAction))
makeResponse actors action mId = case action of
  ReadAction x -> fmap (,Nothing) <$> goRead x
  _ -> case toMutate action mId of
    Just action' ->
      let mkResponse :: () -> Maybe Response
          mkResponse () = case action of
            CreateAction x -> case toCreateResponse x mId of
              Nothing -> error $ "Couldn't make CreateResponse: " <> show x <> ", " <> show mId
              Just (Left ()) -> Nothing
              Just (Right y) -> Just (CreateResponse y)
            _ -> Nothing
       in fmap ((,Just action') . mkResponse) <$> mutate actors action'
    _ ->
      error $
        "Couldn't generate mutatable action: " <> show action <> ", id: " <> show mId
 where
  goRead x = do
    (res :: Authorize ReadResponse) <-
      maybeToAuthorized <$> case x of
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
          traverse (fmap ReferencesOf . unfoldlMToVector)
            =<< readReferencesFrom actors vId
        ReadSubscriptions vId ->
          traverse (fmap Subscriptions . unfoldlMToVector)
            =<< readSubscriptions actors vId
        ReadSubscriptionsOf eId ->
          traverse (fmap SubscriptionsOf . unfoldlMToVector)
            =<< readSubscriptionsFrom actors eId
        ReadForkOf eId ->
          fmap ForkOf <$> readForkOf actors eId
        ReadForkedBy vId ->
          traverse (fmap ForkedBy . unfoldlMToVector) =<< readForkedBy actors vId
        ReadUniversePermission gId ->
          fmap PermissionWithExemption <$> readUniversePermission actors gId
        ReadOrganizationPermission gId ->
          fmap PermissionWithExemption <$> readOrganizationPermission actors gId
        ReadRecruiterPermission gId ->
          fmap Permission <$> readRecruiterPermission actors gId
        ReadSpacePermission gId sId ->
          fmap Permission <$> readSpacePermission actors gId sId
        ReadEntityPermission gId sId ->
          fmap Permission <$> readEntityPermission actors gId sId
        ReadGroupPermission gId gId' ->
          fmap Permission <$> readGroupPermission actors gId gId'
        ReadMemberPermission gId gId' ->
          fmap Permission <$> readMemberPermission actors gId gId'
    pure $ Just . ReadResponse <$> res

unfoldlMToVector :: (Monad m) => UnfoldlM.UnfoldlM m a -> m (Vector a)
unfoldlMToVector = UnfoldlM.foldlM' (\acc x -> pure (acc `V.snoc` x)) mempty

maybeToAuthorized :: Maybe a -> Authorize a
maybeToAuthorized Nothing = Unauthorized
maybeToAuthorized (Just x) = Authorized x

boolToExists :: Bool -> ReadResponse
boolToExists False = DoesNotExist
boolToExists True = DoesExist
