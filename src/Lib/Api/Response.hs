module Lib.Api.Response where

import Control.Applicative ((<|>))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (String),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson.Types (typeMismatch)
import Data.List.NonEmpty (NonEmpty)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import GHC.Generics (Generic)
import Lib.Api.Action (Action (..), MutableAction (..), toMutate)
import Lib.Api.Action.Delete (DeleteAction (..))
import Lib.Api.Action.Read (ReadAction (..))
import Lib.Api.Action.Store (StoreAction (..))
import Lib.Api.Action.Update (UpdateAction (..))
import Lib.Api.Response.Create (CreateResponse, toCreateResponse)
import Lib.Api.Response.Read (ReadResponse (..))
import Lib.Class (
  TerseDB,
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
  readAllEntityPermission,
  readAllGroupPermission,
  readAllMemberPermission,
  readAllSpacePermission,
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
  readRootGroups,
  readSpacePermission,
  readSpaces,
  readSubscriptions,
  readSubscriptionsFrom,
  readTabEntityPermission,
  readTabGroupPermission,
  readTabMemberPermission,
  readTabOrganizationPermission,
  readTabRecruiterPermission,
  readTabSpacePermission,
  readTabUniversePermission,
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
import Lib.Types.Id (ActorId, AnyId)
import Test.QuickCheck (Arbitrary (arbitrary), oneof)

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
          traverse (fmap Actors . unfoldlMToVector) =<< readMembers actors gId
        ReadAllMemberOf aId ->
          traverse (fmap Groups . unfoldlMToVector) =<< readMembersOf actors aId
        ReadMember gId aId ->
          fmap boolToExists <$> memberExists actors gId aId
        ReadRootGroups ->
          Just . Groups <$> (unfoldlMToVector =<< readRootGroups actors)
        ReadPrevGroup gId ->
          fmap (Groups . maybe V.empty V.singleton) <$> readPrevGroup actors gId
        ReadNextGroups gId ->
          traverse (fmap Groups . unfoldlMToVector) =<< readNextGroups actors gId
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
          traverse (fmap Versions . unfoldlMToVector) =<< readReferences actors vId
        ReadReferencesOf vId ->
          traverse (fmap Versions . unfoldlMToVector)
            =<< readReferencesFrom actors vId
        ReadSubscriptions vId ->
          traverse (fmap Entities . unfoldlMToVector)
            =<< readSubscriptions actors vId
        ReadSubscriptionsOf eId ->
          traverse (fmap Versions . unfoldlMToVector)
            =<< readSubscriptionsFrom actors eId
        ReadForkOf eId ->
          fmap (Versions . maybe V.empty V.singleton) <$> readForkOf actors eId
        ReadForkedBy vId ->
          traverse (fmap Entities . unfoldlMToVector) =<< readForkedBy actors vId
        ReadUniversePermission gId ->
          fmap PermissionWithExemption <$> readUniversePermission actors gId
        ReadOrganizationPermission gId ->
          fmap PermissionWithExemption <$> readOrganizationPermission actors gId
        ReadRecruiterPermission gId ->
          fmap Permission <$> readRecruiterPermission actors gId
        ReadSpacePermission gId sId ->
          fmap (GroupPermission . fmap Left) <$> readSpacePermission actors gId sId
        ReadEntityPermission gId sId ->
          fmap (GroupPermission . fmap Right) <$> readEntityPermission actors gId sId
        ReadGroupPermission gId gId' ->
          fmap (GroupPermission . fmap Left) <$> readGroupPermission actors gId gId'
        ReadMemberPermission gId gId' ->
          fmap (GroupPermission . fmap Right) <$> readMemberPermission actors gId gId'
        ReadTabUniversePermission gId ->
          fmap PermissionWithExemption <$> readTabUniversePermission actors gId
        ReadTabOrganizationPermission gId ->
          fmap PermissionWithExemption <$> readTabOrganizationPermission actors gId
        ReadTabRecruiterPermission gId ->
          fmap Permission <$> readTabRecruiterPermission actors gId
        ReadTabSpacePermission gId sId ->
          fmap Permission <$> readTabSpacePermission actors gId sId
        ReadTabEntityPermission gId sId ->
          fmap Permission <$> readTabEntityPermission actors gId sId
        ReadTabGroupPermission gId gId' ->
          fmap Permission <$> readTabGroupPermission actors gId gId'
        ReadTabMemberPermission gId gId' ->
          fmap Permission <$> readTabMemberPermission actors gId gId'
        ReadAllSpacePermission gId ->
          traverse (fmap PermissionsForSpaces . unfoldlMToVector)
            =<< readAllSpacePermission actors gId
        ReadAllEntityPermission gId ->
          traverse (fmap PermissionsForEntities . unfoldlMToVector)
            =<< readAllEntityPermission actors gId
        ReadAllGroupPermission gId ->
          traverse (fmap PermissionsForGroups . unfoldlMToVector)
            =<< readAllGroupPermission actors gId
        ReadAllMemberPermission gId ->
          traverse (fmap PermissionsForMembers . unfoldlMToVector)
            =<< readAllMemberPermission actors gId
    pure $ Just . ReadResponse <$> res

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

maybeToAuthorized :: Maybe a -> Authorize a
maybeToAuthorized Nothing = Unauthorized
maybeToAuthorized (Just x) = Authorized x

boolToExists :: Bool -> ReadResponse
boolToExists False = DoesNotExist
boolToExists True = DoesExist

unfoldlMToVector :: (Monad m) => UnfoldlM.UnfoldlM m a -> m (Vector a)
unfoldlMToVector = UnfoldlM.foldlM' (\acc x -> pure (acc `V.snoc` x)) mempty
