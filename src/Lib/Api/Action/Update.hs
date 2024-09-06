module Lib.Api.Action.Update where

import Lib.Types.Id (GroupId, SpaceId, VersionId, EntityId)
import Lib.Types.Permission (CollectionPermissionWithExemption, CollectionPermission, SinglePermission)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary), oneof)
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), object, withObject, (.:), (.=))
import Control.Applicative ((<|>))

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
