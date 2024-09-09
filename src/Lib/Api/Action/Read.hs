module Lib.Api.Action.Read where

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
import GHC.Generics (Generic)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Test.QuickCheck (Arbitrary (arbitrary), oneof)

data ReadAction
  = ReadAllActors
  | ReadActor ActorId
  | ReadAllGroups
  | ReadGroup GroupId
  | ReadAllMembers GroupId
  | ReadAllMemberOf ActorId
  | ReadMember GroupId ActorId
  | ReadRootGroups
  | ReadPrevGroup GroupId
  | ReadNextGroups GroupId
  | ReadAllSpaces
  | ReadSpace SpaceId
  | ReadAllEntities SpaceId
  | ReadEntity EntityId
  | ReadEntitySpace EntityId
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
  | ReadAllSpacePermission GroupId
  | ReadAllEntityPermission GroupId
  | ReadAllGroupPermission GroupId
  | ReadAllMemberPermission GroupId
  | ReadTabUniversePermission GroupId
  | ReadTabOrganizationPermission GroupId
  | ReadTabRecruiterPermission GroupId
  | ReadTabSpacePermission GroupId SpaceId
  | ReadTabEntityPermission GroupId SpaceId
  | ReadTabGroupPermission GroupId GroupId
  | ReadTabMemberPermission GroupId GroupId
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
      , pure ReadRootGroups
      , ReadPrevGroup <$> arbitrary
      , ReadNextGroups <$> arbitrary
      , pure ReadAllSpaces
      , ReadSpace <$> arbitrary
      , ReadAllEntities <$> arbitrary
      , ReadEntity <$> arbitrary
      , ReadEntitySpace <$> arbitrary
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
      , ReadAllSpacePermission <$> arbitrary
      , ReadAllEntityPermission <$> arbitrary
      , ReadAllGroupPermission <$> arbitrary
      , ReadAllMemberPermission <$> arbitrary
      , ReadTabUniversePermission <$> arbitrary
      , ReadTabOrganizationPermission <$> arbitrary
      , ReadTabRecruiterPermission <$> arbitrary
      , ReadTabSpacePermission <$> arbitrary <*> arbitrary
      , ReadTabEntityPermission <$> arbitrary <*> arbitrary
      , ReadTabGroupPermission <$> arbitrary <*> arbitrary
      , ReadTabMemberPermission <$> arbitrary <*> arbitrary
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
    ReadRootGroups -> String "rg"
    ReadPrevGroup gId -> object ["g" .= object ["p" .= gId]]
    ReadNextGroups gId -> object ["g" .= object ["n" .= gId]]
    ReadAllSpaces -> String "s"
    ReadSpace sId -> object ["s" .= sId]
    ReadAllEntities sId -> object ["e" .= sId]
    ReadEntity eId -> object ["e" .= eId]
    ReadEntitySpace eId -> object ["e" .= object ["s" .= eId]]
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
    ReadAllSpacePermission gId -> object ["p" .= object ["s" .= gId]]
    ReadAllEntityPermission gId -> object ["p" .= object ["e" .= gId]]
    ReadAllGroupPermission gId -> object ["p" .= object ["g" .= gId]]
    ReadAllMemberPermission gId -> object ["p" .= object ["m" .= gId]]
    ReadTabUniversePermission gId ->
      object ["t" .= object ["u" .= gId]]
    ReadTabOrganizationPermission gId ->
      object ["t" .= object ["o" .= gId]]
    ReadTabRecruiterPermission gId ->
      object ["t" .= object ["r" .= gId]]
    ReadTabSpacePermission gId sId ->
      object ["t" .= object ["s" .= object ["g" .= gId, "s" .= sId]]]
    ReadTabEntityPermission gId sId ->
      object ["t" .= object ["e" .= object ["g" .= gId, "s" .= sId]]]
    ReadTabGroupPermission gId sId ->
      object ["t" .= object ["g" .= object ["g" .= gId, "s" .= sId]]]
    ReadTabMemberPermission gId sId ->
      object ["t" .= object ["m" .= object ["g" .= gId, "s" .= sId]]]
instance FromJSON ReadAction where
  parseJSON (String t) = case t of
    "a" -> pure ReadAllActors
    "g" -> pure ReadAllGroups
    "s" -> pure ReadAllSpaces
    "rg" -> pure ReadRootGroups
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
      <|> (onT =<< o .: "t")
      <|> (onP =<< o .: "p")
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
        <|> ((\o -> ReadEntitySpace <$> o .: "s") =<< parseJSON json)
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
    onT o =
      (ReadTabUniversePermission <$> o .: "u")
        <|> (ReadTabOrganizationPermission <$> o .: "o")
        <|> (ReadTabRecruiterPermission <$> o .: "r")
        <|> ((\o -> ReadTabSpacePermission <$> o .: "g" <*> o .: "s") =<< o .: "s")
        <|> ((\o -> ReadTabEntityPermission <$> o .: "g" <*> o .: "s") =<< o .: "e")
        <|> ((\o -> ReadTabGroupPermission <$> o .: "g" <*> o .: "s") =<< o .: "g")
        <|> ((\o -> ReadTabMemberPermission <$> o .: "g" <*> o .: "s") =<< o .: "m")
    onP o =
      (ReadAllSpacePermission <$> o .: "s")
        <|> (ReadAllEntityPermission <$> o .: "e")
        <|> (ReadAllGroupPermission <$> o .: "g")
        <|> (ReadAllMemberPermission <$> o .: "m")
