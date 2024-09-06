module Lib.Api.Response.Read where

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
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Lib.Types.Permission (
  CollectionPermission,
  CollectionPermissionWithExemption,
  SinglePermission,
 )
import Test.QuickCheck (Arbitrary (arbitrary), oneof)

data ReadResponse
  = DoesExist
  | DoesNotExist
  | Actors (Vector ActorId)
  | Groups (Vector GroupId)
  | Spaces (Vector SpaceId)
  | Entities (Vector EntityId)
  | Versions (Vector VersionId)
  | PermissionWithExemption CollectionPermissionWithExemption
  | Permission CollectionPermission
  | GroupPermission (Maybe (Either SinglePermission CollectionPermission))
  | PermissionsForSpaces (Vector (SpaceId, SinglePermission))
  | PermissionsForEntities (Vector (SpaceId, CollectionPermission))
  | PermissionsForGroups (Vector (GroupId, SinglePermission))
  | PermissionsForMembers (Vector (GroupId, CollectionPermission))
  deriving (Eq, Show, Read, Generic)
instance Arbitrary ReadResponse where
  arbitrary =
    oneof
      [ pure DoesExist
      , pure DoesNotExist
      , Actors <$> arbitrary
      , Groups <$> arbitrary
      , Spaces <$> arbitrary
      , Entities <$> arbitrary
      , Versions <$> arbitrary
      , PermissionWithExemption <$> arbitrary
      , Permission <$> arbitrary
      , GroupPermission <$> arbitrary
      , PermissionsForSpaces <$> arbitrary
      , PermissionsForEntities <$> arbitrary
      , PermissionsForGroups <$> arbitrary
      , PermissionsForMembers <$> arbitrary
      ]
instance ToJSON ReadResponse where
  toJSON x = case x of
    DoesExist -> String "y"
    DoesNotExist -> String "n"
    Actors as -> object ["a" .= as]
    Groups gs -> object ["g" .= gs]
    Spaces ss -> object ["s" .= ss]
    Entities es -> object ["e" .= es]
    Versions vs -> object ["v" .= vs]
    PermissionWithExemption p -> object ["p" .= p]
    Permission p -> object ["p" .= p]
    GroupPermission Nothing -> object ["gp" .= (Nothing :: Maybe ())]
    GroupPermission (Just eX) -> object ["gp" .= either toJSON toJSON eX]
    PermissionsForSpaces ps -> object ["ps" .= object ["s" .= ps]]
    PermissionsForEntities ps -> object ["ps" .= object ["e" .= ps]]
    PermissionsForGroups ps -> object ["ps" .= object ["g" .= ps]]
    PermissionsForMembers ps -> object ["ps" .= object ["m" .= ps]]
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
      <|> (Groups <$> o .: "g")
      <|> (goP =<< o .: "p")
      <|> (goGP =<< o .: "gp")
      <|> (goPS =<< o .: "ps")
   where
    goP json =
      (PermissionWithExemption <$> parseJSON json)
        <|> (Permission <$> parseJSON json)
    goGP json =
      ( do
          (Nothing :: Maybe ()) <- parseJSON json
          pure (GroupPermission Nothing)
      )
        <|> (GroupPermission . Just . Left <$> parseJSON json)
        <|> (GroupPermission . Just . Right <$> parseJSON json)
    goPS o =
      (PermissionsForSpaces <$> o .: "s")
        <|> (PermissionsForEntities <$> o .: "e")
        <|> (PermissionsForGroups <$> o .: "g")
        <|> (PermissionsForMembers <$> o .: "m")
