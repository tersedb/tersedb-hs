module Lib.Api.Action.Delete where

import Control.Applicative ((<|>))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  object,
  withObject,
  (.:),
  (.=),
 )
import GHC.Generics (Generic)
import Lib.Types.Id (ActorId, EntityId, GroupId, SpaceId, VersionId)
import Test.QuickCheck (Arbitrary (arbitrary), oneof)

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
