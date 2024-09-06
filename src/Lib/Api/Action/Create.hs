module Lib.Api.Action.Create where

import Lib.Types.Id (GroupId, ActorId, SpaceId, VersionId, EntityId)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary), oneof)
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), Value (String), object, withObject, (.:), (.=))
import Data.Aeson.Types (typeMismatch)
import Control.Applicative ((<|>))

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
