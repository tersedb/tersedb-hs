module Lib.Api.Response.Create where

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
import Lib.Api.Action.Create (CreateAction (..))
import Lib.Types.Id (ActorId, AnyId (..), EntityId, GroupId, SpaceId, VersionId)
import Test.QuickCheck (Arbitrary (arbitrary), oneof)

data CreateResponse
  = NewActor ActorId
  | NewGroup GroupId
  | NewSpace SpaceId
  | NewEntity EntityId VersionId
  | NewVersion VersionId
  deriving (Eq, Show, Read, Generic)
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
