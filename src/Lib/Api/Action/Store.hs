module Lib.Api.Action.Store where

import Lib.Types.Id (GroupId, ActorId, SpaceId, VersionId, EntityId)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary), oneof)
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), object, withObject, (.:), (.=))
import Control.Applicative ((<|>))

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
