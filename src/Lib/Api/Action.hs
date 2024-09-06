module Lib.Api.Action where

import Lib.Api.Action.Read (ReadAction)
import Lib.Api.Action.Create (CreateAction (..))
import Lib.Api.Action.Store (StoreAction (..))
import Lib.Api.Action.Update (UpdateAction)
import Lib.Api.Action.Delete (DeleteAction)
import Lib.Types.Id (AnyId (..))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary), oneof)
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), object, withObject, (.:), (.=))
import Control.Applicative ((<|>))

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
