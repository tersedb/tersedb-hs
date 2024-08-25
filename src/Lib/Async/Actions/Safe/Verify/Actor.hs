module Lib.Async.Actions.Safe.Verify.Actor
  ( anyCanReadActor
  , anyCanCreateActor
  , anyCanUpdateActor
  , anyCanDeleteActor
  ) where

import Lib.Types.Id (ActorId)
import Lib.Async.Types.Monad (TerseM)
import Control.Concurrent.STM (STM)
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Base (MonadBase (liftBase))
import qualified StmContainers.Map as Map
import Lib.Async.Types.Store (toTabRecruiter, temp)
import Lib.Types.Permission (CollectionPermission (..))
import Control.Lens ((^.))
import Lib.Async.Actions.Safe.Verify.Utils (canDo)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Control.Monad.Extra (anyM, orM)

canReadActor :: ActorId -> TerseM STM Bool
canReadActor reader =
  canDo getPerm reader Read
  where
    getPerm gId = do
      s <- ask
      liftBase (fromMaybe Blind <$> Map.lookup gId (s ^. temp . toTabRecruiter))

anyCanReadActor :: NonEmpty ActorId -> TerseM STM Bool
anyCanReadActor = anyM canReadActor . NE.toList

canCreateActor :: ActorId -> TerseM STM Bool
canCreateActor creater =
  canDo getPerm creater Create
  where
    getPerm gId = do
      s <- ask
      liftBase (fromMaybe Blind <$> Map.lookup gId (s ^. temp . toTabRecruiter))

anyCanCreateActor :: NonEmpty ActorId -> TerseM STM Bool
anyCanCreateActor = anyM canCreateActor . NE.toList

canUpdateActor :: ActorId -> ActorId -> TerseM STM Bool
canUpdateActor updater aId = orM
  [ pure (updater == aId)
  , canDo getPerm updater Update
  ]
  where
    getPerm gId = do
      s <- ask
      liftBase (fromMaybe Blind <$> Map.lookup gId (s ^. temp . toTabRecruiter))

anyCanUpdateActor
  :: NonEmpty ActorId -> ActorId -> TerseM STM Bool
anyCanUpdateActor updaters aId =
  anyM (`canUpdateActor` aId) (NE.toList updaters)

canDeleteActor :: ActorId -> ActorId -> TerseM STM Bool
canDeleteActor deleter aId = orM
  [ pure (deleter == aId)
  , canDo getPerm deleter Delete
  ]
  where
    getPerm gId = do
      s <- ask
      liftBase (fromMaybe Blind <$> Map.lookup gId (s ^. temp . toTabRecruiter))

anyCanDeleteActor
  :: NonEmpty ActorId -> ActorId -> TerseM STM Bool
anyCanDeleteActor deleters aId =
  anyM (`canDeleteActor` aId) (NE.toList deleters)
