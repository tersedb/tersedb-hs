module Lib.Async.Actions.Safe.Verify.Actor (
  anyCanReadActor,
  anyCanCreateActor,
  anyCanUpdateActor,
  anyCanDeleteActor,
) where

import Control.Concurrent.STM (STM)
import Control.Lens ((^.))
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Extra (anyM, orM)
import Control.Monad.Reader (MonadReader (ask))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Lib.Async.Actions.Safe.Verify.Utils (canDo)
import Lib.Async.Types.Monad (TerseM)
import Lib.Async.Types.Store (temp, toTabRecruiter)
import Lib.Types.Id (ActorId)
import Lib.Types.Permission (CollectionPermission (..))
import qualified StmContainers.Map as Map

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
canUpdateActor updater aId =
  orM
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
canDeleteActor deleter aId =
  orM
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
