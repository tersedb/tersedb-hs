module Lib.Actions.Safe.Verify.Actor (anyCanReadActor, anyCanCreateActor, anyCanUpdateActor, anyCanDeleteActor) where

import Control.Lens ((^.))
import Control.Monad.Extra (anyM, orM)
import Control.Monad.State (MonadState)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Lib.Actions.Safe.Verify.Utils (canDo)
import Lib.Types.Id (ActorId)
import Lib.Types.Permission (
  CollectionPermission (..),
 )
import Lib.Types.Store (Shared)
import Lib.Types.Store.Tabulation.Group (forRecruiter)

canReadActor :: (MonadState Shared m) => ActorId -> m Bool
canReadActor reader =
  canDo (^. forRecruiter) reader Read

anyCanReadActor :: (MonadState Shared m) => NonEmpty ActorId -> m Bool
anyCanReadActor = anyM canReadActor . NE.toList

canCreateActor :: (MonadState Shared m) => ActorId -> m Bool
canCreateActor creater =
  canDo (^. forRecruiter) creater Create

anyCanCreateActor :: (MonadState Shared m) => NonEmpty ActorId -> m Bool
anyCanCreateActor = anyM canCreateActor . NE.toList

canUpdateActor :: (MonadState Shared m) => ActorId -> ActorId -> m Bool
canUpdateActor updater aId =
  orM
    [ pure (updater == aId)
    , canDo (^. forRecruiter) updater Update
    ]

anyCanUpdateActor
  :: (MonadState Shared m) => NonEmpty ActorId -> ActorId -> m Bool
anyCanUpdateActor updaters aId =
  anyM (`canUpdateActor` aId) (NE.toList updaters)

canDeleteActor :: (MonadState Shared m) => ActorId -> ActorId -> m Bool
canDeleteActor deleter aId =
  orM
    [ pure (deleter == aId)
    , canDo (^. forRecruiter) deleter Delete
    ]

anyCanDeleteActor
  :: (MonadState Shared m) => NonEmpty ActorId -> ActorId -> m Bool
anyCanDeleteActor deleters aId =
  anyM (`canDeleteActor` aId) (NE.toList deleters)
