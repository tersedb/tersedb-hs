module Lib.Async.Actions.Safe.Verify.Utils where

import Control.Concurrent.STM (STM)
import Control.Lens ((^.))
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Trans.Control (MonadBaseControl (liftBaseWith))
import Lib.Async.Types.Monad (TerseM)
import Lib.Async.Types.Store (temp, toMemberOf)
import Lib.Types.Id (ActorId, GroupId)
import Lib.Types.Permission (HasMinimumPermission (..))
import ListT (foldMaybe)
import qualified StmContainers.Multimap as Multimap

canDoWithTab
  :: (HasMinimumPermission a)
  => (GroupId -> TerseM STM a)
  -> ActorId
  -> (GroupId -> TerseM STM a)
  -> TerseM STM Bool
canDoWithTab getPermToCheck aId getReferencePerm = do
  s <- ask
  liftBaseWith $ \runInBase -> do
    let go :: Bool -> GroupId -> STM (Maybe Bool)
        go acc gId = do
          if acc
            then pure Nothing
            else do
              permToCheck <- runInBase (getPermToCheck gId)
              referencePerm <- runInBase (getReferencePerm gId)
              pure . Just $ permToCheck `hasMinimumPermission` referencePerm
    foldMaybe go False (Multimap.listTByKey aId (s ^. temp . toMemberOf))

canDo
  :: (HasMinimumPermission a)
  => (GroupId -> TerseM STM a)
  -> ActorId
  -> a
  -> TerseM STM Bool
canDo getPermToCheck aId perm = canDoWithTab getPermToCheck aId (const (pure perm))
