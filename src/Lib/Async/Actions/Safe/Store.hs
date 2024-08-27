module Lib.Async.Actions.Safe.Store where

import Control.Concurrent.STM (STM)
import Data.List.NonEmpty (NonEmpty)
import Lib.Async.Actions.Safe.Verify (anyCanCreateGroup, conditionally)
import Lib.Async.Actions.Unsafe.Store (unsafeStoreGroup)
import Lib.Async.Types.Monad (TerseM)
import Lib.Types.Id (ActorId, GroupId)

storeGroup :: NonEmpty ActorId -> GroupId -> TerseM STM Bool
storeGroup creator gId =
  anyCanCreateGroup creator
    >>= conditionally (unsafeStoreGroup gId)
