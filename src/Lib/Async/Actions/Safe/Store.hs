module Lib.Async.Actions.Safe.Store where
import Data.List.NonEmpty (NonEmpty)
import Lib.Types.Id (ActorId, GroupId)
import Lib.Async.Types.Monad (TerseM)
import Control.Concurrent.STM (STM)
import Lib.Async.Actions.Safe.Verify (anyCanCreateGroup, conditionally)
import Lib.Async.Actions.Unsafe.Store (unsafeStoreGroup)

storeGroup :: NonEmpty ActorId -> GroupId -> TerseM STM Bool
storeGroup creator gId =
  anyCanCreateGroup creator
    >>= conditionally (unsafeStoreGroup gId)
