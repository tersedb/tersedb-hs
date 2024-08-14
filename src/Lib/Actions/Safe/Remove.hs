module Lib.Actions.Safe.Remove where

import Lib.Types.Id (ActorId, VersionId)
import Lib.Types.Store (Shared)

import Control.Monad.State (MonadState)


removeVersion :: MonadState Shared m => ActorId -> VersionId -> m Bool
removeVersion remover vId = undefined
  -- TODO verify that you can a) update the entity vId belongs to, b) delete vId,
  -- c) update nextVId and prevVId if they exist, and d) adjust any references or subscriptions
  -- that vId had.
  -- TODO find entity belonging to version
  -- if there's a next one and a previous one, merge the two:
  --     A ==> B ==> C
  --
  --     A ========> C
  --      \---> B
  -- then destroy B
  --
  -- if there's only a next one, drop it's prev
  --     A ==> B
  --
  --     B
  --     ^--- A
  --
  -- if there's only a previous one, drop its next
  --     A ==> B
  --
  --     A
  --      \---> B
