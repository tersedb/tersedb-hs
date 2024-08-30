module Lib.Types.Errors where

import Control.Exception (Exception)
import GHC.Generics (Generic)
import Lib.Types.Id (GroupId)

newtype CycleDetected = CycleDetected [GroupId]
  deriving (Generic, Show, Eq)
instance Exception CycleDetected

data UnauthorizedAction = UnauthorizedAction
  deriving (Generic, Show, Eq)
instance Exception UnauthorizedAction
