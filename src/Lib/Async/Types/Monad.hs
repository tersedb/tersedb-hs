module Lib.Async.Types.Monad where

import Control.Monad.Reader (ReaderT)
import Lib.Async.Types.Store (Shared)


type TerseM = ReaderT Shared
