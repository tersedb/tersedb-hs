module Lib.Types.Monad where

import Lib.Types.Store (Shared)

import Control.Monad.State (StateT)


type SheepdogM = StateT Shared IO
