module Lib.Types.Monad where

import Lib.Types.Store (Store)

import Control.Monad.State (StateT)


type SheepdogM = StateT Store IO
