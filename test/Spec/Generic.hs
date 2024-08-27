module Spec.Generic where

import Data.Data (Proxy)
import Lib.Class (TerseDB, TerseDBGen)
import Spec.Generic.Group (groupTests)
import Spec.Generic.Read (readTests)
import Test.Syd (Spec, describe)
import Spec.Generic.Create (createTests)

genericTests :: (TerseDB n m, TerseDBGen n) => Proxy m -> Spec
genericTests p = do
  describe "Group" (groupTests p)
  -- describe "Read" (readTests p)
  describe "Create" (createTests p)
