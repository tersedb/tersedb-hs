module Spec.Generic where
import Lib.Class (TerseDB, TerseDBGen)
import Test.Syd (Spec, describe)
import Spec.Generic.Group (groupTests)
import Data.Data (Proxy)


genericTests :: (TerseDB n m, TerseDBGen n) => Proxy m -> Spec
genericTests p = do
  describe "Group" (groupTests p)
