module Spec.Generic.Create where

createTests :: forall m n. (TerseDB n m, TerseDBGen n) => Proxy m -> Spec
createTests Proxy = undefined
