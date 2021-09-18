module CodegenExports where

import Prelude

import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Module)
import Test.Util (log)
import Tidy.Codegen (exportClass, exportModule, exportOp, exportType, exportTypeAll, exportTypeMembers, exportTypeOp, exportValue, module_, printModule)

test :: Module Void
test = unsafePartial do
  module_ "Test.Exports"
    [ exportValue "map"
    , exportOp "<$>"
    , exportType "Void"
    , exportTypeAll "Either"
    , exportTypeMembers "Maybe" [ "Nothing", "Just" ]
    , exportTypeOp "+"
    , exportClass "Functor"
    , exportModule "Test.Exports"
    ]
    []
    []

main :: Effect Unit
main = log $ printModule test
