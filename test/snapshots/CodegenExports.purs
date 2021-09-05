module CodegenExports where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Codegen (exportClass, exportModule, exportOp, exportType, exportTypeAll, exportTypeMembers, exportTypeOp, exportValue, module_, printModule)
import PureScript.CST.Types (Module)

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
main = Console.log $ printModule test
