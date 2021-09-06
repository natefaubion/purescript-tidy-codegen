module CodegenClass where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Module)
import Tidy.Codegen (classMember, declClass, module_, printModule, typeApp, typeArrow, typeForall, typeVar)

test :: Module Void
test = unsafePartial do
  module_ "Test.Class" [] []
    [ declClass [] "Functor" [ typeVar "f" ] []
        [ classMember "map" do
            typeForall [ typeVar "a", typeVar "b" ]
              $ typeArrow
                  [ typeArrow [ typeVar "a" ] (typeVar "b")
                  , typeApp (typeVar "f") [ typeVar "a" ]
                  ]
                  (typeApp (typeVar "f") [ typeVar "b" ])
        ]
    ]

main :: Effect Unit
main = Console.log $ printModule test
