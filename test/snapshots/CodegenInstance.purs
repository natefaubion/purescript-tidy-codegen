module CodegenInstance where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Module)
import Test.Util (log)
import Tidy.Codegen (binderCtor, binderVar, caseBranch, declInstance, declInstanceChain, exprApp, exprCase, exprCtor, exprIdent, instValue, module_, printModule, typeApp, typeCtor, typeVar)

test :: Module Void
test = unsafePartial do
  module_ "Test.Instance" [] []
    [ declInstance Nothing [] "Functor" [ typeCtor "Maybe" ]
        [ instValue "map" [ binderVar "f", binderVar "a" ] do
            exprCase [ exprIdent "a" ]
              [ caseBranch [ binderCtor "Nothing" [] ] do
                  exprCtor "Nothing"
              , caseBranch [ binderCtor "Just" [ binderVar "b" ] ] do
                  exprApp (exprCtor "Just")
                    [ exprApp (exprIdent "f") [ exprIdent "b" ] ]
              ]
        ]
    , declInstanceChain
        [ declInstance Nothing [] "Foo" [ typeApp (typeCtor "Either") [ typeVar "a" ] ] []
        , declInstance Nothing [] "Foo" [ typeCtor "Maybe" ] []
        ]
    ]

main :: Effect Unit
main = log $ printModule test
