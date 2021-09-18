module CodegenReadme where

import Prelude

import Control.Monad.Writer (tell)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Module)
import Test.Util (log)
import Tidy.Codegen (binderCtor, binderVar, caseBranch, dataCtor, declData, declDerive, declSignature, declValue, exprApp, exprCase, exprIdent, exprSection, printModule, typeApp, typeArrow, typeCtor, typeForall, typeVar)
import Tidy.Codegen.Monad (codegenModule, importOpen)

test :: Module Void
test = unsafePartial $ codegenModule "Data.Maybe" do
  importOpen "Prelude"
  tell
    [ declData "Maybe" [ typeVar "a" ]
        [ dataCtor "Nothing" []
        , dataCtor "Just" [ typeVar "a" ]
        ]

    , declDerive Nothing [] "Functor" [ typeCtor "Maybe" ]

    , declSignature "maybe" do
        typeForall [ typeVar "a", typeVar "b" ] do
          typeArrow
            [ typeVar "b"
            , typeArrow [ typeVar "a" ] (typeVar "b")
            , typeApp (typeCtor "Maybe") [ typeVar "a" ]
            ]
            (typeVar "b")
    , declValue "maybe" [ binderVar "nothing", binderVar "just" ] do
        exprCase [ exprSection ]
          [ caseBranch [ binderCtor "Just" [ binderVar "a" ] ] do
              exprApp (exprIdent "just") [ exprIdent "a" ]
          , caseBranch [ binderCtor "Nothing" [] ] do
              exprIdent "nothing"
          ]
    ]

main :: Effect Unit
main = log $ printModule test
