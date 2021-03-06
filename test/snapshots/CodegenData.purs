module CodegenData where

import Prelude

import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Module)
import Test.Util (log)
import Tidy.Codegen (dataCtor, declData, declDataSignature, module_, printModule, typeArrow, typeCtor, typeForall, typeVar)

test :: Module Void
test = unsafePartial do
  module_ "Test.Data" [] []
    [ declData "Maybe" [ typeVar "a" ]
        [ dataCtor "Nothing" []
        , dataCtor "Just" [ typeVar "a" ]
        ]
    , declDataSignature "Proxy" do
        typeForall [ typeVar "k" ]
          (typeArrow [ typeVar "k" ] (typeCtor "Type"))
    , declData "Proxy" [ typeVar "a" ]
        [ dataCtor "Proxy" []
        ]
    ]

main :: Effect Unit
main = log $ printModule test
