module CodegenNewtype where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Module)
import Tidy.Codegen (declNewtype, declNewtypeSignature, module_, printModule, typeApp, typeArrow, typeCtor, typeForall, typeRecord, typeVar)

test :: Module Void
test = unsafePartial do
  module_ "Test.Newtype" [] []
    [ declNewtypeSignature "Const" do
        typeForall [ typeVar "k" ]
          ( typeArrow
              [ typeCtor "Type"
              , typeVar "k"
              ]
              (typeCtor "Type")
          )
    , declNewtype "Const" [ typeVar "a", typeVar "b" ]
        "Const"
        (typeVar "a")
    , declNewtype "User" []
        "User"
        ( typeRecord
            [ Tuple "id" (typeCtor "UserId")
            , Tuple "name" (typeCtor "String")
            , Tuple "age" (typeCtor "Int")
            , Tuple "email" (typeCtor "Email")
            , Tuple "phone" (typeCtor "PhoneNumber")
            , Tuple "followers" (typeApp (typeCtor "Array") [ typeCtor "UserId" ])
            ]
            Nothing
        )
    ]

main :: Effect Unit
main = Console.log $ printModule test
