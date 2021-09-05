module CodegenTypeSynonym where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Codegen (declType, declTypeSignature, module_, printModule, typeApp, typeArrow, typeCtor, typeForall, typeRow, typeVar)
import PureScript.CST.Types (Module)

test :: Module Void
test = unsafePartial do
  module_ "Test.TypeSynonym" [] []
    [ declTypeSignature "Id" do
        typeForall [ typeVar "k" ]
          $ typeArrow [ typeVar "k" ] (typeVar "k")
    , declType "Id" [ typeVar "a" ] (typeVar "a")
    , declType "UserFields" [ typeVar "r" ]
        ( typeRow
            [ Tuple "id" (typeCtor "UserId")
            , Tuple "name" (typeCtor "String")
            , Tuple "age" (typeCtor "Int")
            , Tuple "email" (typeCtor "Email")
            , Tuple "phone" (typeCtor "PhoneNumber")
            , Tuple "followers" (typeApp (typeCtor "Array") [ typeCtor "UserId" ])
            ]
            (Just (typeVar "r"))
        )
    ]

main :: Effect Unit
main = Console.log $ printModule test
