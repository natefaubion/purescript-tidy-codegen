module CodegenMonad where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Codegen (binaryOp, binderVar, declSignature, declValue, exprApp, exprCtor, exprIdent, exprInt, exprOp, printModule, typeApp, typeArrow, typeCtor)
import PureScript.CST.Codegen.Monad (codegenModule, exporting, importCtor, importFrom, importOpen, importType, importValue, write)
import PureScript.CST.Types (Module)

test :: Module Void
test = unsafePartial do
  codegenModule "Test.Data" do
    importOpen "Prelude"
    maybeTy <- importFrom "Data.Maybe" (importType "Maybe")
    justCtor <- importFrom "Data.Maybe" (importCtor "Maybe" "Just")
    maybeFn <- importFrom "Data.Maybe" (importValue "maybe")
    mapTy <- importFrom "Data.Map" (importType "Map")
    mapLookup <- importFrom "Data.Map" (importValue "Map.lookup")
    exporting do
      write $ declSignature "getNum" do
        typeArrow
          [ typeCtor "String"
          , typeApp (typeCtor mapTy) [ typeCtor "String", typeCtor "Int" ]
          ]
          ( typeApp (typeCtor maybeTy)
              [ typeCtor "Int" ]
          )
      write $ declValue "getNum" [ binderVar "key" ] do
        exprOp
          ( exprApp (exprIdent maybeFn)
              [ exprApp (exprCtor justCtor)
                  [ exprInt 0 ]
              ]
          )
          [ binaryOp "<<<"
              ( exprApp (exprIdent mapLookup)
                  [ exprIdent "key" ]
              )
          ]

main :: Effect Unit
main = Console.log $ printModule test
