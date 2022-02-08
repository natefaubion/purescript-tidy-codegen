module CodegenMonad where

import Prelude

import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Module)
import Test.Util (log)
import Tidy.Codegen (binaryOp, binderVar, declSignature, declValue, exprApp, exprIdent, exprInt, exprOp, printModule, typeApp, typeArrow, typeCtor)
import Tidy.Codegen.Monad (codegenModule, exporting, importCtor, importFrom, importOp, importOpen, importType, importValue, write)

test :: Module Void
test = unsafePartial do
  codegenModule "Test.Monad" do
    importOpen "Prelude"
    maybeTy <- importFrom "Data.Maybe" (importType "Maybe")
    justCtor <- importFrom "Data.Maybe" (importCtor "Maybe" "Just")
    maybeFn <- importFrom "Data.Maybe" (importValue "maybe")
    mapTy <- importFrom "Data.Map" (importType "Map")
    mapLookup <- importFrom "Data.Map" (importValue "Map.lookup")
    altOp <- importFrom "Control.Alt" (importOp "<|>")
    exporting do
      write $ declSignature "getNum" do
        typeArrow
          [ typeCtor "String"
          , typeApp mapTy [ typeCtor "String", typeCtor "Int" ]
          ]
          ( typeApp maybeTy
              [ typeCtor "Int" ]
          )
      write $ declValue "getNum" [ binderVar "key" ] do
        exprOp
          ( exprApp maybeFn
              [ exprApp justCtor
                  [ exprInt 0 ]
              ]
          )
          [ binaryOp "<<<"
              ( exprApp mapLookup
                  [ exprIdent "key" ]
              )
          ]
      write $ declValue "alt'" [ binderVar "a", binderVar "b" ] do
        exprOp (exprIdent "a")
          [ altOp.binaryOp
              (exprIdent "b")
          ]
      write $ declValue "alt''" [ binderVar "a", binderVar "b" ] do
        exprApp altOp.exprOpName
          [ exprIdent "a"
          , exprIdent "b"
          ]

main :: Effect Unit
main = log $ printModule test
