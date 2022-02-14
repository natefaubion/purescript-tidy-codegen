module CodegenMonad where

import Prelude

import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Module)
import Test.Util (log)
import Tidy.Codegen (binaryOp, binderVar, declSignature, declValue, exprApp, exprIdent, exprInt, exprOp, printModule, typeApp, typeArrow, typeCtor)
import Tidy.Codegen.Monad (codegenModule, exporting, importCtor, importFrom, importFromAlias, importOp, importOpen, importOpenHiding, importType, importValue, write)

test :: Module Void
test = unsafePartial do
  codegenModule "Test.Monad" do
    importOpen "Prelude"

    -- Duplicate module import will defer to hidden version
    importOpen "Prim"
    importOpenHiding "Prim" $ importType "Type"

    -- Duplicate module import will defer to open version
    void $ importFrom "Prim.Boolean" $ importType "True"
    importOpen "Prim.Boolean"

    -- Explicit alias will be used over others
    void $ importFromAlias "Data.Set" "Set"
      { val: importValue "NotUsed.isEmpty"
      , ty: importType "Set"
      }

    -- Duplicate qualified module import will defer to simple alias version
    void $ importFromAlias "Data.Either" "Either" $ importValue "either"
    void $ importFrom "Data.Either" $ importValue "Either.isRight"

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
