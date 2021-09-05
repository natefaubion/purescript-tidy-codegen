module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Codegen (binaryOp, binderCtor, binderVar, caseBranch, classMember, dataCtor, declClass, declData, declImport, declImportAs, declInstance, declInstanceChain, declNewtype, declSignature, declType, declTypeSignature, declValue, exportClass, exportModule, exportOp, exportType, exportTypeAll, exportTypeMembers, exportTypeOp, exportValue, exprApp, exprCase, exprCtor, exprIdent, exprInt, exprOp, importClass, importOp, importTypeAll, importTypeMembers, importTypeOp, importValue, instName, module_, printModule, typeApp, typeArrow, typeCtor, typeForall, typeVar, typeVarKinded)
import PureScript.CST.Codegen.Monad (codegenModule)
import PureScript.CST.Codegen.Monad as C
import PureScript.CST.Types (Module)

testModule1 :: Module Void
testModule1 = unsafePartial do
  module_ "Test.Example"
    [ exportValue "map"
    , exportOp "<$>"
    , exportType "Void"
    , exportTypeAll "Either"
    , exportTypeMembers "Maybe" [ "Nothing", "Just" ]
    , exportTypeOp "+"
    , exportClass "Functor"
    , exportModule "Exports"
    ]
    [ declImport "Prelude" []
    , declImport "Control.Category" [ importOp "<<<" ]
    , declImport "Data.Maybe"
        [ importValue "maybe"
        , importTypeAll "Maybe"
        ]
    , declImportAs "Data.Maybe" [] "Maybe"
    , declImport "Data.Foldable" [ importClass "Foldable" ]
    , declImport "Data.Either" [ importTypeMembers "Either" [ "Left", "Right" ] ]
    , declImport "Type.Row"
        [ importTypeOp "+"
        ]
    ]
    [ declClass [] "Functor" [ typeVar "f" ] []
        [ classMember "map" do
            typeForall [ typeVar "a", typeVar "b" ]
              $ typeArrow (typeArrow (typeVar "a") (typeVar "b"))
              $ typeArrow (typeApp (typeVar "f") [ typeVar "a" ])
              $ typeApp (typeVar "f") [ typeVar "b" ]
        ]
    , declData "Maybe" [ typeVar "a" ]
        [ dataCtor "Nothing" []
        , dataCtor "Just" [ typeVar "a" ]
        ]
    , declInstance Nothing [] "Functor" [ typeCtor "Maybe" ]
        [ instName "map" [ binderVar "f", binderVar "a" ] do
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
    , declNewtype "Const" [ typeVar "a", typeVarKinded "b" (typeCtor "Type") ] "Const" (typeVar "a")
    , declTypeSignature "Id" do
        typeForall [ typeVar "k" ]
          $ typeArrow (typeVar "k") (typeVar "k")
    , declType "Id" [ typeVar "a" ] (typeVar "a")
    ]

testModule2 :: Module Void
testModule2 = unsafePartial $ codegenModule "Test.Example" do
  C.importOpen "Prelude"
  maybeTy <- C.importFrom "Data.Maybe" (C.importType "Maybe")
  justCtor <- C.importFrom "Data.Maybe" (C.importCtor "Maybe" "Just")
  maybeFn <- C.importFrom "Data.Maybe" (C.importValue "maybe")
  mapTy <- C.importFrom "Data.Map" (C.importType "Map")
  mapLookup <- C.importFrom "Data.Map" (C.importValue "Map.lookup")
  C.exporting do
    C.write $ declSignature "getNum" do
      typeArrow (typeCtor "String")
        $ typeArrow (typeApp (typeCtor mapTy) [ typeCtor "String", typeCtor "Int" ])
        $ typeApp (typeCtor maybeTy) [ typeCtor "Int" ]
    C.write $ declValue "getNum" [ binderVar "key" ] do
      exprOp
        (exprApp (exprIdent maybeFn) [ exprApp (exprCtor justCtor) [ exprInt 0 ] ])
        [ binaryOp "<<<" (exprApp (exprIdent mapLookup) [ exprIdent "key" ])
        ]

main :: Effect Unit
main = do
  log $ printModule testModule1
  log $ printModule testModule2
