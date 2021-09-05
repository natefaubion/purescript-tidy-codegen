module CodegenImports where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Codegen (declImport, declImportAs, importClass, importOp, importTypeAll, importTypeMembers, importTypeOp, importValue, module_, printModule)
import PureScript.CST.Types (Module)

test :: Module Void
test = unsafePartial do
  module_ "CodegenImports" []
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
    []

main :: Effect Unit
main = Console.log $ printModule test
