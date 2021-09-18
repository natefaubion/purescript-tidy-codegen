module CodegenImports where

import Prelude

import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Module)
import Test.Util (log)
import Tidy.Codegen (declImport, declImportAs, importClass, importOp, importTypeAll, importTypeMembers, importTypeOp, importValue, module_, printModule)

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
main = log $ printModule test
