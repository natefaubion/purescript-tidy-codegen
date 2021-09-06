module Test.GenerateExamples where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Filterable (partitionMap)
import Data.Foldable (foldMap, sequence_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import PureScript.CST (RecoveredParserResult(..), parseDecl, parseModule)
import PureScript.CST.Parser.Monad (PositionedError)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapExpr)
import PureScript.CST.Types (Comment(..), Declaration(..), Expr(..), Guarded(..), Ident(..), Labeled(..), Module(..), ModuleBody(..), ModuleName, Name(..), Where(..))
import Tidy.Codegen (declSignature, declValue, exprApp, exprArray, exprIdent, exprString, leading, printModule, typeApp, typeCtor)
import Tidy.Codegen.Monad (codegenModule, importCtor, importFrom, importOpen, importType, importValue, write)

data GenerateError = ExampleParseError String PositionedError

data ExampleType = ExampleDecl | ExampleType | ExampleExpr

generateExamplesModule :: ModuleName -> String -> Either (NonEmptyArray GenerateError) String
generateExamplesModule modName src = case parseModule src of
  ParseSucceededWithErrors _ errors ->
    Left $ map (ExampleParseError "<file>") errors
  ParseFailed error ->
    Left $ pure $ ExampleParseError "<file>" error
  ParseSucceeded (Module { body: ModuleBody { decls } }) -> do
    let { left, right } = partitionMap sequence declExamples
    case NonEmptyArray.fromArray left of
      Just errors ->
        Left $ NonEmptyArray.fold1 errors
      Nothing -> do
        let
          codegenHeader = map Comment
            [ "------------------------------------"
            , "-- This module is code generated. --"
            , "--          DO NOT EDIT!          --"
            , "------------------------------------"
            ]

        Right $ printModule $ leading codegenHeader $ unsafePartial $ codegenModule modName do
          _ <- importOpen "Prelude"
          _ <- importFrom "Data.Tuple" (importCtor "Tuple" "Tuple")
          _ <- importFrom "Data.Maybe" (importCtor "Maybe" "Just")
          typeEffect <- importFrom "Effect" (importType "Effect")
          exprLog <- importFrom "Effect.Class.Console" (importValue "log")
          exprUnsafePartial <- importFrom "Partial.Unsafe" (importValue "unsafePartial")
          typeModule <- importFrom "PureScript.CST.Types" (importType "Module")
          exprPrintModule <- importFrom "Tidy.Codegen" (importValue "printModule")
          exprModule <- importFrom "Tidy.Codegen" (importValue "module_")
          examples <- for right \(Tuple (Ident ident) (Tuple exampleType expr)) -> do
            let
              imports :: Array _
              imports = expr # foldMapExpr defaultMonoidalVisitor
                { onExpr = case _ of
                    ExprIdent qual ->
                      pure $ importFrom "Tidy.Codegen" (importValue qual)
                    _ ->
                      mempty
                }
            sequence_ imports
            case exampleType of
              ExampleDecl ->
                pure expr
              ExampleType -> do
                exprDeclType <- importFrom "Tidy.Codegen" (importValue "declType")
                pure $ exprApp (exprIdent exprDeclType)
                  [ exprString (String.toUpper (String.take 1 ident) <> String.drop 1 ident <> "Example")
                  , exprArray []
                  , expr
                  ]
              ExampleExpr -> do
                exprDeclValue <- importFrom "Tidy.Codegen" (importValue "declValue")
                pure $ exprApp (exprIdent exprDeclValue)
                  [ exprString (ident <> "Example")
                  , exprArray []
                  , expr
                  ]

          write $ declSignature "test" (typeApp (typeCtor typeModule) [ typeCtor "Void" ])
          write $ declValue "test" [] do
            exprApp (exprIdent exprUnsafePartial)
              [ exprApp (exprIdent exprModule)
                  [ exprString (unwrap modName)
                  , exprArray []
                  , exprArray []
                  , exprArray examples
                  ]
              ]

          write $ declSignature "main" (typeApp (typeCtor typeEffect) [ typeCtor "Unit" ])
          write $ declValue "main" [] do
            exprApp (exprIdent exprLog)
              [ exprApp (exprIdent exprPrintModule)
                  [ exprIdent "test"
                  ]
              ]
    where
    declExamples :: Array _
    declExamples = decls # foldMap case _ of
      DeclSignature (Labeled { label: Name { name, token: { leadingComments } } }) ->
        foldMap pure do
          ix1 <- Array.findIndex isExampleStart leadingComments
          ix2 <- Array.findLastIndex isExampleEnd leadingComments
          let
            exampleSrc =
              leadingComments
                # Array.slice (ix1 + 1) ix2
                # Array.mapMaybe lineComment
                # String.joinWith "\n"

          Tuple name <$> case parseDecl exampleSrc of
            ParseSucceededWithErrors _ errors ->
              pure $ Left $ map (ExampleParseError (unwrap name)) errors
            ParseFailed error ->
              pure $ Left $ pure $ ExampleParseError (unwrap name) error
            ParseSucceeded
              ( DeclValue
                  { name: Name { name: Ident exampleName }
                  , binders: []
                  , guarded: Unconditional _ (Where { bindings: Nothing, expr })
                  }
              )
              | Just exampleType <- exampleTypeFromString exampleName ->
                  pure $ Right $ Tuple exampleType expr
            _ ->
              Nothing
      _ ->
        mempty

    isExampleStart = case _ of
      Comment str ->
        startsWith (String.Pattern "-- | ```purescript") str
      _ -> false

    isExampleEnd = case _ of
      Comment str ->
        startsWith (String.Pattern "-- | ```") str
      _ -> false

    lineComment = case _ of
      Comment str ->
        String.stripPrefix (String.Pattern "-- | ") str
      _ -> Nothing

    startsWith p =
      isJust <<< String.stripPrefix p

    exampleTypeFromString = case _ of
      "exampleDecl" -> Just ExampleDecl
      "exampleType" -> Just ExampleType
      "exampleExpr" -> Just ExampleExpr
      _ -> Nothing
