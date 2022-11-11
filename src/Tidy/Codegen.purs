module Tidy.Codegen
  ( PrintOptions
  , defaultPrintOptions
  , printModule
  , printModuleWithOptions
  , class DeclInstance
  , class TypeVar
  , binaryOp
  , binderArray
  , binderBool
  , binderChar
  , binderCtor
  , binderInt
  , binderNamed
  , binderNumber
  , binderOp
  , binderParens
  , binderRecord
  , binderString
  , binderTyped
  , binderVar
  , binderWildcard
  , blockComment
  , caseBranch
  , classMember
  , dataCtor
  , declClass
  , declClassSignature
  , declData
  , declDataSignature
  , declDerive
  , declDeriveNewtype
  , declForeign
  , declForeignData
  , declImport
  , declImportAs
  , declImportHiding
  , declImportHidingAs
  , declInfix
  , declInfixType
  , declInstance
  , declInstanceChain
  , declNewtype
  , declNewtypeSignature
  , declRole
  , declSignature
  , declType
  , declTypeSignature
  , declValue
  , doBind
  , doDiscard
  , doLet
  , docComments
  , exportClass
  , exportModule
  , exportOp
  , exportType
  , exportTypeAll
  , exportTypeMembers
  , exportTypeOp
  , exportValue
  , exprAdo
  , exprApp
  , exprArray
  , exprBool
  , exprCase
  , exprChar
  , exprCtor
  , exprDo
  , exprDot
  , exprHole
  , exprIdent
  , exprIf
  , exprInfix
  , exprInt
  , exprIntHex
  , exprLambda
  , exprLet
  , exprNegate
  , exprNumber
  , exprOp
  , exprOpName
  , exprParens
  , exprRecord
  , exprSection
  , exprString
  , exprTyped
  , exprUpdate
  , exprWhere
  , guardBinder
  , guardBranch
  , guardExpr
  , importClass
  , importOp
  , importType
  , importTypeAll
  , importTypeMembers
  , importTypeOp
  , importValue
  , instSignature
  , instValue
  , leading
  , letBinder
  , letSignature
  , letValue
  , lineBreaks
  , lineComments
  , module_
  , spaces
  , trailing
  , typeApp
  , typeArrow
  , typeArrowName
  , typeConstrained
  , typeCtor
  , typeForall
  , typeHole
  , typeKinded
  , typeOp
  , typeOpName
  , typeParens
  , typeRecord
  , typeRecordEmpty
  , typeRow
  , typeRowEmpty
  , typeString
  , typeInt
  , typeVar
  , typeVarKinded
  , typeWildcard
  , update
  , updateNested
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap, lmap)
import Data.Int as Int
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Ord (abs)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..), curry)
import Dodo (plainText)
import Dodo as Dodo
import PureScript.CST.Types (Binder(..), ClassFundep, Comment(..), DataCtor(..), DataMembers(..), Declaration(..), DoStatement(..), Export(..), Expr(..), Fixity, FixityOp(..), Foreign(..), Guarded, Ident, Import(..), ImportDecl(..), Instance(..), InstanceBinding(..), InstanceHead, IntValue(..), Label, Labeled(..), LetBinding(..), LineFeed(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName, Name(..), Operator(..), PatternGuard(..), Proper, QualifiedName(..), RecordUpdate(..), Role, Separated(..), SourceToken, Token(..), Type(..), TypeVarBinding(..), Where(..), Wrapped(..))
import PureScript.CST.Types as CST
import Safe.Coerce (coerce)
import Tidy (ImportWrapOption(..), TypeArrowOption(..), UnicodeOption(..), defaultFormatOptions, formatModule, toDoc)
import Tidy.Codegen.Class (class OverLeadingComments, class OverTrailingComments, class ToFixityName, class ToGuarded, class ToName, class ToNonEmptyArray, class ToQualifiedName, class ToRecordLabeled, class ToWhere, ErrorPrefix(..), overLeadingComments, overTrailingComments, toFixityName, toGuarded, toName, toNonEmptyArray, toQualifiedName, toRecordLabeled, toToken, toWhere)
import Tidy.Codegen.Common (toDelimited, toDelimitedNonEmpty, toOneOrDelimited, toParenList, toSeparated, toSourceToken, toWrapped, tokAdo, tokAll, tokAs, tokAt, tokBackslash, tokCase, tokClass, tokComma, tokData, tokDerive, tokDo, tokDot, tokDoubleColon, tokElse, tokEquals, tokFalse, tokForFixity, tokForRole, tokForall, tokForeign, tokHiding, tokIf, tokImport, tokIn, tokInstance, tokLeftArrow, tokLeftBrace, tokLeftFatArrow, tokLeftParen, tokLeftSquare, tokLet, tokModule, tokNegate, tokNewtype, tokOf, tokPipe, tokRightArrow, tokRightBrace, tokRightFatArrow, tokRightParen, tokRightSquare, tokRole, tokSymbolArrow, tokThen, tokTick, tokTrue, tokType, tokUnderscore, tokWhere)
import Tidy.Codegen.Precedence (precBinder0, precBinder1, precBinder2, precExpr0, precExpr1, precExpr2, precExpr3, precExpr5, precExpr6, precExpr7, precExprApp, precExprAppLast, precExprInfix, precInitLast, precType0, precType1, precType2, precType3)
import Tidy.Codegen.String (escapeSourceString)
import Tidy.Codegen.Types (BinaryOp(..), ClassMember, GuardedBranch(..), HoleName(..), SymbolName(..))
import Tidy.Operators (parseOperatorTable)
import Tidy.Operators.Defaults (defaultOperators)
import Tidy.Precedence (PrecedenceMap)

type PrintOptions =
  { importWrap :: ImportWrapOption
  , indentUnit :: String
  , indentWidth :: Int
  , operators :: Lazy PrecedenceMap
  , pageWidth :: Int
  , ribbonRatio :: Number
  , typeArrowPlacement :: TypeArrowOption
  , unicode :: UnicodeOption
  }

defaultPrintOptions :: PrintOptions
defaultPrintOptions =
  { importWrap: ImportWrapSource
  , indentUnit: "  "
  , indentWidth: 2
  , operators: Lazy.defer \_ -> parseOperatorTable defaultOperators
  , pageWidth: 160
  , ribbonRatio: 0.618
  , typeArrowPlacement: TypeArrowFirst
  , unicode: UnicodeSource
  }

-- | Pretty-prints a module using default format options.
-- |
-- |  ```purescript
-- |  exampleSource = printModule myModule
-- |  ```
printModule :: Module Void -> String
printModule = printModuleWithOptions defaultPrintOptions

-- | Pretty-prints a module given a set of options. Use `defaultPrintOptions`
-- | and override as necessary.
-- |
-- | ```purescript
-- | exampleSource = printModule
-- |   defaultPrintOptions
-- |     { indentUnit = "    "
-- |     , indentWidth = 4
-- |     , unicode = UnicodeAlways
-- |     }
-- |   myModule
-- | ```
printModuleWithOptions :: PrintOptions -> Module Void -> String
printModuleWithOptions options mod =
  -- Eta-expanded to defer operator parsing
  Dodo.print plainText dodoOptions
    $ toDoc
    $ formatModule formatOptions
    $ overTrailingComments addTrailingBreak mod
  where
  dodoOptions =
    { indentUnit: options.indentUnit
    , indentWidth: options.indentWidth
    , pageWidth: options.pageWidth
    , ribbonRatio: options.ribbonRatio
    }

  formatOptions = defaultFormatOptions
    { importWrap = options.importWrap
    , operators = Lazy.force options.operators
    , typeArrowPlacement = options.typeArrowPlacement
    , unicode = options.unicode
    }

  addTrailingBreak comments = case Array.last comments of
    Just (Line _ n) | n > 0 ->
      comments
    _ ->
      comments <> lineBreaks 1

-- | An overloaded binary operator constructor. Use with `exprOp`, `typeOp`,
-- | or `binderOp`.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprOp (exprInt 4)
-- |     [ binaryOp "+" (exprIdent "a")
-- |     , binaryOp "/" (exprIdent "b")
-- |     ]
-- | ```
binaryOp :: forall name b. ToQualifiedName name Operator => name -> b -> BinaryOp b
binaryOp a b = BinaryOp (Tuple (toQualifiedName a) b)

-- | Overloaded constructors for type variables. This can be used to construct
-- | not only `CST.Type`, but also type variable bindings in `typeForall` and
-- | data, newtype, type, and class declarations.
-- |
-- | ```purescript
-- | exampleDecl =
-- |   declType "Id" [ typeVarKinded "a" (typeCtor "Type") ]
-- |     (typeVar "a")
-- | ```
class TypeVar a e | a -> e where
  -- | An overloaded constructor for type variables.
  typeVar :: forall name. ToName name Ident => name -> a
  -- | An overloaded constructor for kinded type variables.
  typeVarKinded :: forall name. ToName name Ident => name -> CST.Type e -> a

instance TypeVar (TypeVarBinding e) e where
  typeVar = TypeVarName <<< toName
  typeVarKinded name value =
    TypeVarKinded
      $ toWrapped tokLeftParen tokRightParen
      $ Labeled { label: toName name, separator: tokDoubleColon, value }

instance TypeVar (CST.Type e) e where
  typeVar = TypeVar <<< toName
  typeVarKinded name = typeParens <<< typeKinded (typeParens (TypeVar (toName name)))

-- | An overloaded constructor for a proper type name.
-- |
-- | ```purescript
-- | exampleType = typeApp (typeCtor "Maybe") [ typeCtor "Int" ]
-- | ```
typeCtor :: forall e name. ToQualifiedName name Proper => name -> CST.Type e
typeCtor = TypeConstructor <<< toQualifiedName

-- | A type wildcard (`_`).
typeWildcard :: forall e. CST.Type e
typeWildcard = TypeWildcard tokUnderscore

-- | An overloaded constructor for a row type.
-- |
-- | ```purescript
-- | exampleType = typeRow
-- |   [ Tuple "id" (typeCtor "UserId")
-- |   , Tuple "name" (typeCtor "String")
-- |   , Tuple "age" (typeCtor "Int")
-- |   ]
-- |   (Just (typeVar "r"))
-- | ```
typeRow :: forall e label. ToName label Label => Array (Tuple label (CST.Type e)) -> Maybe (CST.Type e) -> CST.Type e
typeRow lbls ty = TypeRow $ toWrapped tokLeftParen tokRightParen $ CST.Row
  { labels: toSeparated tokComma <<< map (toLabeled tokDoubleColon) <$> NonEmptyArray.fromArray lbls
  , tail: Tuple tokPipe <$> ty
  }

-- | The empty row type.
typeRowEmpty :: forall e. CST.Type e
typeRowEmpty = typeRow ([] :: Array (Tuple Label _)) Nothing

-- | An overloaded constructor for a record type.
-- |
-- | ```purescript
-- | exampleType = typeRecord
-- |   [ Tuple "id" (typeCtor "UserId")
-- |   , Tuple "name" (typeCtor "String")
-- |   , Tuple "age" (typeCtor "Int")
-- |   ]
-- |   (Just (typeVar "r"))
-- | ```
typeRecord :: forall e label. ToName label Label => Array (Tuple label (CST.Type e)) -> Maybe (CST.Type e) -> CST.Type e
typeRecord lbls ty = TypeRecord $ toWrapped tokLeftBrace tokRightBrace $ CST.Row
  { labels: toSeparated tokComma <<< map (toLabeled tokDoubleColon) <$> NonEmptyArray.fromArray lbls
  , tail: Tuple tokPipe <$> ty
  }

-- | The empty record type.
typeRecordEmpty :: forall e. CST.Type e
typeRecordEmpty = typeRecord ([] :: Array (Tuple Label _)) Nothing

-- | Constructs a type-level string while handling escaping.
-- |
-- | ```
-- | exampleType = typeString "string"
-- | ```
typeString :: forall e. String -> CST.Type e
typeString str = TypeString (toSourceToken (TokString (unwrap (escapeSourceString str)) str)) str

-- | Constructs a type-level int
-- |
-- | ```
-- | exampleType = typeInt (-1)
-- | ```
typeInt :: forall e. Int -> CST.Type e
typeInt n = TypeInt neg (toSourceToken (TokInt (show val) (SmallInt val))) (SmallInt val)
  where
  val = abs n
  neg = if n < 0 then Just tokNegate else Nothing

-- | Constructs a type with a kind annotation.
-- |
-- | ```purescript
-- | exampleType =
-- |   typeKinded (typeCtor "Maybe")
-- |     (typeArrow [ typeCtor "Type" ] (typeCtor "Type"))
-- | ```
typeKinded :: forall e. CST.Type e -> CST.Type e -> CST.Type e
typeKinded a b = TypeKinded (precType0 a) tokDoubleColon b

-- | Constructs left-associated applications.
-- |
-- | ```purescript
-- | exampleType =
-- |   typeApp (typeCtor "Map")
-- |     [ typeCtor "UserId"
-- |     , typeCtor "User"
-- |     ]
-- | ```
typeApp :: forall e. CST.Type e -> Array (CST.Type e) -> CST.Type e
typeApp ty = maybe ty (TypeApp (precType3 ty)) <<< NonEmptyArray.fromArray <<< map precType3

-- | Overloaded constructor for a type hole.
-- |
-- | ```purescript
-- | exampleType = typeHole "?HelpMePls"
-- | ```
typeHole :: forall e name. ToName name HoleName => name -> CST.Type e
typeHole = TypeHole <<< (coerce :: Name HoleName -> Name Ident) <<< toName

-- | Constructs binary operator applications. These may be grouped by the
-- | pretty-printer based on precedence.
-- |
-- | ```purescript
-- | exampleType =
-- |   typeOp (typeCtor "String")
-- |     [ binaryOp "/\\" (typeCtor "Int")
-- |     , binaryOp "/\\" (typeCtor "Boolean")
-- |     ]
-- | ```
typeOp :: forall e. CST.Type e -> Array (BinaryOp (CST.Type e)) -> CST.Type e
typeOp ty = maybe ty (TypeOp (precType2 ty) <<< coerce) <<< NonEmptyArray.fromArray <<< map (map precType2)

-- | Overloaded constructor for a type operator's symbolic identifier (wrapping
-- | it in parens), which may be qualified.
-- |
-- | ```purescript
-- | exampleType = typeOpName "(~>)"
-- | ```
typeOpName :: forall e name. ToQualifiedName name SymbolName => name -> CST.Type e
typeOpName = TypeOpName <<< (coerce :: QualifiedName SymbolName -> _) <<< toQualifiedName

-- | Constructs a `forall` given type variable bindings.
-- |
-- | ```purescript
-- | exampleType =
-- |   typeForall [ typeVar "a" ]
-- |     (typeArrow [ typeVar "a" ] (typeVar "a"))
-- | ```
typeForall :: forall e. Array (TypeVarBinding e) -> CST.Type e -> CST.Type e
typeForall vars ty =
  vars # NonEmptyArray.fromArray # maybe ty \vars' ->
    TypeForall tokForall vars' tokDot (precType0 ty)

-- | Constructs right-associated constraint arrows.
-- |
-- | ```purescript
-- | exampleType =
-- |   typeForall [ typeVar "f", typeVar "a" ]
-- |     ( typeConstrained
-- |         [ typeApp (typeCtor "Functor") [ typeVar "f" ]
-- |         , typeApp (typeCtor "Show") [ typeVar "a" ]
-- |         ]
-- |         ( typeArrow
-- |             [ typeApp (typeVar "f")
-- |                 [ typeVar "a" ]
-- |             ]
-- |             ( typeApp (typeVar "f")
-- |                 [ typeCtor "String" ]
-- |             )
-- |         )
-- |     )
-- | ```
typeConstrained :: forall e. Array (CST.Type e) -> CST.Type e -> CST.Type e
typeConstrained = flip $ Array.foldr \a b -> TypeConstrained (precType1 a) tokRightFatArrow (precType0 b)

-- | Constructs right-associated function arrows.
-- |
-- | ```purescript
-- | exampleType =
-- |   typeArrow
-- |     [ typeCtor "UserId"
-- |     , typeCtor "String"
-- |     , typeCtor "Int"
-- |     ]
-- |     (typeCtor "User")
-- | ```
typeArrow :: forall e. Array (CST.Type e) -> CST.Type e -> CST.Type e
typeArrow = flip $ Array.foldr \a b -> TypeArrow (precType1 a) tokRightArrow (precType0 b)

-- | The function arrow's symbolic identifier (`(->)`).
typeArrowName :: forall e. CST.Type e
typeArrowName = TypeArrowName tokSymbolArrow

-- | Wraps a type in parens.
typeParens :: forall e. CST.Type e -> CST.Type e
typeParens = case _ of
  a@(TypeParens _) -> a
  a -> wrap a
  where
  wrap = TypeParens <<< toWrapped tokLeftParen tokRightParen

-- | An overloaded constructor for a value identifier, which may be qualified.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprApp (exprIdent "Map.lookup")
-- |     [ exprIdent "userId"
-- |     , exprIdent "users"
-- |     ]
-- | ```
exprIdent :: forall e name. ToQualifiedName name Ident => name -> Expr e
exprIdent = ExprIdent <<< toQualifiedName

-- | An overloaded constructor for a hole.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprApp (exprCtor "List.Cons")
-- |     [ exprHole "?helpMePls"
-- |     , exprCtor "List.Nil"
-- |     ]
-- | ```
exprHole :: forall e name. ToName name HoleName => name -> Expr e
exprHole = ExprHole <<< (coerce :: Name HoleName -> Name Ident) <<< toName

-- | An overloaded constructor for a value constructor, which may be qualified.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprApp (exprCtor "List.Cons")
-- |     [ exprIdent "a"
-- |     , exprCtor "List.Nil"
-- |     ]
-- | ```
exprCtor :: forall e name. ToQualifiedName name Proper => name -> Expr e
exprCtor = ExprConstructor <<< toQualifiedName

-- | Constructs a `Boolean` literal.
-- |
-- | ```purescript
-- | exampleExpr = exprBool true
-- | ```
exprBool :: forall e. Boolean -> Expr e
exprBool bool = ExprBoolean (if bool then tokTrue else tokFalse) bool

-- | Constructs a `Char` literal while handling escaping.
-- |
-- | ```purescript
-- | exampleExpr = exprChar 'A'
-- | ```
exprChar :: forall e. Char -> Expr e
exprChar ch = ExprChar (toSourceToken (TokChar (unwrap (escapeSourceString (SCU.singleton ch))) ch)) ch

-- | Constructs a `String` literal while handling escaping.
-- |
-- | ```purescript
-- | exampleExpr = exprString "string"
-- | ```
exprString :: forall e. String -> Expr e
exprString str = ExprString (toSourceToken (TokString (unwrap (escapeSourceString str)) str)) str

-- | Constructs an `Int` literal.
-- |
-- | ```purescript
-- | exampleExpr = exprInt 42
-- | ```
exprInt :: forall e. Int -> Expr e
exprInt n = ExprInt (toSourceToken (TokInt (show n) (SmallInt n))) (SmallInt n)

-- | Constructs an `Int` literal using hex notation.
-- |
-- | ```purescript
-- | exampleExpr = exprIntHex 0xFF0000
-- | ```
exprIntHex :: forall e. Int -> Expr e
exprIntHex n = ExprInt (toSourceToken (TokInt ("0x" <> Int.toStringAs Int.hexadecimal n) (SmallInt n))) (SmallInt n)

-- | Constructs a `Number` literal.
-- |
-- | ```purescript
-- | exampleExpr = exprNumber 1.618
-- | ```
exprNumber :: forall e. Number -> Expr e
exprNumber n = ExprNumber (toSourceToken (TokNumber (show n) n)) n

-- | Constructs an `Array` literal.
-- |
-- | ```purescript
-- | exampleExpr = exprArray
-- |   [ exprInt 1
-- |   , exprInt 2
-- |   , exprInt 3
-- |   ]
-- | ```
exprArray :: forall e. Array (Expr e) -> Expr e
exprArray = ExprArray <<< toDelimited tokLeftSquare tokRightSquare tokComma

-- | Constructs a `Record` literal.
-- |
-- | ```purescript
-- | exampleExpr = exprRecord
-- |   [ Tuple "id" (exprIdent "userId")
-- |   , Tuple "name" (exprIdent "userName")
-- |   , Tuple "age" (exprIdent "userAge")
-- |   ]
-- | ```
exprRecord :: forall e field. ToRecordLabeled field (Expr e) => Array field -> Expr e
exprRecord = ExprRecord <<< toDelimited tokLeftBrace tokRightBrace tokComma <<< map toRecordLabeled

-- | Wraps an expression in parens.
exprParens :: forall e. Expr e -> Expr e
exprParens = case _ of
  a@(ExprParens _) -> a
  a -> wrap a
  where
  wrap = ExprParens <<< toWrapped tokLeftParen tokRightParen

-- | Constructs an expression with a type annotation.
-- |
-- | ```purescript
-- | exampleExpr = exprTyped (exprInt 42) (typeCtor "Int")
-- | ```
exprTyped :: forall e. Expr e -> CST.Type e -> Expr e
exprTyped a b = ExprTyped (precExpr0 a) tokDoubleColon b

-- | Constructs left-associative infix expression applications.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprInfix (exprIdent "a")
-- |     [ Tuple (exprIdent "append") (exprIdent "b")
-- |     , Tuple (exprIdent "append") (exprIdent "c")
-- |     ]
-- | ```
exprInfix :: forall e. Expr e -> Array (Tuple (Expr e) (Expr e)) -> Expr e
exprInfix expr =
  maybe expr (ExprInfix (precExpr2 expr))
    <<< precInitLast (bimap wrapInfix precExpr3) (bimap wrapInfix precExprInfix)
  where
  wrapInfix =
    toWrapped tokTick tokTick <<< precExprInfix

-- | Constructors binary operator applications. These may be grouped by the
-- | pretty-printer based on precedence.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprOp (exprString "string")
-- |     [ binaryOp "/\\" (exprInt 42)
-- |     , binaryOp "/\\" (exprBool false)
-- |     ]
-- | ```
exprOp :: forall e. Expr e -> Array (BinaryOp (Expr e)) -> Expr e
exprOp expr =
  maybe expr (ExprOp (precExpr2 expr) <<< coerce)
    <<< precInitLast (map precExpr2) (map precExpr1)

-- | Overloaded constructor for an expression operator's symbolic identifier (wrapping
-- | it in parens).
-- |
-- | ```purescript
-- | exampleExpr = exprOpName "(<>)"
-- | ```
exprOpName :: forall e name. ToQualifiedName name SymbolName => name -> Expr e
exprOpName = ExprOpName <<< (coerce :: QualifiedName SymbolName -> _) <<< toQualifiedName

-- | Constructs unary negation.
exprNegate :: forall e. Expr e -> Expr e
exprNegate = ExprNegate tokNegate <<< precExpr5

-- | Constructs a record dot-accessor.
-- |
-- | ```purescript
-- | exampleExpr = exprDot (exprIdent "response") [ "body", "users" ]
-- | ```
exprDot :: forall e label. ToName label Label => Expr e -> Array label -> Expr e
exprDot expr = NonEmptyArray.fromArray >>> maybe expr \path ->
  ExprRecordAccessor
    { expr: precExpr7 expr
    , dot: tokDot
    , path: toSeparated tokDot $ toName <$> path
    }

-- | Constructs a record update.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprUpdate (exprIdent "user")
-- |     [ update "age" (exprInt 42)
-- |     , updateNested "phone"
-- |         [ update "countryCode" (exprInt 1)
-- |         ]
-- |     ]
-- | ```
exprUpdate :: forall e. Expr e -> Array (RecordUpdate e) -> Expr e
exprUpdate expr = NonEmptyArray.fromArray >>> maybe expr \value ->
  ExprRecordUpdate (precExpr6 expr) $ toDelimitedNonEmpty tokLeftBrace tokRightBrace tokComma value

-- | Constructs an update for a field.
update :: forall e a. ToName a Label => a -> Expr e -> RecordUpdate e
update a b = RecordUpdateLeaf (toName a) tokEquals b

-- | Constructs a nested update for a field.
updateNested :: forall f e label. ToNonEmptyArray f => ToName label Label => label -> f (RecordUpdate e) -> RecordUpdate e
updateNested a = RecordUpdateBranch (toName a) <<< toDelimitedNonEmpty tokLeftBrace tokRightBrace tokComma <<< toNonEmptyArray (ErrorPrefix "updateNested")

-- | Constructs left-associated applications.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprApp (exprIdent "Map.lookup")
-- |     [ exprIdent "userId"
-- |     , exprIdent "users"
-- |     ]
-- | ```
exprApp :: forall e. Expr e -> Array (Expr e) -> Expr e
exprApp head =
  maybe head (ExprApp (precExprApp head))
    <<< precInitLast precExprApp precExprAppLast

-- | Constructs a lambda expression.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprLambda [ binderVar "a", binderVar "b" ]
-- |     ( exprOp (exprIdent "a")
-- |         [ binaryOp "<>" (exprIdent "b") ]
-- |     )
-- | ```
exprLambda :: forall e. Array (Binder e) -> Expr e -> Expr e
exprLambda bnds body = bnds # NonEmptyArray.fromArray # maybe body \bnds' ->
  ExprLambda { symbol: tokBackslash, binders: map precBinder2 bnds', arrow: tokRightArrow, body }

-- | Constructs an if-then-else expression.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprIf (exprApp (exprIdent "isLoggedIn") [ exprIdent "user" ])
-- |     (exprIdent "renderPage")
-- |     (exprApp (exprIdent "httpError") [ exprInt 400 ])
-- | ```
exprIf :: forall e. Expr e -> Expr e -> Expr e -> Expr e
exprIf a b c = ExprIf { keyword: tokIf, cond: a, then: tokThen, true: b, else: tokElse, false: c }

-- | Constructs a case expression.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprCase [ exprIdent "xs" ]
-- |     [ caseBranch [ binderCtor "List.Cons" [ binderVar "x", binderWildcard ] ]
-- |         ( exprApp (exprCtor "Just")
-- |             [ exprIdent "x"
-- |             ]
-- |         )
-- |     , caseBranch [ binderCtor "Nothing" [] ]
-- |         (exprCtor "Nothing")
-- |     ]
-- | ```
exprCase
  :: forall e f g
   . ToNonEmptyArray f
  => ToNonEmptyArray g
  => f (Expr e)
  -> g (Tuple (Separated (Binder e)) (Guarded e))
  -> Expr e
exprCase head branches = ExprCase
  { keyword: tokCase
  , head: toSeparated tokComma (toNonEmptyArray (ErrorPrefix "exprCase head") head)
  , of: tokOf
  , branches: toNonEmptyArray (ErrorPrefix "exprCase branches") branches
  }

-- | Constructs a let expression.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprLet
-- |     [ letSignature "countDown" (typeArrow [ typeCtor "Int" ] (typeCtor "Int"))
-- |     , letValue "countDown" [ binderVar "n" ]
-- |         [ guardBranch [ guardExpr (exprOp (exprIdent "n") [ binaryOp ">" (exprInt 0) ]) ]
-- |             ( exprApp (exprIdent "countDown")
-- |                 [ exprOp (exprIdent "n") [ binaryOp "-" (exprInt 1) ] ]
-- |             )
-- |         , guardBranch [ guardExpr (exprIdent "otherwise") ]
-- |             (exprIdent "n")
-- |         ]
-- |     ]
-- |     (exprApp (exprIdent "countDown") [ exprInt 100 ])
-- | ```
exprLet :: forall e. Array (LetBinding e) -> Expr e -> Expr e
exprLet binds body = binds # NonEmptyArray.fromArray # maybe body \bindings ->
  ExprLet { keyword: tokLet, bindings, in: tokIn, body }

-- | Constructs a do expression.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprDo
-- |     [ doBind (binderVar "followers")
-- |         (exprApp (exprIdent "getFollowers") [ exprIdent "user" ])
-- |     , doBind (binderVar "favorites")
-- |         (exprApp (exprIdent "getFavorites") [ exprIdent "user" ])
-- |     ]
-- |     ( exprApp (exprIdent "pure")
-- |         [ exprRecord [ "followers", "favorites" ] ]
-- |     )
-- | ```
exprDo :: forall e. Array (DoStatement e) -> Expr e -> Expr e
exprDo stmts expr = ExprDo
  { keyword: tokDo
  , statements: NonEmptyArray.snoc' stmts (DoDiscard expr)
  }

-- | Constructs an ado expression. Works just like `exprDo`.
exprAdo :: forall e. Array (DoStatement e) -> Expr e -> Expr e
exprAdo statements result = ExprAdo { keyword: tokAdo, statements, in: tokIn, result }

-- | Constructs a type signature in a let binding context. See `exprLet`.
letSignature :: forall e name. ToName name Ident => name -> CST.Type e -> LetBinding e
letSignature name value = LetBindingSignature $ Labeled
  { label: toName name
  , separator: tokDoubleColon
  , value
  }

-- | Constructs a value binding in a let binding context. See `exprLet`.
letValue
  :: forall e name rhs
   . ToName name Ident
  => ToGuarded rhs e
  => name
  -> Array (Binder e)
  -> rhs
  -> LetBinding e
letValue name binders grd = LetBindingName
  { name: toName name
  , binders
  , guarded: toGuarded tokEquals grd
  }

-- | Constructs a value binding with a left-hand-side pattern binder.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprLet
-- |     [ letBinder (binderRecord [ "name" ])
-- |         (exprIdent "user")
-- |     ]
-- |     (exprIdent "name")
-- | ```
letBinder :: forall e rhs. ToWhere rhs e => Binder e -> rhs -> LetBinding e
letBinder binder = LetBindingPattern binder tokEquals <<< toWhere

-- | Constructs a let binding context within a do expression.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprDo
-- |     [ doLet
-- |         [ letBinder (binderRecord [ "age" ])
-- |             (exprIdent "user")
-- |         ]
-- |     ]
-- |     (exprIdent "age")
-- | ```
doLet :: forall e f. ToNonEmptyArray f => f (LetBinding e) -> DoStatement e
doLet = DoLet tokLet <<< toNonEmptyArray (ErrorPrefix "doLet")

-- | Constructs a do statement which has no binder.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprDo
-- |     [ doDiscard
-- |         ( exprApp (exprIdent "logoutUser")
-- |             [ exprIdent "user" ]
-- |         )
-- |     ]
-- |     ( exprApp (exprIdent "pure")
-- |         [ exprApp (exprIdent "httpStatus")
-- |             [ exprInt 200 ]
-- |         ]
-- |     )
-- | ```
doDiscard :: forall e. Expr e -> DoStatement e
doDiscard = DoDiscard

-- | Constructs a do statement which binds it's return value.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprDo
-- |     [ doBind (binderRecord [ "followers" ])
-- |         (exprApp (exprIdent "getUser") [ exprIdent "user" ])
-- |     ]
-- |     ( exprApp (exprIdent "pure")
-- |         [ exprIdent "followers" ]
-- |     )
-- | ```
doBind :: forall e. Binder e -> Expr e -> DoStatement e
doBind = flip DoBind tokLeftArrow

-- | Constructs a where let binding context. This can be used anywhere that
-- | takes a `ToWhere` or `ToGuarded` constraint, such as in value bindings,
-- | case branches, and guards.
-- |
-- | ```purescript
-- | exampleDecl =
-- |   declValue "getName" [ binderVar "user" ]
-- |     ( exprWhere (exprIdent "name")
-- |         [ letBinder (binderRecord [ "name" ])
-- |             (exprIdent "user")
-- |         ]
-- |     )
-- | ```
exprWhere :: forall e. Expr e -> Array (LetBinding e) -> Where e
exprWhere expr binds = Where { expr, bindings: Tuple tokWhere <$> NonEmptyArray.fromArray binds }

-- | Constructs a guarded branch in a value binding or case expressions.
-- | This can be used anwhere that takes a `ToGuarded` constraint.
-- |
-- | ```purescript
-- | exampleDecl =
-- |   declValue "countDown" [ binderVar "n" ]
-- |     [ guardBranch [ guardExpr (exprOp (exprIdent "n") [ binaryOp ">" (exprInt 0) ]) ]
-- |         ( exprApp (exprIdent "countDown")
-- |             [ exprOp (exprIdent "n") [ binaryOp "-" (exprInt 1) ] ]
-- |         )
-- |     , guardBranch [ guardExpr (exprIdent "otherwise") ]
-- |         (exprIdent "n")
-- |     ]
-- | ```
guardBranch :: forall e f expr. ToNonEmptyArray f => ToWhere expr e => f (PatternGuard e) -> expr -> GuardedBranch e
guardBranch pats = GuardedBranch (toNonEmptyArray (ErrorPrefix "guardBranch") pats) <<< toWhere

-- | Constructs a guard expression that does not bind anything.
guardExpr :: forall e. Expr e -> PatternGuard e
guardExpr = PatternGuard <<< { binder: Nothing, expr: _ }

-- | Constructs a guard expression that binds a value.
guardBinder :: forall e. Binder e -> Expr e -> PatternGuard e
guardBinder bnd = PatternGuard <<< { binder: Just (Tuple bnd tokLeftArrow), expr: _ }

-- | Constructs a case branch for use with exprCase. See exprCase.
caseBranch
  :: forall e f branch
   . ToNonEmptyArray f
  => ToGuarded branch e
  => f (Binder e)
  -> branch
  -> Tuple (Separated (Binder e)) (Guarded e)
caseBranch lhs rhs =
  Tuple (toSeparated tokComma $ map precBinder0 $ (toNonEmptyArray (ErrorPrefix "caseBranch") lhs))
    (toGuarded tokRightArrow rhs)

-- | Constructs an expression section (`_`). This is meaningless and will
-- | produce invalid syntax when used arbitrarily. Pair with appropriate
-- | constructors like `exprCase` or `exprDot`.
exprSection :: forall e. Expr e
exprSection = ExprSection tokUnderscore

-- | Constructs a wildcard (`_`) binding pattern.
-- |
-- | ```purescript
-- | exampleExpr =
-- |   exprLambda [ binderWildcard ]
-- |     (exprApp (exprIdent "countDown") [ exprInt 100 ])
-- | ```
binderWildcard :: forall e. Binder e
binderWildcard = BinderWildcard tokUnderscore

-- | Constructs a variable binding pattern.
binderVar :: forall e name. ToName name Ident => name -> Binder e
binderVar = BinderVar <<< toName

-- | Constructs a named binding pattern (`@`).
binderNamed :: forall e name. ToName name Ident => name -> Binder e -> Binder e
binderNamed n = BinderNamed (toName n) tokAt <<< precBinder2

-- | Constructs a constructor binding pattern.
binderCtor :: forall e name. ToQualifiedName name Proper => name -> Array (Binder e) -> Binder e
binderCtor n = BinderConstructor (toQualifiedName n) <<< map precBinder2

-- | Constructs a boolean literal binding pattern.
binderBool :: forall e. Boolean -> Binder e
binderBool bool = BinderBoolean (if bool then tokTrue else tokFalse) bool

-- | Constructs a char literal binding pattern.
binderChar :: forall e. Char -> Binder e
binderChar ch = BinderChar (toSourceToken (TokChar (unwrap (escapeSourceString (SCU.singleton ch))) ch)) ch

-- | Constructs a string literal binding pattern.
binderString :: forall e. String -> Binder e
binderString str = BinderString (toSourceToken (TokString (unwrap (escapeSourceString str)) str)) str

-- | Constructs an int literal binding pattern.
binderInt :: forall e. Int -> Binder e
binderInt n = BinderInt neg (toSourceToken (TokInt (show val) (SmallInt val))) (SmallInt val)
  where
  val = abs n
  neg = if n < 0 then Just tokNegate else Nothing

-- | Constructs a number literal binding pattern.
binderNumber :: forall e. Number -> Binder e
binderNumber n = BinderNumber neg (toSourceToken (TokNumber (show val) val)) val
  where
  val = abs n
  neg = if n < 0.0 then Just tokNegate else Nothing

-- | Constructs an array literal binding pattern.
binderArray :: forall e. Array (Binder e) -> Binder e
binderArray arr = BinderArray $ Wrapped
  { close: tokRightSquare
  , open: tokLeftSquare
  , value: toSeparated tokComma <$> NonEmptyArray.fromArray arr
  }

-- | Constructs a record literal binding pattern.
binderRecord :: forall e field. ToRecordLabeled field (Binder e) => Array field -> Binder e
binderRecord arr = BinderRecord $ Wrapped
  { close: tokRightBrace
  , open: tokLeftBrace
  , value: toSeparated tokComma <<< map toRecordLabeled <$> NonEmptyArray.fromArray arr
  }

-- | Wraps a binding pattern in parens.
binderParens :: forall e. Binder e -> Binder e
binderParens = BinderParens <<< toWrapped tokLeftParen tokRightParen

-- | Constructs a typed binding pattern.
binderTyped :: forall e. Binder e -> CST.Type e -> Binder e
binderTyped = flip BinderTyped tokDoubleColon <<< precBinder0

-- | Constructs an operator binding pattern.
binderOp :: forall e. Binder e -> Array (BinaryOp (Binder e)) -> Binder e
binderOp bnd = maybe bnd (BinderOp bnd <<< coerce) <<< NonEmptyArray.fromArray <<< map (map precBinder1)

-- | Constructs a data declaration.
-- |
-- | ```purescript
-- | exampleDecl =
-- |   declData "Either" [ typeVar "a", typeVar "b" ]
-- |     [ dataCtor "Left" [ typeVar "a" ]
-- |     , dataCtor "Right" [ typeVar "b" ]
-- |     ]
-- | ```
declData
  :: forall e name
   . ToName name Proper
  => name
  -> Array (TypeVarBinding e)
  -> Array (DataCtor e)
  -> Declaration e
declData name vars ctors =
  DeclData { keyword: tokData, name: toName name, vars } $
    Tuple tokEquals <<< toSeparated tokPipe <$> NonEmptyArray.fromArray ctors

-- | Constructs a data constructor variant. See declData.
dataCtor :: forall e name. ToName name Proper => name -> Array (CST.Type e) -> DataCtor e
dataCtor name fields = DataCtor { name: toName name, fields }

-- | Constructs a type synonym declaration.
-- |
-- | ```purescript
-- | exampleDecl =
-- |   declType "UserFields" [ typeVar "r" ]
-- |     ( typeRow
-- |         [ Tuple "id" (typeCtor "UserId")
-- |         , Tuple "name" (typeCtor "String")
-- |         , Tuple "age" (typeCtor "Int")
-- |         ]
-- |         (Just (typeVar "r"))
-- |     )
-- | ```
declType
  :: forall e name
   . ToName name Proper
  => name
  -> Array (TypeVarBinding e)
  -> CST.Type e
  -> Declaration e
declType name vars =
  DeclType { keyword: tokType, name: toName name, vars } tokEquals

-- | Constructs a newtype declaration.
-- |
-- | ```purescript
-- | exampleDecl =
-- |   declNewtype "UserId" [] "UserId" (typeCtor "String")
-- | ```
declNewtype
  :: forall e name ctor
   . ToName name Proper
  => ToName ctor Proper
  => name
  -> Array (TypeVarBinding e)
  -> ctor
  -> CST.Type e
  -> Declaration e
declNewtype name vars ctor =
  DeclNewtype { keyword: tokNewtype, name: toName name, vars } tokEquals (toName ctor)

-- | Constructs a class declaration.
-- |
-- | ```purescript
-- | exampleDecl =
-- |   declClass [ typeApp (typeCtor "Eq") [ typeVar "a" ] ] "Ord" [ typeVar "a" ] []
-- |     [ classMember "compare"
-- |         (typeArrow [ typeVar "a", typeVar "a" ] (typeCtor "Ordering"))
-- |     ]
-- | ```
declClass
  :: forall e name
   . ToName name Proper
  => Array (CST.Type e)
  -> name
  -> Array (TypeVarBinding e)
  -> Array ClassFundep
  -> Array (ClassMember e)
  -> Declaration e
declClass super name vars fundeps members =
  DeclClass
    { keyword: tokClass
    , super: flip Tuple tokLeftFatArrow <<< toOneOrDelimited <$> NonEmptyArray.fromArray super
    , name: toName name
    , vars
    , fundeps: Tuple tokPipe <<< toSeparated tokComma <$> NonEmptyArray.fromArray fundeps
    }
    (Tuple tokWhere <$> NonEmptyArray.fromArray members)

-- | Constructs a class member signature. See declClass.
classMember :: forall e name. ToName name Ident => name -> CST.Type e -> ClassMember e
classMember = curry (toLabeled tokDoubleColon)

class DeclInstance a e | a -> e where
  -- | An overloaded constructor for instances. Can be used to construct a
  -- | declaration directly, or within an instance chain.
  -- |
  -- | ```purescript
  -- | exampleDecl =
  -- |   declInstance Nothing [] "Functor" [ typeApp (typeCtor "Maybe") [ typeVar "a" ] ]
  -- |     [ instValue "map" [ binderVar "f" ]
  -- |         ( exprCase [ exprSection ]
  -- |             [ caseBranch [ binderCtor "Just" [ binderVar "a" ] ]
  -- |                 (exprApp (exprCtor "Just") [ exprApp (exprIdent "f") [ exprIdent "a" ] ])
  -- |             , caseBranch [ binderCtor "Nothing" [] ]
  -- |                 (exprCtor "Nothing")
  -- |             ]
  -- |         )
  -- |     ]
  -- | ```
  declInstance
    :: forall className
     . ToQualifiedName className Proper
    => Maybe (Name Ident)
    -> Array (CST.Type e)
    -> className
    -> Array (CST.Type e)
    -> Array (InstanceBinding e)
    -> a

instance DeclInstance (Instance e) e where
  declInstance name constraints className types bindings = Instance
    { head: instHead name constraints className types
    , body: Tuple tokWhere <$> NonEmptyArray.fromArray bindings
    }

instance DeclInstance (Declaration e) e where
  declInstance name constraints className types bindings =
    DeclInstanceChain $ Separated
      { head: declInstance name constraints className types bindings
      , tail: []
      }

-- | Constructs an instance chain.
-- |
-- | ```purescript
-- | exampleDecl =
-- |   declInstanceChain
-- |     [ declInstance Nothing [] "IsTypeEqual"
-- |         [ typeVar "a", typeVar "a", typeCtor "True" ]
-- |         []
-- |     , declInstance Nothing [] "IsTypeEqual"
-- |         [ typeVar "a", typeVar "b", typeCtor "False" ]
-- |         []
-- |     ]
-- | ```
declInstanceChain :: forall e f. ToNonEmptyArray f => f (Instance e) -> Declaration e
declInstanceChain = DeclInstanceChain <<< toSeparated tokElse <<< toNonEmptyArray (ErrorPrefix "declInstanceChain")

-- | Constructs an signature for an instance binding.
instSignature :: forall e a. ToName a Ident => a -> CST.Type e -> InstanceBinding e
instSignature = curry $ InstanceBindingSignature <<< toLabeled tokDoubleColon

-- | Constructs an instance value binding. See declInstance.
instValue :: forall e a b. ToName a Ident => ToGuarded b e => a -> Array (Binder e) -> b -> InstanceBinding e
instValue name binders grd = InstanceBindingName
  { name: toName name
  , binders
  , guarded: toGuarded tokEquals grd
  }

instHead
  :: forall e className
   . ToQualifiedName className Proper
  => Maybe (Name Ident)
  -> Array (CST.Type e)
  -> className
  -> Array (CST.Type e)
  -> InstanceHead e
instHead name constraints className types =
  { keyword: tokInstance
  , name: flip Tuple tokDoubleColon <$> name
  , constraints: flip Tuple tokRightFatArrow <<< toOneOrDelimited <$> NonEmptyArray.fromArray constraints
  , className: toQualifiedName className
  , types: map precType3 types
  }

-- | Constructs an instance deriving declaration.
-- |
-- | ```purescript
-- | exampleDecl =
-- |   declDerive Nothing [] "Eq" [ typeCtor "UserId" ]
-- | ```
declDerive
  :: forall e className
   . ToQualifiedName className Proper
  => Maybe (Name Ident)
  -> Array (CST.Type e)
  -> className
  -> Array (CST.Type e)
  -> Declaration e
declDerive name constraints className types =
  DeclDerive tokDerive Nothing (instHead name constraints className types)

-- | Constructs a newtype instance deriving declaration.
-- |
-- | ```purescript
-- | exampleDecl =
-- |   declDeriveNewtype Nothing [] "Eq" [ typeCtor "UserId" ]
-- | ```
declDeriveNewtype
  :: forall e className
   . ToQualifiedName className Proper
  => Maybe (Name Ident)
  -> Array (CST.Type e)
  -> className
  -> Array (CST.Type e)
  -> Declaration e
declDeriveNewtype name constraints className types =
  DeclDerive tokDerive (Just tokNewtype) (instHead name constraints className types)

-- | Constructs a kind signature for a data declaration.
declDataSignature :: forall e a. ToName a Proper => a -> CST.Type e -> Declaration e
declDataSignature = curry $ DeclKindSignature tokData <<< toLabeled tokDoubleColon

-- | Constructs a kind signature for a newtype declaration.
declNewtypeSignature :: forall e a. ToName a Proper => a -> CST.Type e -> Declaration e
declNewtypeSignature = curry $ DeclKindSignature tokNewtype <<< toLabeled tokDoubleColon

-- | Constructs a kind signature for a type synonym declaration.
declTypeSignature :: forall e a. ToName a Proper => a -> CST.Type e -> Declaration e
declTypeSignature = curry $ DeclKindSignature tokType <<< toLabeled tokDoubleColon

-- | Constructs a kind signature for a class declaration.
declClassSignature :: forall e a. ToName a Proper => a -> CST.Type e -> Declaration e
declClassSignature = curry $ DeclKindSignature tokClass <<< toLabeled tokDoubleColon

-- | Constructs a type signature for a value declaration.
declSignature :: forall e a. ToName a Ident => a -> CST.Type e -> Declaration e
declSignature = curry $ DeclSignature <<< toLabeled tokDoubleColon

-- | Constructs a value declaration.
-- |
-- | ```purescript
-- | exampleDecl =
-- |   declValue "countDown" [ binderVar "n" ]
-- |     [ guardBranch [ guardExpr (exprOp (exprIdent "n") [ binaryOp ">" (exprInt 0) ]) ]
-- |         ( exprApp (exprIdent "countDown")
-- |             [ exprOp (exprIdent "n") [ binaryOp "-" (exprInt 1) ] ]
-- |         )
-- |     , guardBranch [ guardExpr (exprIdent "otherwise") ]
-- |         (exprIdent "n")
-- |     ]
-- | ```
declValue
  :: forall e name guards
   . ToName name Ident
  => ToGuarded guards e
  => name
  -> Array (Binder e)
  -> guards
  -> Declaration e
declValue name binders grd = DeclValue
  { name: toName name
  , binders
  , guarded: toGuarded tokEquals grd
  }

-- | Constructs an fixity declaration for a value operator.
-- |
-- | ```purescript
-- | exampleDecl =
-- |   declInfix Infixl 4 "map" "<$>"
-- | ```
declInfix
  :: forall e name op
   . ToFixityName name
  => ToName op Operator
  => Fixity
  -> Int
  -> name
  -> op
  -> Declaration e
declInfix fixity prec name op = DeclFixity
  { keyword: Tuple (tokForFixity fixity) fixity
  , prec: lmap toSourceToken $ toToken prec
  , operator: FixityValue (toFixityName name) tokAs (toName op)
  }

-- | Constructs an fixity declaration for a type operator.
-- |
-- | ```purescript
-- | exampleDecl =
-- |   declInfix Infixr 0 "RowApply" "+"
-- | ```
declInfixType
  :: forall e name op
   . ToQualifiedName name Proper
  => ToName op Operator
  => Fixity
  -> Int
  -> name
  -> op
  -> Declaration e
declInfixType fixity prec name op = DeclFixity
  { keyword: Tuple (tokForFixity fixity) fixity
  , prec: lmap toSourceToken $ toToken prec
  , operator: FixityType tokType (toQualifiedName name) tokAs (toName op)
  }

-- | Constructs a foreign import declaration.
declForeign :: forall e name. ToName name Ident => name -> CST.Type e -> Declaration e
declForeign = curry $ DeclForeign tokForeign tokImport <<< ForeignValue <<< toLabeled tokDoubleColon

-- | Constructs a foreign data import declaration.
declForeignData :: forall e name. ToName name Proper => name -> CST.Type e -> Declaration e
declForeignData = curry $ DeclForeign tokForeign tokImport <<< ForeignData tokData <<< toLabeled tokDoubleColon

-- | Constructs a role declaration.
declRole :: forall e f name. ToName name Proper => ToNonEmptyArray f => name -> f Role -> Declaration e
declRole name roles = DeclRole tokType tokRole (toName name) ((Tuple =<< tokForRole) <$> toNonEmptyArray (ErrorPrefix "declRole") roles)

-- | Constructs an import declaration with selective imports. Providing no named
-- | imports results in an open import.
declImport :: forall e name. ToName name ModuleName => name -> Array (Import e) -> ImportDecl e
declImport name imports = ImportDecl
  { keyword: tokImport
  , module: toName name
  , names: Tuple Nothing <<< toParenList <$> NonEmptyArray.fromArray imports
  , qualified: Nothing
  }

-- | Constructs a qualified import declaration.
declImportAs :: forall e as name. ToName name ModuleName => ToName as ModuleName => name -> Array (Import e) -> as -> ImportDecl e
declImportAs name imports as = ImportDecl
  { keyword: tokImport
  , module: toName name
  , names: Tuple Nothing <<< toParenList <$> NonEmptyArray.fromArray imports
  , qualified: Just (Tuple tokAs (toName as))
  }

-- | Constructs a hiding import declaration.
declImportHiding :: forall e name. ToName name ModuleName => name -> Array (Import e) -> ImportDecl e
declImportHiding name imports = ImportDecl
  { keyword: tokImport
  , module: toName name
  , names: Tuple (Just tokHiding) <<< toParenList <$> NonEmptyArray.fromArray imports
  , qualified: Nothing
  }

-- | Constructs a qualified hiding import declaration.
declImportHidingAs :: forall e as name. ToName name ModuleName => ToName as ModuleName => name -> Array (Import e) -> as -> ImportDecl e
declImportHidingAs name imports as = ImportDecl
  { keyword: tokImport
  , module: toName name
  , names: Tuple (Just tokHiding) <<< toParenList <$> NonEmptyArray.fromArray imports
  , qualified: Just (Tuple tokAs (toName as))
  }

-- | Constructs a value import.
importValue :: forall e name. ToName name Ident => name -> Import e
importValue = ImportValue <<< toName

-- | Constructs a value operator import.
importOp :: forall e name. ToName name SymbolName => name -> Import e
importOp = ImportOp <<< (coerce :: Name SymbolName -> Name Operator) <<< toName

-- | Constructs a type import.
importType :: forall e name. ToName name Proper => name -> Import e
importType = flip ImportType Nothing <<< toName

-- | Constructs a type import with all data members.
importTypeAll :: forall e name. ToName name Proper => name -> Import e
importTypeAll = flip ImportType (Just (DataAll tokAll)) <<< toName

-- | Constructs a type import with selective data members.
importTypeMembers :: forall e name member. ToName name Proper => ToName member Proper => name -> Array member -> Import e
importTypeMembers name members = ImportType (toName name) $ Just $ DataEnumerated $ Wrapped
  { close: tokRightParen
  , open: tokLeftParen
  , value: toSeparated tokComma <<< map toName <$> NonEmptyArray.fromArray members
  }

-- | Constructs a type operator import.
importTypeOp :: forall e name. ToName name SymbolName => name -> Import e
importTypeOp = ImportTypeOp tokType <<< (coerce :: Name SymbolName -> Name Operator) <<< toName

-- | Constructs a class import.
importClass :: forall e name. ToName name Proper => name -> Import e
importClass = ImportClass tokClass <<< toName

-- | Constructs a value export.
exportValue :: forall e name. ToName name Ident => name -> Export e
exportValue = ExportValue <<< toName

-- | Constructs a value operator export.
exportOp :: forall e name. ToName name SymbolName => name -> Export e
exportOp = ExportOp <<< (coerce :: Name SymbolName -> Name Operator) <<< toName

-- | Constructs a type export.
exportType :: forall e name. ToName name Proper => name -> Export e
exportType = flip ExportType Nothing <<< toName

-- | Constructs a type export with all data members.
exportTypeAll :: forall e name. ToName name Proper => name -> Export e
exportTypeAll = flip ExportType (Just (DataAll tokAll)) <<< toName

-- | Constructs a type export with selective data members.
exportTypeMembers :: forall e name member. ToName name Proper => ToName member Proper => name -> Array member -> Export e
exportTypeMembers name members = ExportType (toName name) $ Just $ DataEnumerated $ Wrapped
  { close: tokRightParen
  , open: tokLeftParen
  , value: toSeparated tokComma <<< map toName <$> NonEmptyArray.fromArray members
  }

-- | Constructs a type operator export.
exportTypeOp :: forall e name. ToName name SymbolName => name -> Export e
exportTypeOp = ExportTypeOp tokType <<< (coerce :: Name SymbolName -> Name Operator) <<< toName

-- | Constructs a class export.
exportClass :: forall e name. ToName name Proper => name -> Export e
exportClass = ExportClass tokClass <<< toName

-- | Constructs a module re-export.
exportModule :: forall e name. ToName name ModuleName => name -> Export e
exportModule = ExportModule tokModule <<< toName

-- | Constructs a module.
module_
  :: forall e name
   . ToName name ModuleName
  => name
  -> Array (Export e)
  -> Array (ImportDecl e)
  -> Array (Declaration e)
  -> Module e
module_ name exports imports decls = Module
  { header: ModuleHeader
      { keyword: tokModule
      , name: toName name
      , exports: toParenList <$> NonEmptyArray.fromArray exports
      , where: tokWhere
      , imports
      }
  , body: ModuleBody
      { decls
      , trailingComments: []
      , end: zero
      }
  }

-- | Constructs line comments (`--`).
lineComments :: String -> Array (Comment LineFeed)
lineComments = map (Comment <<< append "-- ") <<< String.split (String.Pattern "\n")

-- | Constructs documentation line comments (`-- |`).
docComments :: String -> Array (Comment LineFeed)
docComments = map (Comment <<< append "-- | ") <<< String.split (String.Pattern "\n")

-- | Constructs a block comment.
blockComment :: forall a. String -> Array (Comment a)
blockComment = pure <<< Comment <<< append "{- " <<< flip append " -}"

-- | Constructs line break annotations.
lineBreaks :: Int -> Array (Comment LineFeed)
lineBreaks = pure <<< Line LF

-- | Constructs space annotations.
spaces :: forall a. Int -> Array (Comment a)
spaces = pure <<< Space

-- | Attaches leading comments to a CST node.
leading :: forall a. OverLeadingComments a => Array (Comment LineFeed) -> a -> a
leading c = overLeadingComments (append c)

-- | Attaches trailing comments to a CST node.
trailing :: forall a trl. OverTrailingComments a trl => a -> Array (Comment trl) -> a
trailing a c = overTrailingComments (flip append c) a

toLabeled :: forall a b c. ToName a b => SourceToken -> Tuple a c -> Labeled (Name b) c
toLabeled tok (Tuple lbl value) = Labeled
  { label: toName lbl
  , separator: tok
  , value
  }
