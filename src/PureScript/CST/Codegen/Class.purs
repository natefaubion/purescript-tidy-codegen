module PureScript.CST.Codegen.Class where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Tuple (Tuple(..), snd)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Codegen.Common (toSeparated, toSourceToken, tokColon, tokComma, tokForFixity, tokForRole, tokPipe)
import PureScript.CST.Codegen.String (escapeSourceString)
import PureScript.CST.Codegen.Types (GuardedBranch(..), Qualified(..), SymbolName(..))
import PureScript.CST.Lexer (lexToken)
import PureScript.CST.Types (Binder(..), Comment, Declaration(..), Expr(..), Fixity(..), Guarded(..), GuardedExpr(..), Ident(..), ImportDecl(..), Instance(..), IntValue(..), Label(..), Labeled(..), LineFeed, Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), Operator(..), Proper(..), QualifiedName(..), RecordLabeled(..), Role(..), Separated(..), SourceToken, Token(..), Type(..), Where(..), Wrapped(..))
import PureScript.CST.Types as CST
import Safe.Coerce (coerce)
import Type.Equality (class TypeEquals, proof)
import Type.Equality as TypeEquals

newtype ErrorPrefix = ErrorPrefix String

class ToNonEmptyArray f where
  toNonEmptyArray :: forall a. ErrorPrefix -> f a -> NonEmptyArray a

instance Partial => ToNonEmptyArray Array where
  toNonEmptyArray (ErrorPrefix errPrefix) a = case NonEmptyArray.fromArray a of
    Just b -> b
    Nothing -> crashWith $ errPrefix <> ": Array is empty"

instance ToNonEmptyArray NonEmptyArray where
  toNonEmptyArray = const identity

toTokenFromString :: forall a. Partial => FromToken a => ErrorPrefix -> String -> Tuple Token a
toTokenFromString (ErrorPrefix errPrefix) str =
  case lexToken str of
    Right tok | Just val <- fromToken tok ->
      Tuple tok val
    _ ->
      crashWith $ errPrefix <> ": " <> str

class ToToken a b where
  toToken :: a -> Tuple Token b

instance Partial => ToToken String Ident where
  toToken = toTokenFromString (ErrorPrefix "Not an Ident")

instance ToToken Ident Ident where
  toToken str = Tuple (TokLowerName Nothing (unwrap str)) str

instance Partial => ToToken String Proper where
  toToken = toTokenFromString (ErrorPrefix "Not a Proper")

instance ToToken Proper Proper where
  toToken str = Tuple (TokUpperName Nothing (unwrap str)) str

instance Partial => ToToken String Operator where
  toToken = toTokenFromString (ErrorPrefix "Not an Operator")

instance ToToken Operator Operator where
  toToken str = Tuple (TokOperator Nothing (unwrap str)) str

instance Partial => ToToken String SymbolName where
  toToken str = case lexToken str of
    Right tok@(TokSymbolName Nothing sym) -> Tuple tok (SymbolName sym)
    Right (TokOperator Nothing sym) -> Tuple (TokSymbolName Nothing sym) (SymbolName str)
    _ -> crashWith $ "Not a SymbolName: " <> str

instance ToToken SymbolName SymbolName where
  toToken str = Tuple (TokSymbolName Nothing (unwrap str)) str

instance ToToken SymbolName Operator where
  toToken str = Tuple (TokSymbolName Nothing (unwrap str)) (Operator (unwrap str))

instance Partial => ToToken String ModuleName where
  toToken = toTokenFromString (ErrorPrefix "Not a ModuleName")

instance ToToken String Label where
  toToken str = case lexToken str of
    Right tok@(TokLowerName Nothing lbl) -> Tuple tok (Label lbl)
    _ -> toToken (Label str)

instance ToToken Label Label where
  toToken (Label lbl) = Tuple (TokString (unwrap (escapeSourceString lbl)) lbl) (Label lbl)

instance ToToken ModuleName ModuleName where
  toToken = unsafePartial (toToken <<< unwrap)

instance ToToken Int IntValue where
  toToken n = Tuple (TokInt (show n) (SmallInt n)) (SmallInt n)

instance ToToken Int Int where
  toToken n = Tuple (TokInt (show n) (SmallInt n)) n

instance ToToken Number Number where
  toToken n = Tuple (TokNumber (show n) n) n

instance ToToken String String where
  toToken str = Tuple (TokString (unwrap (escapeSourceString str)) str) str

instance ToToken Boolean Boolean where
  toToken b = Tuple (TokLowerName Nothing (if b then "true" else "false")) b

instance ToToken Fixity Fixity where
  toToken fixity = Tuple (tokForFixity fixity).value fixity

instance ToToken Role Role where
  toToken role = Tuple (tokForRole role).value role

instance Partial => ToToken String (Qualified Ident) where
  toToken = toTokenFromString (ErrorPrefix "Not a Qualified dIdent")

instance ToToken (Qualified Ident) (Qualified Ident) where
  toToken qual@(Qualified mn str) = Tuple (TokLowerName mn (unwrap str)) qual

instance Partial => ToToken String (Qualified Proper) where
  toToken = toTokenFromString (ErrorPrefix "Not a Qualified Proper")

instance ToToken (Qualified Proper) (Qualified Proper) where
  toToken qual@(Qualified mn str) = Tuple (TokUpperName mn (unwrap str)) qual

instance Partial => ToToken String (Qualified Operator) where
  toToken = toTokenFromString (ErrorPrefix "Not a Qualified Operator")

instance ToToken (Qualified Operator) (Qualified Operator) where
  toToken qual@(Qualified mn str) = Tuple (TokOperator mn (unwrap str)) qual

instance Partial => ToToken String (Qualified SymbolName) where
  toToken = toTokenFromString (ErrorPrefix "Not a Qualified SymbolName")

instance ToToken (Qualified SymbolName) (Qualified SymbolName) where
  toToken qual@(Qualified mn str) = Tuple (TokSymbolName mn (unwrap str)) qual

instance ToToken (QualifiedName a) (Qualified a) where
  toToken (QualifiedName qual) = Tuple qual.token.value (Qualified qual.module qual.name)

class ToName a b where
  toName :: a -> Name b

defaultToName :: forall a b. ToToken a b => a -> Name b
defaultToName val = do
  let (Tuple tok name) = toToken val
  Name { name, token: toSourceToken tok }

instance Partial => ToName String Ident where
  toName = defaultToName

instance ToName Ident Ident where
  toName = defaultToName

instance ToName (Name Ident) Ident where
  toName = identity

instance Partial => ToName String Proper where
  toName = defaultToName

instance ToName Proper Proper where
  toName = defaultToName

instance ToName (Name Proper) Proper where
  toName = identity

instance Partial => ToName String Operator where
  toName = defaultToName

instance ToName Operator Operator where
  toName = defaultToName

instance ToName (Name Operator) Operator where
  toName = identity

instance Partial => ToName String SymbolName where
  toName = defaultToName

instance ToName SymbolName SymbolName where
  toName = defaultToName

instance ToName Operator SymbolName where
  toName str = Name { name: coerce str, token: toSourceToken (TokSymbolName Nothing (unwrap str)) }

instance ToName (Name Operator) SymbolName where
  toName (Name { name, token }) = Name
    { name: coerce name
    , token:
        case token.value of
          TokOperator mn op ->
            token { value = TokSymbolName mn op }
          _ -> token
    }

instance ToName String Label where
  toName = defaultToName

instance ToName Label Label where
  toName = defaultToName

instance Partial => ToName String ModuleName where
  toName = defaultToName

instance ToName ModuleName ModuleName where
  toName = defaultToName

instance ToName (QualifiedName a) a where
  toName (QualifiedName { name, token }) = Name { name, token }

class FromToken b where
  fromToken :: Token -> Maybe b

instance FromToken Ident where
  fromToken = case _ of
    TokLowerName Nothing str -> Just (Ident str)
    _ -> Nothing

instance FromToken (Qualified Ident) where
  fromToken = case _ of
    TokLowerName qual str -> Just (Qualified qual (Ident str))
    _ -> Nothing

instance FromToken Proper where
  fromToken = case _ of
    TokUpperName Nothing str -> Just (Proper str)
    _ -> Nothing

instance FromToken (Qualified Proper) where
  fromToken = case _ of
    TokUpperName qual str -> Just (Qualified qual (Proper str))
    _ -> Nothing

instance FromToken Operator where
  fromToken = case _ of
    TokOperator Nothing str -> Just (Operator str)
    _ -> Nothing

instance FromToken (Qualified Operator) where
  fromToken = case _ of
    TokOperator qual str -> Just (Qualified qual (Operator str))
    _ -> Nothing

instance FromToken SymbolName where
  fromToken = case _ of
    TokSymbolName Nothing str -> Just (SymbolName str)
    _ -> Nothing

instance FromToken (Qualified SymbolName) where
  fromToken = case _ of
    TokSymbolName qual str -> Just (Qualified qual (SymbolName str))
    _ -> Nothing

instance FromToken ModuleName where
  fromToken = case _ of
    TokUpperName (Just qual) str -> Just (ModuleName ((unwrap qual) <> "." <> str))
    TokUpperName Nothing str -> Just (ModuleName str)
    _ -> Nothing

instance FromToken Label where
  fromToken = case _ of
    TokLowerName Nothing str -> Just (Label str)
    TokString _ str -> Just (Label str)
    TokRawString str -> Just (Label str)
    _ -> Nothing

instance FromToken IntValue where
  fromToken = case _ of
    TokInt _ int -> Just int
    _ -> Nothing

instance FromToken Number where
  fromToken = case _ of
    TokNumber _ num -> Just num
    _ -> Nothing

instance FromToken Boolean where
  fromToken = case _ of
    TokLowerName Nothing "true" -> Just true
    TokLowerName Nothing "false" -> Just false
    _ -> Nothing

instance FromToken Fixity where
  fromToken = case _ of
    TokLowerName Nothing "infix" -> Just Infix
    TokLowerName Nothing "infixl" -> Just Infixr
    TokLowerName Nothing "infixr" -> Just Infixr
    _ -> Nothing

instance FromToken Role where
  fromToken = case _ of
    TokLowerName Nothing "nominal" -> Just Nominal
    TokLowerName Nothing "representational" -> Just Representational
    TokLowerName Nothing "phantom" -> Just Phantom
    _ -> Nothing

toQualified :: forall a. Partial => FromToken (Qualified a) => String -> Qualified a
toQualified = snd <<< toTokenFromString (ErrorPrefix "Not Qualified")

class ToQualifiedName a b where
  toQualifiedName :: a -> QualifiedName b

defaultToQualifiedName :: forall a b. ToToken a b => Qualified a -> QualifiedName b
defaultToQualifiedName (Qualified mn a) = do
  let (Tuple tok (name :: b)) = toToken a
  QualifiedName { module: mn, name, token: toSourceToken tok }

instance (Partial, FromToken (Qualified a)) => ToQualifiedName String a where
  toQualifiedName str = case lexToken str of
    Right tok | Just (Qualified mn a) <- fromToken tok ->
      QualifiedName { module: mn, name: a, token: toSourceToken tok }
    _ -> crashWith $ "Not a QualifiedName: " <> str

instance ToToken a b => ToQualifiedName (Qualified a) b where
  toQualifiedName = defaultToQualifiedName

instance ToQualifiedName Ident Ident where
  toQualifiedName = toQualifiedName <<< Qualified Nothing

instance ToQualifiedName Proper Proper where
  toQualifiedName = toQualifiedName <<< Qualified Nothing

instance ToQualifiedName Operator Operator where
  toQualifiedName = toQualifiedName <<< Qualified Nothing

instance ToQualifiedName SymbolName Operator where
  toQualifiedName = toQualifiedName <<< Qualified Nothing

instance ToQualifiedName (Name a) a where
  toQualifiedName (Name { name, token }) = QualifiedName { module: Nothing, name, token }

instance ToQualifiedName (QualifiedName a) a where
  toQualifiedName = identity

class ToModuleName a where
  toModuleName :: a -> ModuleName

instance Partial => ToModuleName String where
  toModuleName = snd <<< toToken

instance ToModuleName ModuleName where
  toModuleName = identity

class ToRecordLabeled a b where
  toRecordLabeled :: a -> RecordLabeled b

instance Partial => ToRecordLabeled String b where
  toRecordLabeled = RecordPun <<< toName

instance ToRecordLabeled Ident b where
  toRecordLabeled = RecordPun <<< toName

instance ToRecordLabeled (Name Ident) b where
  toRecordLabeled = RecordPun

instance (ToName a Label, TypeEquals b c) => ToRecordLabeled (Tuple a b) c where
  toRecordLabeled (Tuple field value) =
    RecordField (toName field) tokColon (TypeEquals.to value)

instance TypeEquals a b => ToRecordLabeled (RecordLabeled a) b where
  toRecordLabeled = proof

class ToWhere a e | a -> e where
  toWhere :: a -> Where e

instance ToWhere (Expr e) e where
  toWhere = Where <<< { bindings: Nothing, expr: _ }

instance ToWhere (Where e) e where
  toWhere = identity

class ToGuarded a e | a -> e where
  toGuarded :: SourceToken -> a -> Guarded e

instance ToGuarded (Expr e) e where
  toGuarded tok = Unconditional tok <<< toWhere

instance ToGuarded (Where e) e where
  toGuarded = Unconditional

instance TypeEquals a (GuardedBranch e) => ToGuarded (NonEmptyArray a) e where
  toGuarded tok = Guarded <<< map (go <<< TypeEquals.to)
    where
    go (GuardedBranch pats wh) = GuardedExpr
      { bar: tokPipe
      , patterns: toSeparated tokComma pats
      , separator: tok
      , where: wh
      }

instance (Partial, TypeEquals a (GuardedBranch e)) => ToGuarded (Array a) e where
  toGuarded tok = toGuarded tok <<< toNonEmptyArray (ErrorPrefix "ToGuarded")

class ToFixityName a where
  toFixityName :: a -> QualifiedName (Either Ident Proper)

instance Partial => ToFixityName String where
  toFixityName str = case lexToken str of
    Right tok@(TokLowerName mn name) ->
      QualifiedName { module: mn, name: Left (Ident name), token: toSourceToken tok }
    Right tok@(TokUpperName mn name) ->
      QualifiedName { module: mn, name: Right (Proper name), token: toSourceToken tok }
    _ ->
      crashWith $ "Not a fixity name: " <> str

instance ToFixityName Ident where
  toFixityName = over QualifiedName (\a -> a { name = Left a.name }) <<< toQualifiedName

instance ToFixityName Proper where
  toFixityName = over QualifiedName (\a -> a { name = Right a.name }) <<< toQualifiedName

type LeadingComments r = (leadingComments :: Array (Comment LineFeed) | r)

type TrailingComments trl r = (trailingComments :: Array (Comment trl) | r)

class OverLeadingComments a where
  overLeadingComments :: (Array (Comment LineFeed) -> Array (Comment LineFeed)) -> a -> a

class OverTrailingComments a trl | a -> trl where
  overTrailingComments :: (Array (Comment trl) -> Array (Comment trl)) -> a -> a

instance OverLeadingComments Void where
  overLeadingComments _ = absurd

instance OverTrailingComments Void LineFeed where
  overTrailingComments _ = absurd

instance TypeEquals r (LeadingComments r') => OverLeadingComments (Record r) where
  overLeadingComments k r = do
    let r' = coerce r :: Record (LeadingComments r')
    coerce r' { leadingComments = k r'.leadingComments } :: Record r

instance TypeEquals r (TrailingComments trl r') => OverTrailingComments (Record r) trl where
  overTrailingComments k r = do
    let r' = coerce r :: Record (TrailingComments trl r')
    coerce r' { trailingComments = k r'.trailingComments } :: Record r

instance OverLeadingComments (ModuleHeader e) where
  overLeadingComments k (ModuleHeader m) =
    ModuleHeader m { keyword = overLeadingComments k m.keyword }

instance OverTrailingComments (ModuleBody e) LineFeed where
  overTrailingComments k (ModuleBody m) =
    ModuleBody m { trailingComments = k m.trailingComments }

instance OverLeadingComments (Module e) where
  overLeadingComments k (Module m) =
    Module m { header = overLeadingComments k m.header }

instance OverTrailingComments (Module e) LineFeed where
  overTrailingComments k (Module m) =
    Module m { body = overTrailingComments k m.body }

instance OverLeadingComments (ImportDecl e) where
  overLeadingComments k (ImportDecl a) = ImportDecl $ a { keyword = overLeadingComments k a.keyword }

instance OverLeadingComments e => OverLeadingComments (Declaration e) where
  overLeadingComments k = case _ of
    DeclData a b -> DeclData (a { keyword = overLeadingComments k a.keyword }) b
    DeclType a b c -> DeclType (a { keyword = overLeadingComments k a.keyword }) b c
    DeclNewtype a b c d -> DeclNewtype (a { keyword = overLeadingComments k a.keyword }) b c d
    DeclClass a b -> DeclClass (a { keyword = overLeadingComments k a.keyword }) b
    DeclInstanceChain a -> DeclInstanceChain (overLeadingComments k a)
    DeclDerive a b c -> DeclDerive (overLeadingComments k a) b c
    DeclKindSignature a b -> DeclKindSignature (overLeadingComments k a) b
    DeclSignature a -> DeclSignature (overLeadingComments k a)
    DeclValue a -> DeclValue $ a { name = overLeadingComments k a.name }
    DeclFixity a -> DeclFixity $ a { keyword = lmap (overLeadingComments k) a.keyword }
    DeclForeign a b c -> DeclForeign (overLeadingComments k a) b c
    DeclRole a b c d -> DeclRole (overLeadingComments k a) b c d
    DeclError e -> DeclError (overLeadingComments k e)

instance OverLeadingComments (Instance e) where
  overLeadingComments k (Instance a) = Instance $ a { head { keyword = overLeadingComments k a.head.keyword } }

instance OverLeadingComments e => OverLeadingComments (CST.Type e) where
  overLeadingComments k = case _ of
    TypeVar a -> TypeVar (overLeadingComments k a)
    TypeConstructor a -> TypeConstructor (overLeadingComments k a)
    TypeWildcard a -> TypeWildcard (overLeadingComments k a)
    TypeHole a -> TypeHole (overLeadingComments k a)
    TypeString a b -> TypeString (overLeadingComments k a) b
    TypeRow a -> TypeRow (overLeadingComments k a)
    TypeRecord a -> TypeRecord (overLeadingComments k a)
    TypeForall a b c d -> TypeForall (overLeadingComments k a) b c d
    TypeKinded a b c -> TypeKinded (overLeadingComments k a) b c
    TypeApp a b -> TypeApp (overLeadingComments k a) b
    TypeOp a b -> TypeOp (overLeadingComments k a) b
    TypeOpName a -> TypeOpName (overLeadingComments k a)
    TypeArrow a b c -> TypeArrow (overLeadingComments k a) b c
    TypeArrowName a -> TypeArrowName (overLeadingComments k a)
    TypeConstrained a b c -> TypeConstrained (overLeadingComments k a) b c
    TypeParens a -> TypeParens (overLeadingComments k a)
    TypeUnaryRow a b -> TypeUnaryRow (overLeadingComments k a) b
    TypeError e -> TypeError (overLeadingComments k e)

instance OverLeadingComments e => OverLeadingComments (Expr e) where
  overLeadingComments k = case _ of
    ExprHole a -> ExprHole (overLeadingComments k a)
    ExprSection a -> ExprSection (overLeadingComments k a)
    ExprIdent a -> ExprIdent (overLeadingComments k a)
    ExprConstructor a -> ExprConstructor (overLeadingComments k a)
    ExprBoolean a b -> ExprBoolean (overLeadingComments k a) b
    ExprChar a b -> ExprChar (overLeadingComments k a) b
    ExprString a b -> ExprString (overLeadingComments k a) b
    ExprInt a b -> ExprInt (overLeadingComments k a) b
    ExprNumber a b -> ExprNumber (overLeadingComments k a) b
    ExprArray a -> ExprArray (overLeadingComments k a)
    ExprRecord a -> ExprRecord (overLeadingComments k a)
    ExprParens a -> ExprParens (overLeadingComments k a)
    ExprTyped a b c -> ExprTyped (overLeadingComments k a) b c
    ExprInfix a b -> ExprInfix (overLeadingComments k a) b
    ExprOp a b -> ExprOp (overLeadingComments k a) b
    ExprOpName a -> ExprOpName (overLeadingComments k a)
    ExprNegate a b -> ExprNegate (overLeadingComments k a) b
    ExprRecordAccessor a -> ExprRecordAccessor a { expr = overLeadingComments k a.expr }
    ExprRecordUpdate a b -> ExprRecordUpdate (overLeadingComments k a) b
    ExprApp a b -> ExprApp (overLeadingComments k a) b
    ExprLambda a -> ExprLambda a { symbol = overLeadingComments k a.symbol }
    ExprIf a -> ExprIf a { keyword = overLeadingComments k a.keyword }
    ExprCase a -> ExprCase a { keyword = overLeadingComments k a.keyword }
    ExprLet a -> ExprLet a { keyword = overLeadingComments k a.keyword }
    ExprDo a -> ExprDo a { keyword = overLeadingComments k a.keyword }
    ExprAdo a -> ExprAdo a { keyword = overLeadingComments k a.keyword }
    ExprError e -> ExprError (overLeadingComments k e)

instance OverLeadingComments e => OverLeadingComments (Binder e) where
  overLeadingComments k = case _ of
    BinderWildcard a -> BinderWildcard (overLeadingComments k a)
    BinderVar a -> BinderVar (overLeadingComments k a)
    BinderNamed a b c -> BinderNamed (overLeadingComments k a) b c
    BinderConstructor a b -> BinderConstructor (overLeadingComments k a) b
    BinderBoolean a b -> BinderBoolean (overLeadingComments k a) b
    BinderChar a b -> BinderChar (overLeadingComments k a) b
    BinderString a b -> BinderString (overLeadingComments k a) b
    BinderInt (Just a) b c -> BinderInt (Just (overLeadingComments k a)) b c
    BinderInt _ b c -> BinderInt Nothing (overLeadingComments k b) c
    BinderNumber (Just a) b c -> BinderNumber (Just (overLeadingComments k a)) b c
    BinderNumber _ b c -> BinderNumber Nothing (overLeadingComments k b) c
    BinderArray a -> BinderArray (overLeadingComments k a)
    BinderRecord a -> BinderRecord (overLeadingComments k a)
    BinderParens a -> BinderParens (overLeadingComments k a)
    BinderTyped a b c -> BinderTyped (overLeadingComments k a) b c
    BinderOp a b -> BinderOp (overLeadingComments k a) b
    BinderError e -> BinderError (overLeadingComments k e)

instance OverLeadingComments (Name a) where
  overLeadingComments k (Name a) = Name $ a { token = overLeadingComments k a.token }

instance OverLeadingComments (QualifiedName a) where
  overLeadingComments k (QualifiedName a) = QualifiedName $ a { token = overLeadingComments k a.token }

instance OverLeadingComments a => OverLeadingComments (Labeled a b) where
  overLeadingComments k (Labeled a) = Labeled $ a { label = overLeadingComments k a.label }

instance OverLeadingComments a => OverLeadingComments (Separated a) where
  overLeadingComments k (Separated { head, tail }) = Separated { head: overLeadingComments k head, tail }

instance OverLeadingComments (Wrapped a) where
  overLeadingComments k (Wrapped a) = Wrapped $ a { open = overLeadingComments k a.open }

instance OverLeadingComments a => OverLeadingComments (Tuple a b) where
  overLeadingComments k (Tuple a b) = Tuple (overLeadingComments k a) b
