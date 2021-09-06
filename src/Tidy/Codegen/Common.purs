module Tidy.Codegen.Common where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.CST.Types (Delimited, DelimitedNonEmpty, Fixity(..), OneOrDelimited(..), Role(..), Separated(..), SourceRange, SourceStyle(..), SourceToken, Token(..), Wrapped(..))

zeroRange :: SourceRange
zeroRange = zero

toSourceToken :: Token -> SourceToken
toSourceToken =
  { range: zeroRange
  , leadingComments: []
  , trailingComments: []
  , value: _
  }

tokUnderscore :: SourceToken
tokUnderscore = toSourceToken TokUnderscore

tokPipe :: SourceToken
tokPipe = toSourceToken TokPipe

tokColon :: SourceToken
tokColon = toSourceToken (TokOperator Nothing ":")

tokDoubleColon :: SourceToken
tokDoubleColon = toSourceToken (TokDoubleColon ASCII)

tokForall :: SourceToken
tokForall = toSourceToken (TokForall ASCII)

tokSymbolArrow :: SourceToken
tokSymbolArrow = toSourceToken (TokSymbolArrow ASCII)

tokRightParen :: SourceToken
tokRightParen = toSourceToken TokRightParen

tokLeftParen :: SourceToken
tokLeftParen = toSourceToken TokLeftParen

tokRightBrace :: SourceToken
tokRightBrace = toSourceToken TokRightBrace

tokLeftBrace :: SourceToken
tokLeftBrace = toSourceToken TokLeftBrace

tokRightSquare :: SourceToken
tokRightSquare = toSourceToken TokRightSquare

tokLeftSquare :: SourceToken
tokLeftSquare = toSourceToken TokLeftSquare

tokTick :: SourceToken
tokTick = toSourceToken TokTick

tokNegate :: SourceToken
tokNegate = toSourceToken (TokOperator Nothing "-")

tokDot :: SourceToken
tokDot = toSourceToken TokDot

tokComma :: SourceToken
tokComma = toSourceToken TokComma

tokEquals :: SourceToken
tokEquals = toSourceToken TokEquals

tokBackslash :: SourceToken
tokBackslash = toSourceToken TokBackslash

tokRightArrow :: SourceToken
tokRightArrow = toSourceToken (TokRightArrow ASCII)

tokRightFatArrow :: SourceToken
tokRightFatArrow = toSourceToken (TokRightFatArrow ASCII)

tokLeftArrow :: SourceToken
tokLeftArrow = toSourceToken (TokLeftArrow ASCII)

tokLeftFatArrow :: SourceToken
tokLeftFatArrow = toSourceToken (TokOperator Nothing "<=")

tokAt :: SourceToken
tokAt = toSourceToken TokAt

tokAll :: SourceToken
tokAll = toSourceToken (TokSymbolName Nothing "..")

tokIf :: SourceToken
tokIf = tokKeyword "if"

tokThen :: SourceToken
tokThen = tokKeyword "then"

tokElse :: SourceToken
tokElse = tokKeyword "else"

tokCase :: SourceToken
tokCase = tokKeyword "case"

tokOf :: SourceToken
tokOf = tokKeyword "of"

tokWhere :: SourceToken
tokWhere = tokKeyword "where"

tokData :: SourceToken
tokData = tokKeyword "data"

tokClass :: SourceToken
tokClass = tokKeyword "class"

tokNewtype :: SourceToken
tokNewtype = tokKeyword "newtype"

tokType :: SourceToken
tokType = tokKeyword "type"

tokInstance :: SourceToken
tokInstance = tokKeyword "instance"

tokDerive :: SourceToken
tokDerive = tokKeyword "derive"

tokInfix :: SourceToken
tokInfix = tokKeyword "infix"

tokInfixl :: SourceToken
tokInfixl = tokKeyword "infixl"

tokInfixr :: SourceToken
tokInfixr = tokKeyword "infixr"

tokRole :: SourceToken
tokRole = tokKeyword "role"

tokRepresentational :: SourceToken
tokRepresentational = tokKeyword "representational"

tokPhantom :: SourceToken
tokPhantom = tokKeyword "phantom"

tokNominal :: SourceToken
tokNominal = tokKeyword "nominal"

tokImport :: SourceToken
tokImport = tokKeyword "import"

tokAs :: SourceToken
tokAs = tokKeyword "as"

tokDo :: SourceToken
tokDo = tokKeyword "do"

tokAdo :: SourceToken
tokAdo = tokKeyword "ado"

tokLet :: SourceToken
tokLet = tokKeyword "let"

tokIn :: SourceToken
tokIn = tokKeyword "in"

tokModule :: SourceToken
tokModule = tokKeyword "module"

tokTrue :: SourceToken
tokTrue = tokKeyword "true"

tokFalse :: SourceToken
tokFalse = tokKeyword "false"

tokForeign :: SourceToken
tokForeign = tokKeyword "foreign"

tokHiding :: SourceToken
tokHiding = tokKeyword "hiding"

tokKeyword :: String -> SourceToken
tokKeyword = toSourceToken <<< TokLowerName Nothing

tokForFixity :: Fixity -> SourceToken
tokForFixity = case _ of
  Infix -> tokInfix
  Infixl -> tokInfixl
  Infixr -> tokInfixr

tokForRole :: Role -> SourceToken
tokForRole = case _ of
  Nominal -> tokNominal
  Representational -> tokRepresentational
  Phantom -> tokPhantom

toSeparated :: forall a. SourceToken -> NonEmptyArray a -> Separated a
toSeparated tok arr = Separated
  { head: NonEmptyArray.head arr
  , tail: Tuple tok <$> NonEmptyArray.tail arr
  }

toOneOrDelimited :: forall a. NonEmptyArray a -> OneOrDelimited a
toOneOrDelimited arr =
  if NonEmptyArray.length arr == 1 then
    One (NonEmptyArray.head arr)
  else
    Many $ Wrapped
      { close: tokRightParen
      , open: tokLeftParen
      , value: toSeparated tokComma arr
      }

toDelimitedNonEmpty :: forall a. SourceToken -> SourceToken -> SourceToken -> NonEmptyArray a -> DelimitedNonEmpty a
toDelimitedNonEmpty open close sep value = toWrapped open close (toSeparated sep value)

toDelimited :: forall a. SourceToken -> SourceToken -> SourceToken -> Array a -> Delimited a
toDelimited open close sep value = toWrapped open close $ toSeparated sep <$> NonEmptyArray.fromArray value

toWrapped :: forall a. SourceToken -> SourceToken -> a -> Wrapped a
toWrapped open close = Wrapped <<< { open, close, value: _ }

toParenList :: forall a. NonEmptyArray a -> DelimitedNonEmpty a
toParenList = toDelimitedNonEmpty tokLeftParen tokRightParen tokComma
