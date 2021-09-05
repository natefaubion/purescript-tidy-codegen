module PureScript.CST.Codegen.Types where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import PureScript.CST.Types (Ident, Labeled, ModuleName, Name, Operator, PatternGuard, QualifiedName, Where)
import PureScript.CST.Types as CST

newtype SymbolName = SymbolName String

derive instance Newtype SymbolName _
derive newtype instance Eq SymbolName
derive newtype instance Ord SymbolName

data Qualified a = Qualified (Maybe ModuleName) a

derive instance Functor Qualified

unQualified :: forall a. Qualified a -> a
unQualified (Qualified _ a) = a

newtype BinaryOp a = BinaryOp (Tuple (QualifiedName Operator) a)

derive newtype instance Functor BinaryOp

data GuardedBranch e = GuardedBranch (NonEmptyArray (PatternGuard e)) (Where e)

type ClassMember e = Labeled (Name Ident) (CST.Type e)

newtype SourceString = SourceString String

derive instance Newtype SourceString _
derive newtype instance Eq SourceString
derive newtype instance Ord SourceString
