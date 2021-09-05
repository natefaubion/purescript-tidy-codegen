module PureScript.CST.Codegen.String where

import Prelude

import Data.Array as Array
import Data.CodePoint.Unicode (GeneralCategory(..), generalCategory)
import Data.Enum (fromEnum)
import Data.Int as Int
import Data.Maybe (maybe)
import Data.Monoid (power)
import Data.Set (Set)
import Data.Set as Set
import Data.String (CodePoint, codePointFromChar)
import Data.String as String
import Data.String.CodeUnits as SCU
import PureScript.CST.Codegen.Types (SourceString(..))

escapeSourceString :: String -> SourceString
escapeSourceString = SourceString <<< Array.foldMap escape <<< String.toCodePointArray
  where
  escape :: CodePoint -> String
  escape cp
    | cp == codePointFromChar '\n' = "\\n"
    | cp == codePointFromChar '\r' = "\\r"
    | cp == codePointFromChar '\t' = "\\t"
    | cp == codePointFromChar '\\' = "\\\\"
    | cp == codePointFromChar '"' = "\\\""
    | shouldPrint cp = String.singleton cp
    | otherwise = "\\x" <> toHex cp

  toHex :: CodePoint -> String
  toHex cp = do
    let hex = Int.toStringAs Int.hexadecimal (fromEnum cp)
    power "0" (6 - SCU.length hex) <> hex

  shouldPrint :: CodePoint -> Boolean
  shouldPrint = maybe false (flip Set.member categories) <<< generalCategory

  categories :: Set GeneralCategory
  categories = Set.fromFoldable
    [ UppercaseLetter
    , LowercaseLetter
    , TitlecaseLetter
    , OtherLetter
    , DecimalNumber
    , LetterNumber
    , OtherNumber
    , ConnectorPunctuation
    , DashPunctuation
    , OpenPunctuation
    , ClosePunctuation
    , InitialQuote
    , FinalQuote
    , OtherPunctuation
    , MathSymbol
    , CurrencySymbol
    , ModifierSymbol
    , OtherSymbol
    ]

