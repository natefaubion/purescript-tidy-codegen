module CodegenBinders where

import Prelude

import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Module)
import Test.Util (log)
import Tidy.Codegen (binaryOp, binderCtor, binderInt, binderNamed, binderNumber, binderOp, binderTyped, binderVar, caseBranch, declValue, exprBool, exprCase, exprLambda, exprSection, module_, printModule, typeCtor)

test :: Module Void
test = unsafePartial do
  let
    bNegInt = binderInt (-3)
    bNegNum = binderNumber (-3.0)
    bVarA = binderVar "a"
    bVarB = binderVar "b"
    bTyped = binderTyped bVarA (typeCtor "Type")
    bNamed arg = binderNamed "x" arg
    bCtor0 = binderCtor "Constructor" []
    bCtor1 bArg = binderCtor "Constructor" [ bArg ]
    bOp l r = binderOp l [ binaryOp "/\\" r ]
    eBool = exprBool true
  module_ "Test.Binders" [] []
    [ declValue "lamBinderCtor0" [] do
        exprLambda [ bCtor0 ] eBool
    , declValue "lamBinderCtor1" [] do
        exprLambda [ bCtor1 bVarA ] eBool
    , declValue "lamBinderNamedBinderVar" [] do
        exprLambda [ bNamed bVarA ] eBool
    , declValue "lamBinderNamedBinderCtor0" [] do
        exprLambda [ bNamed bCtor0 ] eBool
    , declValue "lamBinderNamedBinderCtor1" [] do
        exprLambda [ bNamed $ bCtor1 bVarA ] eBool
    , declValue "lamBinderNamedBinderOp" [] do
        exprLambda [ bNamed $ bOp bVarA bVarB ] eBool
    , declValue "lamBinderNamedBinderTyped" [] do
        exprLambda [ bNamed bTyped ] eBool
    , declValue "lamBinderNamedBinderNegInt" [] do
        exprLambda [ bNamed bNegInt ] eBool
    , declValue "lamBinderNamedBinderNegNum" [] do
        exprLambda [ bNamed bNegNum ] eBool
    , declValue "lamBinderTyped" [] do
        exprLambda [ bTyped ] eBool
    , declValue "lamBinderOp" [] do
        exprLambda [ bOp bVarA bVarB ] eBool
    , declValue "lamBinderNegInt" [] do
        exprLambda [ bNegInt ] eBool
    , declValue "lamBinderNegNum" [] do
        exprLambda [ bNegNum ] eBool
    , declValue "caseBinderCtor0" [] do
        exprCase [ exprSection ]
          [ caseBranch [ bCtor0 ] eBool
          ]
    , declValue "caseBinderCtor1" [] do
        exprCase [ exprSection ]
          [ caseBranch [ bCtor1 bVarA ] eBool
          ]
    , declValue "caseBinderNamedBinderVar" [] do
        exprCase [ exprSection ]
          [ caseBranch [ bNamed bVarA ] eBool
          ]
    , declValue "caseBinderNamedBinderCtor0" [] do
        exprCase [ exprSection ]
          [ caseBranch [ bNamed bCtor0 ] eBool
          ]
    , declValue "caseBinderNamedBinderCtor1" [] do
        exprCase [ exprSection ]
          [ caseBranch [ bNamed $ bCtor1 bVarA ] eBool
          ]
    , declValue "caseBinderNamedBinderOp" [] do
        exprCase [ exprSection ]
          [ caseBranch [ bNamed $ bOp bVarA bVarB ] eBool
          ]
    , declValue "caseBinderNamedBinderTyped" [] do
        exprCase [ exprSection ]
          [ caseBranch [ bNamed bTyped ] eBool
          ]
    , declValue "caseBinderNamedBinderNegInt" [] do
        exprCase [ exprSection ]
          [ caseBranch [ bNamed bNegInt ] eBool
          ]
    , declValue "caseBinderNamedBinderNegNum" [] do
        exprCase [ exprSection ]
          [ caseBranch [ bNamed bNegNum ] eBool
          ]
    , declValue "caseBinderTyped" [] do
        exprCase [ exprSection ]
          [ caseBranch [ bTyped ] eBool
          ]
    , declValue "caseBinderOp" [] do
        exprCase [ exprSection ]
          [ caseBranch [ bOp bVarA bVarB ] eBool
          ]
    , declValue "caseBinderNegInt" [] do
        exprCase [ exprSection ]
          [ caseBranch [ bNegInt ] eBool
          ]
    , declValue "caseBinderNegNum" [] do
        exprCase [ exprSection ]
          [ caseBranch [ bNegNum ] eBool
          ]
    ]

main :: Effect Unit
main = log $ printModule test
