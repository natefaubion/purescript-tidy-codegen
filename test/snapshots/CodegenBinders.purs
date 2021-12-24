module CodegenBinders where

import Prelude

import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Module)
import Test.Util (log)
import Tidy.Codegen (binaryOp, binderCtor, binderInt, binderNamed, binderNumber, binderOp, binderTyped, binderVar, caseBranch, declValue, exprBool, exprCase, exprIdent, exprInt, exprLambda, exprNumber, exprSection, module_, printModule, typeCtor)

test :: Module Void
test = unsafePartial do
  module_ "Test.Binders" [] []
    [ declValue "lamBinderCtor0" [] do
        exprLambda [ binderCtor "Constructor" [] ]
          (exprBool true)
    , declValue "lamBinderCtor1" [] do
        exprLambda [ binderCtor "Constructor" [ binderVar "a" ] ]
          (exprIdent "a")
    , declValue "lamBinderNamed" [] do
        exprLambda [ binderNamed "alpha" (binderVar "a") ]
          (exprIdent "a")
    , declValue "lamBinderTyped" [] do
        exprLambda [ binderTyped (binderVar "a") (typeCtor "Type") ]
          (exprIdent "a")
    , declValue "lamBinderOp" [] do
        exprLambda [ binderOp (binderVar "a") [ binaryOp "/\\" (binderVar "b") ]  ]
          (exprIdent "a")
    , declValue "lamBinderNegInt" [] do
        exprLambda [ binderInt (-3) ]
          (exprInt (-3))
    , declValue "lamBinderNegNum" [] do
        exprLambda [ binderNumber (-3.0) ]
          (exprNumber (-3.0))
    , declValue "caseBinderCtor0" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderCtor "Constructor" [] ]
              (exprBool true)
          ]
    , declValue "caseBinderCtor1" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderCtor "Constructor" [ binderVar "a" ] ]
              (exprBool true)
          ]
          ]
    , declValue "caseBinderNamed" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderNamed "alpha" (binderVar "a") ]
              (exprBool true)
          ]
    , declValue "caseBinderTyped" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderTyped (binderVar "a") (typeCtor "Type") ]
              (exprBool true)
          ]
    , declValue "caseBinderOp" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderOp (binderVar "a") [ binaryOp "/\\" (binderVar "b") ] ]
              (exprBool true)
          ]
    , declValue "caseBinderNegInt" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderInt (-3) ]
              (exprBool true)
          ]
    , declValue "caseBinderNegNum" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderNumber (-3.0) ]
              (exprBool true)
          ]
    ]

main :: Effect Unit
main = log $ printModule test
