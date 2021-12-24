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
    , declValue "lamBinderNamedBinderVar" [] do
        exprLambda [ binderNamed "alpha" (binderVar "a") ]
          (exprIdent "a")
    , declValue "lamBinderNamedBinderCtor0" [] do
        exprLambda [ binderNamed "alpha" (binderCtor "Constructor" []) ]
          (exprIdent "a")
    , declValue "lamBinderNamedBinderCtor1" [] do
        exprLambda [ binderNamed "alpha" (binderCtor "Constructor" [ binderVar "a" ]) ]
          (exprIdent "a")
    , declValue "lamBinderNamedBinderOp" [] do
        exprLambda [ binderNamed "alpha" (binderOp (binderVar "a") [ binaryOp "/\\" $ binderVar "b" ]) ]
          (exprIdent "a")
    , declValue "lamBinderNamedBinderTyped" [] do
        exprLambda [ binderNamed "alpha" (binderTyped (binderVar "a") (typeCtor "Type")) ]
          (exprIdent "a")
    , declValue "lamBinderNamedBinderNegInt" [] do
        exprLambda [ binderNamed "alpha" (binderInt (-3)) ]
          (exprIdent "a")
    , declValue "lamBinderNamedBinderNegNum" [] do
        exprLambda [ binderNamed "alpha" (binderNumber (-3.0)) ]
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
    , declValue "caseBinderNamedBinderVar" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderNamed "alpha" (binderVar "a") ]
              (exprBool true)
          ]
    , declValue "caseBinderNamedBinderCtor0" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderNamed "alpha" (binderCtor "Constructor" []) ]
              (exprBool true)
          ]
    , declValue "caseBinderNamedBinderCtor1" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderNamed "alpha" (binderCtor "Constructor" [ binderVar "a" ]) ]
              (exprBool true)
          ]
    , declValue "caseBinderNamedBinderOp" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderNamed "alpha" (binderOp (binderVar "a") [ binaryOp "/\\" $ binderVar "b" ]) ]
              (exprBool true)
          ]
    , declValue "caseBinderNamedBinderTyped" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderNamed "alpha" (binderTyped (binderVar "a") (typeCtor "Type")) ]
              (exprBool true)
          ]
    , declValue "caseBinderNamedBinderNegInt" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderNamed "alpha" (binderInt (-3)) ]
              (exprBool true)
          ]
    , declValue "caseBinderNamedBinderNegNum" [] do
        exprCase [ exprSection ]
          [ caseBranch [ binderNamed "alpha" (binderNumber (-3.0)) ]
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
