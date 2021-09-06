------------------------------------
-- This module is code generated. --
--          DO NOT EDIT!          --
------------------------------------
module CodegenExamples where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Module, Fixity(..))
import Tidy.Codegen (binaryOp, binderCtor, binderRecord, binderVar, binderWildcard, caseBranch, classMember, dataCtor, declClass, declData, declDerive, declDeriveNewtype, declInfix, declInstance, declInstanceChain, declNewtype, declType, declValue, doBind, doDiscard, doLet, exprApp, exprArray, exprBool, exprCase, exprChar, exprCtor, exprDo, exprDot, exprIdent, exprIf, exprInfix, exprInt, exprIntHex, exprLambda, exprLet, exprNumber, exprOp, exprOpName, exprRecord, exprString, exprTyped, exprUpdate, exprWhere, guardBranch, guardExpr, letBinder, letSignature, letValue, module_, printModule, typeApp, typeArrow, typeConstrained, typeCtor, typeForall, typeKinded, typeOp, typeOpName, typeRecord, typeRow, typeVar, update, updateNested)

test :: Module Void
test = unsafePartial
  ( module_ "CodegenExamples" [] []
      [ declValue "binaryOpExample" []
          ( exprOp (exprInt 4)
              [ binaryOp "+" (exprIdent "a")
              , binaryOp "/" (exprIdent "b")
              ]
          )
      , declType "TypeCtorExample" [] (typeApp (typeCtor "Maybe") [ typeCtor "Int" ])
      , declType "TypeRowExample" []
          ( typeRow
              [ Tuple "id" (typeCtor "UserId")
              , Tuple "name" (typeCtor "String")
              , Tuple "age" (typeCtor "Int")
              ]
              (Just (typeVar "r"))
          )
      , declType "TypeRecordExample" []
          ( typeRecord
              [ Tuple "id" (typeCtor "UserId")
              , Tuple "name" (typeCtor "String")
              , Tuple "age" (typeCtor "Int")
              ]
              (Just (typeVar "r"))
          )
      , declType "TypeKindedExample" []
          ( typeKinded (typeCtor "Maybe")
              (typeArrow [ typeCtor "Type" ] (typeCtor "Type"))
          )
      , declType "TypeAppExample" []
          ( typeApp (typeCtor "Map")
              [ typeCtor "UserId"
              , typeCtor "User"
              ]
          )
      , declType "TypeOpExample" []
          ( typeOp (typeCtor "String")
              [ binaryOp "/\\" (typeCtor "Int")
              , binaryOp "/\\" (typeCtor "Boolean")
              ]
          )
      , declType "TypeOpNameExample" [] (typeOpName "(~>)")
      , declType "TypeForallExample" []
          ( typeForall [ typeVar "a" ]
              (typeArrow [ typeVar "a" ] (typeVar "a"))
          )
      , declType "TypeConstrainedExample" []
          ( typeForall [ typeVar "f", typeVar "a" ]
              ( typeConstrained
                  [ typeApp (typeCtor "Functor") [ typeVar "f" ]
                  , typeApp (typeCtor "Show") [ typeVar "a" ]
                  ]
                  ( typeArrow
                      [ typeApp (typeVar "f")
                          [ typeVar "a" ]
                      ]
                      ( typeApp (typeVar "f")
                          [ typeCtor "String" ]
                      )
                  )
              )
          )
      , declType "TypeArrowExample" []
          ( typeArrow
              [ typeCtor "UserId"
              , typeCtor "String"
              , typeCtor "Int"
              ]
              (typeCtor "User")
          )
      , declValue "exprIdentExample" []
          ( exprApp (exprIdent "Map.lookup")
              [ exprIdent "userId"
              , exprIdent "users"
              ]
          )
      , declValue "exprCtorExample" []
          ( exprApp (exprCtor "List.Cons")
              [ exprIdent "a"
              , exprCtor "List.Nil"
              ]
          )
      , declValue "exprBoolExample" [] (exprBool true)
      , declValue "exprCharExample" [] (exprChar 'A')
      , declValue "exprStringExample" [] (exprString "string")
      , declValue "exprIntExample" [] (exprInt 42)
      , declValue "exprIntHexExample" [] (exprIntHex 0xFF0000)
      , declValue "exprNumberExample" [] (exprNumber 1.618)
      , declValue "exprArrayExample" []
          ( exprArray
              [ exprInt 1
              , exprInt 2
              , exprInt 3
              ]
          )
      , declValue "exprRecordExample" []
          ( exprRecord
              [ Tuple "id" (exprIdent "userId")
              , Tuple "name" (exprIdent "userName")
              , Tuple "age" (exprIdent "userAge")
              ]
          )
      , declValue "exprTypedExample" [] (exprTyped (exprInt 42) (typeCtor "Int"))
      , declValue "exprInfixExample" []
          ( exprInfix (exprIdent "a")
              [ Tuple (exprIdent "append") (exprIdent "b")
              , Tuple (exprIdent "append") (exprIdent "c")
              ]
          )
      , declValue "exprOpExample" []
          ( exprOp (exprString "string")
              [ binaryOp "/\\" (exprInt 42)
              , binaryOp "/\\" (exprBool false)
              ]
          )
      , declValue "exprOpNameExample" [] (exprOpName "(<>)")
      , declValue "exprDotExample" [] (exprDot (exprIdent "response") [ "body", "users" ])
      , declValue "exprUpdateExample" []
          ( exprUpdate (exprIdent "user")
              [ update "age" (exprInt 42)
              , updateNested "phone"
                  [ update "countryCode" (exprInt 1)
                  ]
              ]
          )
      , declValue "exprAppExample" []
          ( exprApp (exprIdent "Map.lookup")
              [ exprIdent "userId"
              , exprIdent "users"
              ]
          )
      , declValue "exprLambdaExample" []
          ( exprLambda [ binderVar "a", binderVar "b" ]
              ( exprOp (exprIdent "a")
                  [ binaryOp "<>" (exprIdent "b") ]
              )
          )
      , declValue "exprIfExample" []
          ( exprIf (exprApp (exprIdent "isLoggedIn") [ exprIdent "user" ])
              (exprIdent "renderPage")
              (exprApp (exprIdent "httpError") [ exprInt 400 ])
          )
      , declValue "exprCaseExample" []
          ( exprCase [ exprIdent "xs" ]
              [ caseBranch [ binderCtor "List.Cons" [ binderVar "x", binderWildcard ] ]
                  ( exprApp (exprCtor "Just")
                      [ exprIdent "x"
                      ]
                  )
              , caseBranch [ binderCtor "Nothing" [] ]
                  (exprCtor "Nothing")
              ]
          )
      , declValue "exprLetExample" []
          ( exprLet
              [ letSignature "countDown" (typeArrow [ typeCtor "Int" ] (typeCtor "Int"))
              , letValue "countDown" [ binderVar "n" ]
                  [ guardBranch [ guardExpr (exprOp (exprIdent "n") [ binaryOp ">" (exprInt 0) ]) ]
                      ( exprApp (exprIdent "countDown")
                          [ exprOp (exprIdent "n") [ binaryOp "-" (exprInt 1) ] ]
                      )
                  , guardBranch [ guardExpr (exprIdent "otherwise") ]
                      (exprIdent "n")
                  ]
              ]
              (exprApp (exprIdent "countDown") [ exprInt 100 ])
          )
      , declValue "exprDoExample" []
          ( exprDo
              [ doBind (binderVar "followers")
                  (exprApp (exprIdent "getFollowers") [ exprIdent "user" ])
              , doBind (binderVar "favorites")
                  (exprApp (exprIdent "getFavorites") [ exprIdent "user" ])
              ]
              ( exprApp (exprIdent "pure")
                  [ exprRecord [ "followers", "favorites" ] ]
              )
          )
      , declValue "letBinderExample" []
          ( exprLet
              [ letBinder (binderRecord [ "name" ])
                  (exprIdent "user")
              ]
              (exprIdent "name")
          )
      , declValue "doLetExample" []
          ( exprDo
              [ doLet
                  [ letBinder (binderRecord [ "age" ])
                      (exprIdent "user")
                  ]
              ]
              (exprIdent "age")
          )
      , declValue "doDiscardExample" []
          ( exprDo
              [ doDiscard
                  ( exprApp (exprIdent "logoutUser")
                      [ exprIdent "user" ]
                  )
              ]
              ( exprApp (exprIdent "pure")
                  [ exprApp (exprIdent "httpStatus")
                      [ exprInt 200 ]
                  ]
              )
          )
      , declValue "doBindExample" []
          ( exprDo
              [ doBind (binderRecord [ "followers" ])
                  (exprApp (exprIdent "getUser") [ exprIdent "user" ])
              ]
              ( exprApp (exprIdent "pure")
                  [ exprIdent "followers" ]
              )
          )
      , declValue "getName" [ binderVar "user" ]
          ( exprWhere (exprIdent "name")
              [ letBinder (binderRecord [ "name" ])
                  (exprIdent "user")
              ]
          )
      , declValue "countDown" [ binderVar "n" ]
          [ guardBranch [ guardExpr (exprOp (exprIdent "n") [ binaryOp ">" (exprInt 0) ]) ]
              ( exprApp (exprIdent "countDown")
                  [ exprOp (exprIdent "n") [ binaryOp "-" (exprInt 1) ] ]
              )
          , guardBranch [ guardExpr (exprIdent "otherwise") ]
              (exprIdent "n")
          ]
      , declValue "binderWildcardExample" []
          ( exprLambda [ binderWildcard ]
              (exprApp (exprIdent "countDown") [ exprInt 100 ])
          )
      , declData "Either" [ typeVar "a", typeVar "b" ]
          [ dataCtor "Left" [ typeVar "a" ]
          , dataCtor "Right" [ typeVar "b" ]
          ]
      , declType "UserFields" [ typeVar "r" ]
          ( typeRow
              [ Tuple "id" (typeCtor "UserId")
              , Tuple "name" (typeCtor "String")
              , Tuple "age" (typeCtor "Int")
              ]
              (Just (typeVar "r"))
          )
      , declNewtype "UserId" [] "UserId" (typeCtor "String")
      , declClass [ typeApp (typeCtor "Eq") [ typeVar "a" ] ] "Ord" [ typeVar "a" ] []
          [ classMember "compare"
              (typeArrow [ typeVar "a", typeVar "a" ] (typeCtor "Ordering"))
          ]
      , declInstanceChain
          [ declInstance Nothing [] "IsTypeEqual"
              [ typeVar "a", typeVar "a", typeCtor "True" ]
              []
          , declInstance Nothing [] "IsTypeEqual"
              [ typeVar "a", typeVar "b", typeCtor "False" ]
              []
          ]
      , declDerive Nothing [] "Eq" [ typeCtor "UserId" ]
      , declDeriveNewtype Nothing [] "Eq" [ typeCtor "UserId" ]
      , declValue "countDown" [ binderVar "n" ]
          [ guardBranch [ guardExpr (exprOp (exprIdent "n") [ binaryOp ">" (exprInt 0) ]) ]
              ( exprApp (exprIdent "countDown")
                  [ exprOp (exprIdent "n") [ binaryOp "-" (exprInt 1) ] ]
              )
          , guardBranch [ guardExpr (exprIdent "otherwise") ]
              (exprIdent "n")
          ]
      , declInfix Infixl 4 "map" "<$>"
      , declInfix Infixr 0 "RowApply" "+"
      ]
  )

main :: Effect Unit
main = log (printModule test)