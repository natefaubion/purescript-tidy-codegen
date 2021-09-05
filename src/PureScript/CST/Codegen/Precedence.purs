module PureScript.CST.Codegen.Precedence where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import PureScript.CST.Codegen.Common (toWrapped, tokLeftParen, tokRightParen)
import PureScript.CST.Types (Binder(..), Expr(..), Type(..))
import PureScript.CST.Types as CST

typeParens :: forall e. CST.Type e -> CST.Type e
typeParens = TypeParens <<< toWrapped tokLeftParen tokRightParen

precType0 :: forall e. CST.Type e -> CST.Type e
precType0 a = case a of
  TypeKinded _ _ _ -> typeParens a
  _ -> a

precType1 :: forall e. CST.Type e -> CST.Type e
precType1 a = case a of
  TypeKinded _ _ _ -> typeParens a
  TypeForall _ _ _ _ -> typeParens a
  TypeArrow _ _ _ -> typeParens a
  TypeConstrained _ _ _ -> typeParens a
  _ -> a

precType2 :: forall e. CST.Type e -> CST.Type e
precType2 a = case a of
  TypeConstrained _ _ _ -> typeParens a
  TypeForall _ _ _ _ -> typeParens a
  TypeArrow _ _ _ -> typeParens a
  TypeKinded _ _ _ -> typeParens a
  TypeOp _ _ -> typeParens a
  _ -> a

precType3 :: forall e. CST.Type e -> CST.Type e
precType3 a = case a of
  TypeConstrained _ _ _ -> typeParens a
  TypeForall _ _ _ _ -> typeParens a
  TypeArrow _ _ _ -> typeParens a
  TypeKinded _ _ _ -> typeParens a
  TypeOp _ _ -> typeParens a
  TypeApp _ _ -> typeParens a
  _ -> a

exprParens :: forall e. Expr e -> Expr e
exprParens = ExprParens <<< toWrapped tokLeftParen tokRightParen

precExpr0 :: forall e. Expr e -> Expr e
precExpr0 a = case a of
  ExprTyped _ _ _ -> exprParens a
  _ -> a

precExpr1 :: forall e. Expr e -> Expr e
precExpr1 a = case a of
  ExprTyped _ _ _ -> exprParens a
  ExprOp _ _ -> exprParens a
  _ -> a

precExprInfix :: forall e. Expr e -> Expr e
precExprInfix a = case a of
  ExprTyped _ _ _ -> exprParens a
  ExprOp _ _ -> exprParens a
  ExprInfix _ _ -> exprParens a
  _ -> a

precExpr2 :: forall e. Expr e -> Expr e
precExpr2 a = case a of
  ExprTyped _ _ _ -> exprParens a
  ExprOp _ _ -> exprParens a
  ExprLambda _ -> exprParens a
  ExprIf _ -> exprParens a
  ExprLet _ -> exprParens a
  ExprAdo _ -> exprParens a
  _ -> a

precExpr3 :: forall e. Expr e -> Expr e
precExpr3 a = case a of
  ExprTyped _ _ _ -> exprParens a
  ExprOp _ _ -> exprParens a
  ExprLambda _ -> exprParens a
  ExprIf _ -> exprParens a
  ExprLet _ -> exprParens a
  ExprAdo _ -> exprParens a
  ExprInfix _ _ -> exprParens a
  _ -> a

precExprApp :: forall e. Expr e -> Expr e
precExprApp a = case a of
  ExprTyped _ _ _ -> exprParens a
  ExprOp _ _ -> exprParens a
  ExprLambda _ -> exprParens a
  ExprIf _ -> exprParens a
  ExprLet _ -> exprParens a
  ExprAdo _ -> exprParens a
  ExprInfix _ _ -> exprParens a
  ExprApp _ _ -> exprParens a
  _ -> a

precExprAppLast :: forall e. Expr e -> Expr e
precExprAppLast a = case a of
  ExprTyped _ _ _ -> exprParens a
  ExprOp _ _ -> exprParens a
  ExprInfix _ _ -> exprParens a
  ExprApp _ _ -> exprParens a
  _ -> a

precExpr4 :: forall e. Expr e -> Expr e
precExpr4 a = case a of
  ExprTyped _ _ _ -> exprParens a
  ExprOp _ _ -> exprParens a
  ExprLambda _ -> exprParens a
  ExprIf _ -> exprParens a
  ExprLet _ -> exprParens a
  ExprAdo _ -> exprParens a
  ExprInfix _ _ -> exprParens a
  ExprCase _ -> exprParens a
  ExprDo _ -> exprParens a
  _ -> a

precExpr5 :: forall e. Expr e -> Expr e
precExpr5 a = case a of
  ExprTyped _ _ _ -> exprParens a
  ExprOp _ _ -> exprParens a
  ExprInfix _ _ -> exprParens a
  ExprNegate _ _ -> exprParens a
  ExprApp _ _ -> exprParens a
  _ -> a

precExpr6 :: forall e. Expr e -> Expr e
precExpr6 a = case a of
  ExprTyped _ _ _ -> exprParens a
  ExprOp _ _ -> exprParens a
  ExprInfix _ _ -> exprParens a
  ExprNegate _ _ -> exprParens a
  ExprApp _ _ -> exprParens a
  ExprLambda _ -> exprParens a
  ExprIf _ -> exprParens a
  ExprCase _ -> exprParens a
  ExprLet _ -> exprParens a
  ExprDo _ -> exprParens a
  ExprAdo _ -> exprParens a
  _ -> a

precExpr7 :: forall e. Expr e -> Expr e
precExpr7 a = case a of
  ExprTyped _ _ _ -> exprParens a
  ExprOp _ _ -> exprParens a
  ExprInfix _ _ -> exprParens a
  ExprNegate _ _ -> exprParens a
  ExprApp _ _ -> exprParens a
  ExprLambda _ -> exprParens a
  ExprIf _ -> exprParens a
  ExprCase _ -> exprParens a
  ExprLet _ -> exprParens a
  ExprDo _ -> exprParens a
  ExprAdo _ -> exprParens a
  ExprRecordAccessor _ -> exprParens a
  ExprRecordUpdate _ _ -> exprParens a
  _ -> a

binderParens :: forall e. Binder e -> Binder e
binderParens = BinderParens <<< toWrapped tokRightParen tokLeftParen

precBinder0 :: forall e. Binder e -> Binder e
precBinder0 a = case a of
  BinderTyped _ _ _ -> binderParens a
  _ -> a

precBinder1 :: forall e. Binder e -> Binder e
precBinder1 a = case a of
  BinderTyped _ _ _ -> binderParens a
  BinderOp _ _ -> binderParens a
  _ -> a

precBinder2 :: forall e. Binder e -> Binder e
precBinder2 a = case a of
  BinderTyped _ _ _ -> binderParens a
  BinderOp _ _ -> binderParens a
  BinderInt (Just _) _ _ -> binderParens a
  BinderNumber (Just _) _ _ -> binderParens a
  BinderConstructor _ _ -> binderParens a
  _ -> a

precInitLast :: forall a b. (a -> b) -> (a -> b) -> Array a -> Maybe (NonEmptyArray b)
precInitLast p1 p2 = Array.unsnoc >>> map \{ init, last } ->
  NonEmptyArray.snoc' (p1 <$> init) (p2 last)

precInitLast1 :: forall a b. (a -> b) -> (a -> b) -> NonEmptyArray a -> NonEmptyArray b
precInitLast1 p1 p2 arr = do
  let { init, last } = NonEmptyArray.unsnoc arr
  NonEmptyArray.snoc' (p1 <$> init) (p2 last)
