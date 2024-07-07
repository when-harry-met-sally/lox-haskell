module Evaluator (evaluate) where

import Shared

evaluateExpression :: Expression -> Value
evaluateExpression expression = case expression of
  (Grouping e) -> evaluateExpression e
  -- Number
  (Negate e) -> case evaluateExpression e of
    IntVal a -> IntVal (-a)
    BoolVal a -> BoolVal (not a)
    _ -> error "Can only negate a number"
  (Number e) -> IntVal e
  (Boolean e) -> BoolVal e
  (Multiply x y) -> case (evaluateExpression x, evaluateExpression y) of
    (IntVal rx, IntVal ry) -> IntVal (rx * ry)
    _ -> error "Bad"
  (Divide x y) -> case (evaluateExpression x, evaluateExpression y) of
    (IntVal rx, IntVal 0) -> error "Can't divide by 0"
    (IntVal rx, IntVal ry) -> IntVal (rx `div` ry)
    _ -> error "Bad"
  (Add x y) -> case (evaluateExpression x, evaluateExpression y) of
    (IntVal rx, IntVal ry) -> IntVal (rx + ry)
    _ -> error "Bad"
  (Exponent x y) -> case (evaluateExpression x, evaluateExpression y) of
    (IntVal rx, IntVal ry) -> IntVal (rx ^ ry)
    _ -> error "Bad"
  (Subtract x y) -> case (evaluateExpression x, evaluateExpression y) of
    (IntVal rx, IntVal ry) -> IntVal (rx - ry)
    _ -> error "Bad"
  -- Comparison
  (Not x) -> case evaluateExpression x of
    BoolVal y -> BoolVal (not y)
  (Greater x y) -> case (evaluateExpression x, evaluateExpression y) of
    (IntVal rx, IntVal ry) -> BoolVal (rx > ry)
    _ -> error "Bad"
  (GreaterEqual x y) -> case (evaluateExpression x, evaluateExpression y) of
    (IntVal rx, IntVal ry) -> BoolVal (rx >= ry)
  (Less x y) -> case (evaluateExpression x, evaluateExpression y) of
    (IntVal rx, IntVal ry) -> BoolVal (rx < ry)
    _ -> error "Bad"
  (LessEqual x y) -> case (evaluateExpression x, evaluateExpression y) of
    (IntVal rx, IntVal ry) -> BoolVal (rx <= ry)
    _ -> error "Bad"
  (Equal x y) -> case (evaluateExpression x, evaluateExpression y) of
    (IntVal rx, IntVal ry) -> BoolVal (rx == ry)
    (BoolVal rx, BoolVal ry) -> BoolVal (rx == ry)
    _ -> error "Bad"
  (NotEqual x y) -> case (evaluateExpression x, evaluateExpression y) of
    (IntVal rx, IntVal ry) -> BoolVal (rx /= ry)
    (BoolVal rx, BoolVal ry) -> BoolVal (rx /= ry)

evaluateStatement :: Statement -> IO ()
evaluateStatement expr =
  case expr of
    (ExpressionStatement expr) -> do
      let _ = evaluateExpression expr
      return ()
    (PrintStatement expr) -> print ("LOG", evaluateExpression expr)

evaluate :: [Statement] -> IO ()
evaluate = mapM_ evaluateStatement
