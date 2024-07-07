module Evaluator (evaluate) where

import Shared

evaluate :: Expression -> Value
evaluate expression = case expression of
  (Grouping e) -> evaluate e
  -- Number
  (Negate e) -> case evaluate e of
    IntVal a -> IntVal (-a)
    _ -> error "Can only negate a number"
  (Number e) -> IntVal e
  (Multiply x y) -> case (evaluate x, evaluate y) of
    (IntVal rx, IntVal ry) -> IntVal (rx * ry)
    _ -> error "Bad"
  (Divide x y) -> case (evaluate x, evaluate y) of
    (IntVal rx, IntVal 0) -> error "Can't divide by 0"
    (IntVal rx, IntVal ry) -> IntVal (rx `div` ry)
    _ -> error "Bad"
  (Add x y) -> case (evaluate x, evaluate y) of
    (IntVal rx, IntVal ry) -> IntVal (rx + ry)
    _ -> error "Bad"
  (Exponent x y) -> case (evaluate x, evaluate y) of
    (IntVal rx, IntVal ry) -> IntVal (rx ^ ry)
    _ -> error "Bad"
  (Subtract x y) -> case (evaluate x, evaluate y) of
    (IntVal rx, IntVal ry) -> IntVal (rx - ry)
    _ -> error "Bad"
  -- Bool
  (Not x) -> case evaluate x of
    BoolVal y -> BoolVal (not y)
  (Greater x y) -> case (evaluate x, evaluate y) of
    (IntVal rx, IntVal ry) -> BoolVal (rx > ry)
    _ -> error "Bad"
  (GreaterEqual x y) -> case (evaluate x, evaluate y) of
    (IntVal rx, IntVal ry) -> BoolVal (rx >= ry)
  (Less x y) -> case (evaluate x, evaluate y) of
    (IntVal rx, IntVal ry) -> BoolVal (rx < ry)
    _ -> error "Bad"
  (LessEqual x y) -> case (evaluate x, evaluate y) of
    (IntVal rx, IntVal ry) -> BoolVal (rx <= ry)
    _ -> error "Bad"

-- (Not e) -> Not $ evaluate e

-- data NegateType = Num Int | Boolean Bool
--
-- myFunction :: NegateType -> NegateType
-- myFunction (Num x) = Num (x * (-1))
-- myFunction (Boolean s) = Boolean (not s)

data Value = IntVal Int | BoolVal Bool | StringVal String
  deriving (Show, Eq, Ord)
