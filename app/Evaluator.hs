module Evaluator (evaluate) where

import Shared

evaluate :: Expression -> Int
evaluate expression = case expression of
  (Grouping e) -> evaluate e
  (Negate e) -> -(evaluate e)
  (Number e) -> e
  (Multiply x y) -> evaluate x * evaluate y
  (Divide x y) -> evaluate x `div` evaluate y
  (Add x y) -> evaluate x + evaluate y
  (Exponent x y) -> evaluate x ^ evaluate y
  (Subtract x y) -> evaluate x - evaluate y

-- data NegateType = Num Int | Boolean Bool
--
-- myFunction :: NegateType -> NegateType
-- myFunction (Num x) = Num (x * (-1))
-- myFunction (Boolean s) = Boolean (not s)
