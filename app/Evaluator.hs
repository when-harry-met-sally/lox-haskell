module Evaluator (evaluate) where

import Control.Monad (foldM_)
import qualified Data.Map.Strict as Map
import Shared

type Env = Map.Map String Value

evaluateExpression :: Expression -> Env -> Value
evaluateExpression expression env = case expression of
  (Grouping e) -> evaluateExpression e env
  -- Number
  (Negate e) -> case evaluateExpression e env of
    IntVal a -> IntVal (-a)
    BoolVal a -> BoolVal (not a)
    _ -> error "Can only negate a number"
  (Number e) -> IntVal e
  (Boolean e) -> BoolVal e
  (Str e) -> StringVal e
  (Identifier name) -> case Map.lookup name env of
    Just v -> v
    _ -> error "Can't find"
  (Multiply x y) -> case (evaluateExpression x env, evaluateExpression y env) of
    (IntVal rx, IntVal ry) -> IntVal (rx * ry)
    _ -> error "Bad"
  (Divide x y) -> case (evaluateExpression x env, evaluateExpression y env) of
    (IntVal rx, IntVal 0) -> error "Can't divide by 0"
    (IntVal rx, IntVal ry) -> IntVal (rx `div` ry)
    _ -> error "Bad"
  (Add x y) -> case (evaluateExpression x env, evaluateExpression y env) of
    (IntVal rx, IntVal ry) -> IntVal (rx + ry)
    _ -> error "Bad"
  (Exponent x y) -> case (evaluateExpression x env, evaluateExpression y env) of
    (IntVal rx, IntVal ry) -> IntVal (rx ^ ry)
    _ -> error "Bad"
  (Subtract x y) -> case (evaluateExpression x env, evaluateExpression y env) of
    (IntVal rx, IntVal ry) -> IntVal (rx - ry)
    _ -> error "Bad"
  -- Comparison
  (Not x) -> case evaluateExpression x env of
    BoolVal y -> BoolVal (not y)
  (Greater x y) -> case (evaluateExpression x env, evaluateExpression y env) of
    (IntVal rx, IntVal ry) -> BoolVal (rx > ry)
    _ -> error "Bad"
  (GreaterEqual x y) -> case (evaluateExpression x env, evaluateExpression y env) of
    (IntVal rx, IntVal ry) -> BoolVal (rx >= ry)
  (Less x y) -> case (evaluateExpression x env, evaluateExpression y env) of
    (IntVal rx, IntVal ry) -> BoolVal (rx < ry)
    _ -> error "Bad"
  (LessEqual x y) -> case (evaluateExpression x env, evaluateExpression y env) of
    (IntVal rx, IntVal ry) -> BoolVal (rx <= ry)
    _ -> error "Bad"
  (Equal x y) -> case (evaluateExpression x env, evaluateExpression y env) of
    (IntVal rx, IntVal ry) -> BoolVal (rx == ry)
    (BoolVal rx, BoolVal ry) -> BoolVal (rx == ry)
    _ -> error "Bad"
  (NotEqual x y) -> case (evaluateExpression x env, evaluateExpression y env) of
    (IntVal rx, IntVal ry) -> BoolVal (rx /= ry)
    (BoolVal rx, BoolVal ry) -> BoolVal (rx /= ry)
  _ -> error "bad"

evaluateDeclaration :: Declaration -> Env -> (IO (), Env)
evaluateDeclaration expr env =
  case expr of
    (StatementDeclaration stmt) -> case stmt of
      (ExpressionStatement expr) -> do
        let _ = evaluateExpression expr
        (return (), env)
      (PrintStatement expr) -> (print ("LOG", evaluateExpression expr env), env)
    (VarDeclaration name expr) -> (return (), Map.insert name (evaluateExpression expr env) env)

eval :: [Declaration] -> Env -> IO ()
eval declarations env = foldM_ processDeclaration env declarations
  where
    processDeclaration :: Env -> Declaration -> IO Env
    processDeclaration currentEnv decl = do
      let (ioAction, newEnv) = evaluateDeclaration decl currentEnv
      ioAction -- Execute the IO action
      return newEnv -- Pass the updated environment to the next iteration

evaluate :: [Declaration] -> IO ()
evaluate declarations = eval declarations Map.empty
