module Evaluator (evaluate) where

import Control.Monad (foldM_)
import qualified Data.Map.Strict as Map
import Shared

type Env = [Map.Map String Value]

envLookup :: Env -> String -> Value
envLookup [] key = error ("Can't find: " ++ key)
envLookup (scope : stack) key = case Map.lookup key scope of
  Just v -> v
  _ -> envLookup stack key

isInScope :: Env -> String -> Bool
isInScope [] _ = False
isInScope (scope : stack) key = case Map.lookup key scope of
  Just v -> True
  _ -> isInScope stack key

envPut :: Env -> String -> Value -> Env
envPut [] _ _ = error "Environment is empty, cannot insert value"
envPut a@(scope : stack) key value = case isInScope a key of
  True -> error ("Variable " ++ key ++ " is already in scope")
  False -> Map.insert key value scope : stack

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
  (Identifier key) -> envLookup env key
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
    (StringVal rx, StringVal ry) -> BoolVal (rx == ry)
    (BoolVal rx, BoolVal ry) -> BoolVal (rx == ry)
    _ -> error "Bad"
  (NotEqual x y) -> case (evaluateExpression x env, evaluateExpression y env) of
    (IntVal rx, IntVal ry) -> BoolVal (rx /= ry)
    (StringVal rx, StringVal ry) -> BoolVal (rx /= ry)
    (BoolVal rx, BoolVal ry) -> BoolVal (rx /= ry)
  _ -> error "Bad"

evaluateDeclaration :: Declaration -> Env -> IO Env
evaluateDeclaration (StatementDeclaration stmt) env = case stmt of
  (ExpressionStatement expr) -> do
    let _ = evaluateExpression expr env
    return env
  (PrintStatement expr) -> do
    print ("LOX LOG", evaluateExpression expr env)
    return env
  (Block declarations) -> do evaluateBlock declarations (Map.empty : env)
  (IfStatement expr block) ->
    case evaluateExpression expr env of
      BoolVal True -> do evaluateBlock block env
      BoolVal False -> return env
      _ -> error "An if statement must resolve to a boolean value"
  (IfElseStatement expr block1 block2) ->
    case evaluateExpression expr env of
      BoolVal True -> do evaluateBlock block1 env
      BoolVal False -> do evaluateBlock block2 env
      _ -> error "An if statement must resolve to a boolean value"
evaluateDeclaration (VarDeclaration name expr) env = do
  let env' = envPut env name (evaluateExpression expr env)
  return env'

evaluateBlock :: [Declaration] -> Env -> IO Env
evaluateBlock [] (x : xs) = return xs
evaluateBlock (d : ds) env = do
  env' <- evaluateDeclaration d env
  evaluateBlock ds env'

eval :: [Declaration] -> Env -> IO ()
eval declarations env = foldM_ processDeclaration env declarations
  where
    processDeclaration :: Env -> Declaration -> IO Env
    processDeclaration currentEnv decl = do
      let ioAction = evaluateDeclaration decl currentEnv
      ioAction -- Execute the IO action

evaluate :: [Declaration] -> IO ()
evaluate declarations = eval declarations [Map.empty]
