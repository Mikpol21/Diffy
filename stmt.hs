module Stmt where

import Expr
import Env

data Stmt = VarDecl String Expr
        | FuncDecl String ([Expr] -> Expr) Int
        | Expression Expr
        | Sequence Stmt Stmt

eval_stmt :: Evaluable a =>  Stmt -> FuncEnv a -> Failable (a, FuncEnv a)
eval_stmt (VarDecl name expr) env = fmap (\v -> (v, extend env (name, Left v))) $ eval expr env
eval_stmt (FuncDecl name func arity) env = return (fromDouble 0, extend env (name, Right (func, arity)))
eval_stmt (Expression expr) env = fmap (\v -> (v, env)) $ eval expr env
eval_stmt (Sequence stmt1 stmt2) env = do
    (v1, env1) <- eval_stmt stmt1 env
    (v2, env2) <- eval_stmt stmt2 env1
    return (v2, env2)