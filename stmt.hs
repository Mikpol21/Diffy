module Stmt where

import Expr hiding (newton_method)
import Env hiding (lookup)
import Prelude hiding (fail)
import Control.Monad.Fail
import Dual

data Argument = ArgExpr Expr
              | ArgVar String
              | ArgConst Double

data Stmt = VarDecl String Expr
        | FuncDecl String ([Expr] -> Expr) Int
        | Expression Expr
        | Sequence Stmt Stmt
        | Command String [Argument]

instance Show Stmt where
    show (VarDecl name expr) = "let " ++ name ++ " = " ++ show expr
    show (FuncDecl name _ arity) = "func " ++ name ++ "[" ++ replicate (arity - 1) ',' ++ "]"
    show (Expression expr) = show expr
    show (Sequence stmt1 stmt2) = show stmt1 ++ "; " ++ show stmt2

eval_stmt :: Evaluable a =>  Stmt -> FuncEnv a -> Failable (a, FuncEnv a)
eval_stmt (VarDecl name expr) env = fmap (\v -> (v, extend env (name, Left v))) $ eval expr env
eval_stmt (FuncDecl name func arity) env = return (fromDouble 0, extend env (name, Right (func, arity)))
eval_stmt (Expression expr) env = fmap (\v -> (v, env)) $ eval expr env
eval_stmt (Sequence stmt1 stmt2) env = do
    (v1, env1) <- eval_stmt stmt1 env
    (v2, env2) <- eval_stmt stmt2 env1
    return (v2, env2)
eval_stmt (Command name args) env = do
    v <- eval_command name args env
    return (v, env)


eval_prog :: Evaluable a => Stmt -> Failable a
eval_prog stmt = fmap fst $ eval_stmt stmt empty

eval_command :: Evaluable a => String -> [Argument] -> FuncEnv a -> Failable a
eval_command "grad" args env = eval_grad args env
eval_command "root" args env = eval_root args env
eval_command _ _ _ = fail "Unknown command"

eval_grad :: Evaluable a => [Argument] -> FuncEnv a -> Failable a
eval_grad [ArgExpr expr, ArgVar x] env = fmap deriv $ eval expr (dualize_var x $ dualize_env env)
eval_grad [ArgVar x, ArgExpr expr] env = eval_grad [ArgExpr expr, ArgVar x] env
eval_grad _ _ = fail "Wrong number of arguments for gradient"

eval_root :: Evaluable a => [Argument] -> FuncEnv a -> Failable a
eval_root [ArgVar f, ArgConst x0] env = case lookup f env of
    Just (Right (func, arity)) -> if arity == 1 then fmap fromDouble $ newton_method func (dualize_env env) x0 100 else fail "Wrong number of arguments for function"
    _ -> fail "Function not found" 
eval_root [ArgConst x0, ArgVar f] env = eval_root [ArgVar f, ArgConst x0] env
eval_root [ArgExpr expr, ArgVar f] env = eval_root [ArgVar f, ArgExpr expr] env
eval_root [ArgVar f, ArgExpr expr] env = do
    x0 <- eval expr env
    eval_root [ArgVar f, ArgConst (toDouble x0)] env
eval_root _ _ = fail "Wrong number of arguments for minimization"

newton_method :: Evaluable a => ([Expr] -> Expr) -> FuncEnv (Dual a) -> Double -> Int -> Failable Double
newton_method f env x n = if n == 0 then return x else do
    dual <- eval (f [Var "0"]) (env `extend` ("0", Left $ Dual (fromDouble x) (fromDouble 1.0)))
    case dual of
        Dual.Const fx -> fail "The function is constant"
        Dual fx dx -> newton_method f env (x - toDouble fx / toDouble dx) (n - 1)
    
dualize_env :: (Evaluable a) => FuncEnv a -> FuncEnv (Dual a)
dualize_env env = map to_dual env
    where   to_dual (x', Left v) = (x' ,Left $ Dual.Const (toDouble v))
            to_dual (x', Right v) = (x', Right v)

dualize_var :: (Evaluable a) => String -> FuncEnv (Dual a) -> FuncEnv (Dual a)
dualize_var x env = update x to_dual env
    where   to_dual (Left (Dual.Const v)) = Left $ Dual (fromDouble v) (fromDouble 1.0)
            to_dual (Right v) = Right v
