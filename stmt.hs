module Stmt where

import Expr hiding (newton_method)
import Env hiding (lookup)
import Prelude hiding (fail)
import Control.Monad.Fail
import Dual
import Debug.Trace

data Argument = ArgExpr Expr
              | ArgVar String
              | ArgConst Double

data Stmt = VarDecl String Expr
        | FuncDecl String ([Expr] -> Expr) Int
        | Expression Expr
        | Sequence Stmt Stmt
        | Command String [Argument]

eval_arg_double :: Evaluable a => Argument -> FuncEnv a -> Failable a
eval_arg_double (ArgExpr expr) env = eval expr env
eval_arg_double (ArgVar name) env = case lookup name env of
    Just (Left v) -> return v
    _ -> fail "Variable not found"
eval_arg_double (ArgConst v) _ = return $ fromDouble v

instance Show Stmt where
    show (VarDecl name expr) = "let " ++ name ++ " = " ++ show expr
    show (FuncDecl name _ arity) = "func " ++ name ++ "[" ++ replicate (arity - 1) ',' ++ "]"
    show (Expression expr) = show expr
    show (Sequence stmt1 stmt2) = show stmt1 ++ "; " ++ show stmt2

eval_stmt :: Evaluable a =>  Stmt -> FuncEnv a -> Failable ([a], FuncEnv a)
eval_stmt (VarDecl name expr) env = fmap (\v -> ([v], extend env (name, Left v))) $ eval expr env
eval_stmt (FuncDecl name func arity) env = return ([], extend env (name, Right (func, arity)))
eval_stmt (Expression expr) env = fmap (\v -> ([v], env)) $ eval expr env
eval_stmt (Sequence stmt1 stmt2) env = do
    (v1, env1) <- eval_stmt stmt1 env
    (v2, env2) <- eval_stmt stmt2 env1
    return (v1 ++ v2, env2)
eval_stmt (Command name args) env = do
    v <- eval_command name args env
    return (v, env)


eval_prog :: Evaluable a => Stmt -> Failable [a]
eval_prog stmt = fmap fst $ eval_stmt stmt empty

eval_command :: Evaluable a => String -> [Argument] -> FuncEnv a -> Failable [a]
eval_command "grad" args env = fmap (\v -> [v]) $ eval_grad args env
eval_command "root" args env = fmap (\v -> [v]) $ eval_root args env
eval_command "minimize" args env = eval_grad_desc args env
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


eval_grad_desc :: Evaluable a => [Argument] -> FuncEnv a -> Failable [a]
eval_grad_desc (ArgVar func : ArgConst alpha : x0') env = do
    x0 <- traverse (flip eval_arg_double env) x0'
    f <- case Prelude.lookup func env of
        Just (Right (func, arity)) -> if arity == length x0 then return func else fail "Wrong number of arguments for function"
        _ -> fail "Function not found"
    gradient_descent f x0 env (fromDouble alpha) 100
eval_grad_desc (ArgConst alpha : ArgVar func : x0') env = eval_grad_desc (ArgVar func : ArgConst alpha : x0') env
eval_grad_desc _ _ = fail "Wrong number of arguments for minimization"

gradient_descent :: Evaluable a => ([Expr] -> Expr) -> [a] -> FuncEnv a -> a -> Int -> Failable [a]
gradient_descent f x0 env alpha n = 
    if n == 0 then return x0
    else do
        let m = length x0 + 1
        let new_vars = [Var (show i) | i <- [1.. m]]
        let env_x0 = [(x, Left v) | (x, v) <- zip [show i | i <- [1..]] x0]
        let env' = dualize_env $ env_x0 ++ env
        let envs = [dualize_var (show i) env' | i <- [1 .. m]]
        --let envs = trace (show $ map show_env envs') envs'
        grad <- traverse (eval (f new_vars)) envs
        let grad' = map deriv grad
        let x1 = zipWith (-) x0 $ map (alpha *) grad'
        let max_change = maximum $ zipWith (\x y -> toDouble $ abs (x - y)) x0 x1
        if max_change < 0.0001 then return x1 else gradient_descent f x1 env alpha (n - 1)


show_env :: Evaluable a => FuncEnv a -> String
show_env env = concat $ map show_pair env
    where show_pair (name, Left value) = name ++ " = " ++ show value
          show_pair (name, Right (func, arity)) = name ++ "[" ++ show arity ++ "]"
