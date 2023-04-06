module Expr where

import Env
import Prelude hiding (lookup)

data Expr = Expr :+: Expr
        | Expr :-:  Expr
        | Expr :*: Expr
        | Expr :/: Expr
        | Double :.: Expr
        | Var String
        | Const Double
        | Exp Expr
        | Log Expr
        | Expr :^: Expr
        | Apply String [Expr]
        | Let String Expr Expr

infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:
infixr 8 :^:
infixl 9 :.:


instance Show Expr where
    show (e1 :+: e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (e1 :-: e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (e1 :*: e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    show (e1 :/: e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
    show (c :.: e) = show c ++ " * (" ++ show e ++ ")"
    show (Var x) = x
    show (Const c) = show c
    show (Exp e) = "exp(" ++ show e ++ ")"
    show (Log e) = "log(" ++ show e ++ ")"
    show (e1 :^: e2) = show e1 ++ "^" ++ show e2
    show (Let x e1 e2) = x ++ " = " ++ show e1 ++ "; " ++ show e2
    show (Apply f args) = f ++ "[" ++ showArgs args ++ "]"
        where showArgs [] = ""
              showArgs [x] = show x
              showArgs (x:xs) = show x ++ ", " ++ showArgs xs

type R = Double

class Doubleable a where
    fromDouble :: Double -> a
    toDouble :: a -> Double

instance Doubleable Double where
    fromDouble = id
    toDouble = id

class (Eq a, Doubleable a, Floating a) => Evaluable a

instance Evaluable Double

type FuncEnv a = Env (Either a ([Expr] -> Expr, Int))

eval :: Evaluable a => Expr -> FuncEnv a -> Failable a
eval (e1 :+: e2) env = do
    v1 <- eval e1 env
    v2 <- eval e2 env
    return (v1 + v2)
eval (e1 :-: e2) env = do
    v1 <- eval e1 env
    v2 <- eval e2 env
    return (v1 - v2)
eval (e1 :*: e2) env = do
    v1 <- eval e1 env
    v2 <- eval e2 env
    return (v1 * v2)
eval (e1 :/: e2) env = do
    v1 <- eval e1 env
    v2 <- eval e2 env
    if v2 == fromDouble 0 then Left "Division by zero"
    else return (v1 / v2)
eval (c :.: e) env = fmap (fromDouble c*) $ eval e env
eval (Var x) env = do
    v <- lookup x env
    case v of
        Left c -> return c
        Right (f, _) -> fail $ "Variable " ++ x ++ " is not a constant"
eval (Const c) env = return $ fromDouble c
eval (Exp e) env = fmap exp $ eval e env
eval (Log e) env = fmap log $ eval e env
eval (e1 :^: e2) env = do
    v1 <- eval e1 env
    v2 <- eval e2 env
    return (v1 ** v2)
eval (Apply f args) env = do
    x <- lookup f env
    case x of 
        Left _ -> fail $ f ++ " is a constant"
        Right (func, arity) -> if length args == arity 
            then eval (func args) env 
            else fail $ "Wrong number of arguments for " ++ f
eval (Let x e1 e2) env = do
    v <- eval e1 env
    eval e2 (extend env (x, (Left v)))
        

substitute :: Expr -> String -> Expr -> Expr
substitute (e1 :+: e2) x e = substitute e1 x e :+: substitute e2 x e
substitute (e1 :-: e2) x e = substitute e1 x e :-: substitute e2 x e
substitute (e1 :*: e2) x e = substitute e1 x e :*: substitute e2 x e
substitute (e1 :/: e2) x e = substitute e1 x e :/: substitute e2 x e
substitute (c :.: e') x e = c :.: substitute e' x e
substitute (Var x') x e = if x == x' then e else Var x'
substitute (Const c) _ _ = Const c
substitute (Exp e) x e' = Exp (substitute e x e')
substitute (Log e) x e' = Log (substitute e x e')
substitute (e1 :^: e2) x e' = substitute e1 x e' :^: substitute e2 x e'
substitute (Apply f args) x e = Apply f (map (\e -> substitute e x e) args)
substitute (Let x' e1 e2) x e = Let x' (substitute e1 x e) (substitute e2 x e)



sum_expr :: [Expr] -> Expr
sum_expr [] = Const 0
sum_expr [e] = e
sum_expr (e:es) = e :+: sum_expr es

diff :: Expr -> String -> Expr
diff (e1 :+: e2) x = diff e1 x :+: diff e2 x
diff (e1 :-: e2) x = diff e1 x :-: diff e2 x
diff (e1 :*: e2) x = diff e1 x :*: e2 :+: e1 :*: diff e2 x
diff (e1 :/: e2) x = (diff e1 x :*: e2 :-: e1 :*: diff e2 x) :/: (e2 :*: e2)
diff (c :.: e) x = c :.: diff e x
diff (Var x') x = if x == x' then Const 1 else Const 0
diff (Const _) _ = Const 0
diff (Exp e) x = Exp e :*: diff e x
diff (Log e) x = diff e x :/: e
diff (e1 :^: e2) x = (e1 :^: e2) :*: (diff e2 x :*: Log e1 :+: e2 :*: diff e1 x :/: e1)

newton_method :: Expr -> String -> R -> R -> Int -> Failable R
newton_method f x x0 eps max_iter = newton_method' f x x0 eps 0
    where
        newton_method' f x x0 eps n = do
            let env = singleton x (Left x0)
            vx <- eval f env
            dx <- eval (diff f x) env
            let x1 = x0 - vx / dx
            if abs (x1 - x0) < eps || n == max_iter then return x1
            else newton_method' f x x1 eps (n + 1)

newtonMethod :: Expr -> String -> R -> Failable R
newtonMethod f x x0 = newton_method f x x0 0.0001 100

isZero :: Expr -> Bool
isZero (Const 0) = True
isZero (Const _) = False
isZero (Var _) = False
isZero (e1 :+: e2) = isZero e1 && isZero e2
isZero (e1 :-: e2) = isZero e1 && isZero e2
isZero (e1 :*: e2) = isZero e1 || isZero e2
isZero (e1 :/: e2) = isZero e1
isZero (c :.: e) = c == 0 || isZero e
isZero _ = False

prune0 :: Expr -> Expr
prune0 (e1 :+: e2) = 
    if isZero e1 then prune0 e2
    else if isZero e2 then prune0 e1
    else prune0 e1 :+: prune0 e2
prune0 (e1 :-: e2) =
    if isZero e1 then (-1) :.: prune0 e2
    else if isZero e2 then prune0 e1
    else prune0 e1 :-: prune0 e2
prune0 (e1 :*: e2) =
    if isZero e1 || isZero e2 then Const 0
    else prune0 e1 :*: prune0 e2
prune0 (e1 :/: e2) =
    if isZero e1 then Const 0
    else prune0 e1 :/: prune0 e2
prune0 (c :.: e) = if c == 0 then Const 0 else c :.: prune0 e
prune0 e = e