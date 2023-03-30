module Expr where

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

infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:
infixr 8 :^:
infixl 9 :.:
-- 



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

type R = Double
type Failable a = Either String a
type Env a = String -> Failable a

singleton_env :: String -> a -> Env a
singleton_env x v = \y -> if x == y then Right v else Left ("Unbound variable " ++ y)

class Doubleable a where
    fromDouble :: Double -> a

instance Doubleable Double where
    fromDouble = id

class (Eq a, Doubleable a, Floating a) => Evaluable a

instance Evaluable Double

eval :: Evaluable a => Expr -> Env a -> Failable a
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
eval (Var x) env = env x
eval (Const c) env = return $ fromDouble c
eval (Exp e) env = fmap exp $ eval e env
eval (Log e) env = fmap log $ eval e env
eval (e1 :^: e2) env = do
    v1 <- eval e1 env
    v2 <- eval e2 env
    return (v1 ** v2)


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
            let env = singleton_env x x0
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