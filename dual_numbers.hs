module Dual where
import Expr (Doubleable(..), fromDouble, Evaluable(..), Expr, Failable, singleton_env, eval)
data Dual a =  Const Double | Dual {value :: a, deriv :: a}
  deriving (Eq)

instance Doubleable a => Doubleable (Dual a) where
    fromDouble x = Const x

instance Evaluable a => Evaluable (Dual a)

instance Show a => Show (Dual a) where
    show (Const x) = show x
    show (Dual x y) =  "(" ++ show x ++ " + " ++ show y ++ "ε" ++ ")"

instance (Doubleable a, Num a) => Num (Dual a) where
    (Const x) + (Const y) = Const (x + y)
    (Const x) + (Dual y z) = Dual (fromDouble x + y) z
    (Dual x y) + (Const z) = Dual (x + fromDouble z) y
    (Dual x y) + (Dual z w) = Dual (x + z) (y + w)

    (Const x) * (Const y) = Const (x * y)
    (Const x) * (Dual y z) = Dual (fromDouble x * y) (fromDouble x * z)
    (Dual x y) * (Const z) = Dual (x * fromDouble z) (y * fromDouble z)
    (Dual x y) * (Dual z w) = Dual (x * z) (x * w + y * z)


    negate (Const x) = Const (negate x)
    negate (Dual x y) = Dual (negate x) (negate y)

    abs (Const x) = Const (abs x)
    abs (Dual x y) = Dual (abs x) (abs y)

    signum (Const x) = Const (signum x)
    signum (Dual x y) = Dual (signum x) (signum y)

    fromInteger x = Const (fromInteger x)

instance (Doubleable a, Fractional a) => Fractional (Dual a) where
    (Const x) / (Const y) = Const (x / y)
    (Const x) / (Dual y z) = Dual (fromDouble x / y) (negate (fromDouble x / (y * y)) * z)
    (Dual x y) / (Const z) = Dual (x / fromDouble z) (y / fromDouble z)
    (Dual x y) / (Dual z w) = Dual (x / z) ((y * z - x * w) / (z * z))

    fromRational x = Const (fromRational x)

instance (Doubleable a, Floating a) => Floating (Dual a) where
    pi = Const pi
    exp (Const x) = Const (exp x)
    exp (Dual x y) = Dual (exp x) (y * exp (x))
    log (Const x) = Const (log x)
    log (Dual x y) = Dual (log x) (y / x)
    sin (Const x) = Const (sin x)
    sin (Dual x y) = Dual (sin x) (y * (cos x))
    cos (Const x) = Const (cos x)
    cos (Dual x y) = Dual (cos x) (negate (y * (sin x)))
    sinh (Const x) = Const (sinh x)
    sinh (Dual x y) = Dual (sinh x) (y * (cosh x))
    cosh (Const x) = Const (cosh x)
    cosh (Dual x y) = Dual (cosh x) (y * (sinh x))
    asinh (Const x) = Const (asinh x)
    asinh (Dual x y) = Dual (asinh x) (y / (sqrt (1 + x * x)))
    acosh (Const x) = Const (acosh x)
    acosh (Dual x y) = Dual (acosh x) (y / (sqrt (x * x - 1)))
    atanh (Const x) = Const (atanh x)
    atanh (Dual x y) = Dual (atanh x) (y / ((sqrt (1 - x * x))))

diff :: Expr -> Double -> String -> Failable Double
diff e x0 x = fmap deriv $ eval e (singleton_env x (Dual x0 1.0))

hessian :: Expr -> Double -> String -> Failable Double
hessian e x0 x = fmap (deriv . deriv) $ eval e (singleton_env x (Dual (Dual x0 1.0) (Dual 1.0 0.0)))
