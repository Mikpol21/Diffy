module Dual where
import Expr (Doubleable(..), fromDouble, Evaluable(..))
data Dual a =  Const Double | Dual Double a
  deriving (Eq)

instance Doubleable a => Doubleable (Dual a) where
    fromDouble x = Const x

instance Evaluable a => Evaluable (Dual a)

instance Show a => Show (Dual a) where
    show (Const x) = show x
    show (Dual x y) = show x ++ " + " ++ show y ++ "ε"

instance (Doubleable a, Num a) => Num (Dual a) where
    (Const x) + (Const y) = Const (x + y)
    (Const x) + (Dual y z) = Dual (x + y) z
    (Dual x y) + (Const z) = Dual (x + z) y
    (Dual x y) + (Dual z w) = Dual (x + z) (y + w)

    (Const x) * (Const y) = Const (x * y)
    (Const x) * (Dual y z) = Dual (x * y) (fromDouble x * z)
    (Dual x y) * (Const z) = Dual (x * z) (y * fromDouble z)
    (Dual x y) * (Dual z w) = Dual (x * z) (fromDouble x * w + y * fromDouble z)

    negate (Const x) = Const (negate x)
    negate (Dual x y) = Dual (negate x) (negate y)

    abs (Const x) = Const (abs x)
    abs (Dual x y) = Dual (abs x) (abs y)

    signum (Const x) = Const (signum x)
    signum (Dual x y) = Dual (signum x) (signum y)

    fromInteger x = Const (fromInteger x)

instance (Doubleable a, Fractional a) => Fractional (Dual a) where
    (Const x) / (Const y) = Const (x / y)
    (Const x) / (Dual y z) = Dual (x / y) (negate (fromDouble x / (fromDouble y * fromDouble y)) * z)
    (Dual x y) / (Const z) = Dual (x / z) (y / fromDouble z)
    (Dual x y) / (Dual z w) = Dual (x / z) ((y * fromDouble z - fromDouble x * w) / (fromDouble z * fromDouble z))

    fromRational x = Const (fromRational x)

instance (Doubleable a, Floating a) => Floating (Dual a) where
    pi = Const pi
    exp (Const x) = Const (exp x)
    exp (Dual x y) = Dual (exp x) (y * exp (fromDouble x))
    log (Const x) = Const (log x)
    log (Dual x y) = Dual (log x) (y / fromDouble x)
    sin (Const x) = Const (sin x)
    sin (Dual x y) = Dual (sin x) (y * fromDouble (cos x))
    cos (Const x) = Const (cos x)
    cos (Dual x y) = Dual (cos x) (negate (y * fromDouble (sin x)))
    sinh (Const x) = Const (sinh x)
    sinh (Dual x y) = Dual (sinh x) (y * fromDouble (cosh x))
    cosh (Const x) = Const (cosh x)
    cosh (Dual x y) = Dual (cosh x) (y * fromDouble (sinh x))
    asinh (Const x) = Const (asinh x)
    asinh (Dual x y) = Dual (asinh x) (y / fromDouble (sqrt (1 + x * x)))
    acosh (Const x) = Const (acosh x)
    acosh (Dual x y) = Dual (acosh x) (y / fromDouble (sqrt (x * x - 1)))
    atanh (Const x) = Const (atanh x)
    atanh (Dual x y) = Dual (atanh x) (y / (fromDouble (sqrt (1 - x * x))))

