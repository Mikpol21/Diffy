{-# LANGUAGE FlexibleInstances #-}
module Env where
import qualified Control.Monad.Fail as Fail

type Failable a = Either String a

instance Fail.MonadFail (Either String) where
  fail = Left



type Env a = [(String, a)]

empty :: Env a
empty = []

singleton :: String -> a -> Env a
singleton x v = [(x, v)]

extend :: Env a -> (String, a) -> Env a
extend env (x, v) = (x, v) : env

lookup :: String -> Env a -> Failable a
lookup x env = case Prelude.lookup x env of
    Nothing -> Left ("Unbound variable " ++ x)
    Just v -> Right v

transform :: (a -> b) -> Env a -> Env b
transform f env = [(x, f v) | (x, v) <- env]

update :: String -> (a -> a) -> Env a -> Env a
update x f env = case Prelude.lookup x env of
    Nothing -> env
    Just v -> (x, f v) : Prelude.filter (\(x', _) -> x /= x') env

