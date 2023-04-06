module Env where

type Failable a = Either String a

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
