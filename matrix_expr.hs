{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module MatrixExpr where

import Expr
import Data.Vector hiding ((++))
import qualified Prelude
import Prelude hiding (length, zipWith, map, sum, fromList, (!), head)

type M a = Vector (Vector a)
type V a = Vector a

data MatrixExpr where
    (:+) :: MatrixExpr -> MatrixExpr -> MatrixExpr
    (:-) :: MatrixExpr -> MatrixExpr -> MatrixExpr
    (:*) :: MatrixExpr -> MatrixExpr -> MatrixExpr
    (:.) :: R -> MatrixExpr -> MatrixExpr
    VarM :: String -> Int -> Int -> MatrixExpr
    ConstM :: M R -> MatrixExpr
    T :: MatrixExpr -> MatrixExpr
    (:@) :: (Expr -> Expr) -> MatrixExpr -> MatrixExpr


instance Show MatrixExpr where
    show (e1 :+ e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (e1 :- e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (e1 :* e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    show (c :. e) = show c ++ " * (" ++ show e ++ ")"
    show (VarM x n m) = x ++ " :: (" ++ show n ++ "," ++ show m ++ ")"
    show (ConstM c) = show c
    show (T e) = show e ++ "^T"
    show (f :@ e) = "Map " ++ show e

shape :: M a -> (Int, Int)
shape m = (length m, length $ head m)


reduce :: MatrixExpr -> M Expr
reduce (e1 :+ e2) = zipWith (zipWith (:+:)) (reduce e1) (reduce e2)
reduce (e1 :- e2) = zipWith (zipWith (:-:)) (reduce e1) (reduce e2)
reduce (c :. e) = map (map (c :.:)) (reduce e)
reduce (VarM x n m) = fromList [fromList [Var (x ++ show i ++ show j) | j <- [0..m-1]] | i <- [0..n-1]]
reduce (ConstM c) = map (map Const) c
reduce (T e) = let e' = reduce e in fromList [fromList [e' ! j ! i | j <- [0..(length $ head e') - 1]] | i <- [0..(length e') - 1]]
reduce (f :@ e) = map (map f) (reduce e)
reduce (e1 :* e2) = let
    r1 = reduce e1
    r2 = reduce e2
    in  if snd (shape r1) /= fst (shape r2) then error "Matrix dimensions do not match"
        else fromList [fromList [sum_expr [r1 ! i ! k :*: r2 ! k ! j | k <- [0..(length r1 - 1)]] | j <- [0..(length r2) - 1]] | i <- [0..(length r1 - 1)]]