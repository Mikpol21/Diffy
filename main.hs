module Main where

import Env
import Expr
import Dual
import Stmt
import ExprParser
import Parser
import Data.Either
import System.IO
import Control.Exception
import System.Environment (getArgs)


liftEither :: Either String a -> IO a
liftEither (Left s) = Prelude.fail s
liftEither (Right v) = return v

showResult :: Show a => a -> IO ()
showResult v = do
    putStrLn $ "-- " ++ show v
    hFlush stdout

run_prog :: String  -> FuncEnv Double -> IO (FuncEnv Double)
run_prog code env = do
    (stmt, rest) <- liftEither $ runParser parse_stmt code
    if rest == "" then return empty else Prelude.fail "Parse error"
    (v, env') <- liftEither $ eval_stmt stmt env
    showResult v
    hFlush stdout
    return (env' ++ env)

dialog :: FuncEnv Double -> IO ()
dialog env = handle (\e -> putStrLn (show (e :: IOError)) >> dialog env) $ do
    putStr "Diffy> "
    hFlush stdout
    line <- getLine
    hFlush stdout
    env' <- run_prog line env
    dialog env'

main :: IO ()
main = do
    args <- getArgs
    env <- foldl (>>=) (return empty) (map load_file args)
    dialog env

load_file :: String -> FuncEnv Double -> IO (FuncEnv Double)
load_file path env = do
    putStrLn $ "Loading file " ++ path
    hFlush stdout
    code <- readFile path
    env' <- run_prog code env
    return env'
