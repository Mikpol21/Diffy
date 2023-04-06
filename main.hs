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


liftEither :: Either String a -> IO a
liftEither (Left s) = Prelude.fail s
liftEither (Right v) = return v


dialog :: FuncEnv Double -> IO ()
dialog env = handle (\e -> putStrLn (show (e :: IOError)) >> dialog env) $ do
    putStr "Diffy> "
    hFlush stdout
    line <- getLine
    hFlush stdout
    (stmt, rest) <- liftEither $ runParser parse_stmt line
    if rest == "" then return () else Prelude.fail "Parse error"
    hFlush stdout
    (v, env') <- liftEither $ eval_stmt stmt env
    putStrLn $ show v
    hFlush stdout
    dialog (env' ++ env)
    where fail_with_dialog s = do
            putStrLn s
            hFlush stdout
            dialog env

main :: IO ()
main = dialog empty