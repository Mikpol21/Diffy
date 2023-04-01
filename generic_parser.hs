{-LANGUAGE FlexibleInstances-}
module Parser where
import Control.Arrow hiding (first, second)
import Control.Applicative -- hiding (many)
import Data.Bifunctor
import Prelude hiding (fail)



newtype Parser a = Parser {runParser :: String -> Either String (a, String)}

instance Functor Parser where
    fmap f (Parser parser) = Parser $ \str ->
        let res = parser str in  fmap (first f) res

instance Applicative Parser where
    pure x = Parser $ \str -> pure (x, str)
    Parser parserF <*> Parser parserX = Parser $ \str1 -> do
        (f, str2) <- parserF str1
        (x, str3) <- parserX str2
        pure (f x, str3)

instance Monad Parser where
    return = pure
    (Parser parser) >>= g = Parser $ \str1 -> do
        (x, str2) <- parser str1
        runParser (g x) str2
-- xm :: Pipe a, g :: a -> Parser b

instance (Monoid a)=> Alternative (Either a) where
    empty = Left mempty
    Left _ <|> g = g
    f <|> _ = f

instance Alternative Parser where
    empty = fail "empty alternative"
    f <|> g = Parser $ \str ->
        runParser f str <|> runParser g str


fail msg = Parser $ \str -> Left msg

consume :: Parser Char
consume = Parser $ \str -> case str of
    [] -> Left "no remaining characters"
    (c:cs) -> pure (c, cs)

(<?>) :: (a -> Bool) -> Parser a -> Parser a
c <?> p = do
        x <- p
        if c x then return x else fail "predicate not satisfied"

char :: Char -> Parser Char
char c = do
    d <- consume
    if d == c then return c else fail $ "Expected: " ++ show c ++ ", found: " ++ show d

string, symbol :: String -> Parser String
string [] = return ""
string (x:xs) = do
    c <- char x
    cs <- string xs
    return (c:cs)
symbol s = whitespaces >> string s

oneOfs :: [String] -> Parser String
oneOfs = map symbol >>> foldr (<|>) empty
oneOf :: [Char] -> Parser Char
oneOf = map char >>> foldr (<|>) empty

digit :: Parser Char
digit = oneOf ['0'..'9']

digits :: Parser String
digits = many digit




whitespace :: Parser ()
whitespace = fmap (const ()) $ oneOf ['\t', '\n', ' ']
whitespaces :: Parser ()
whitespaces = many whitespace >> return ()

atLeastOne :: Parser a -> Parser [a]
atLeastOne parser = liftA2 (:) parser (many parser)

chainr :: (Monad p, Alternative p) => p (a -> a -> a) -> p a -> p a
chainr parserOp parser = (do
    a <- parser
    op <- parserOp
    b <- chainr parserOp parser
    return $ op a b) <|> parser


chainl parserOp parser = do
    a <- parser
    f <- rest
    return $ f a
    where 
        rest = (do
            op <- parserOp
            b <- parser
            c <- rest
            return $ c . flip op b
            ) <|> return id


token :: Parser a -> Parser a
token p = whitespaces >> p

word :: Parser String
word = do
    whitespaces
    c <- oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']
    cs <- many . oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "0123456789"
    return (c:cs)

sepBy :: String -> Parser a -> Parser [a]
sepBy sep parser = chainr (symbol sep >> return (.)) (fmap (:) parser) <*> return []

num :: Parser Int
num = fmap (\x -> read x :: Int) . token . some . oneOf $ "0123456789"