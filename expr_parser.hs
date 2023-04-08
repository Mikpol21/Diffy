module ExprParser where
import Expr
import Env
import Parser
import Control.Applicative
import Stmt

import Prelude hiding (fail, lookup)




parse_sequence, parse_stmt :: Parser Stmt
parse_sequence = chainl (symbol ";" >> return Sequence) (parse_var_decl <|> parse_func_decl <|> fmap Expression parse_expr <|> parse_command)
parse_stmt = parse_sequence

parse_var_decl :: Parser Stmt
parse_var_decl = do
    symbol "let"
    name <- parse_var_name
    symbol "="
    expr <- parse_expr
    return $ VarDecl name expr

parse_func_decl :: Parser Stmt
parse_func_decl = do
    symbol "func"
    name <- parse_var_name
    symbol "["
    args <- sepBy "," parse_var_name
    symbol "]"
    symbol "{"
    expr <- parse_expr
    symbol "}"
    return $ FuncDecl name (\args' -> foldr (\(x, y) e -> substitute e x y) expr (zip args args')) (length args)

parse_expr :: Parser Expr
parse_expr = (parse_let <|> parse_add)

parse_let :: Parser Expr
parse_let = do
    symbol "let"
    name <- parse_var_name
    symbol "="
    expr <- parse_expr
    symbol ";"
    body <- (parse_let <|> parse_add)
    return (Let name expr body)

parse_add :: Parser Expr
parse_add = chainl (symbol "+" >> return (:+:)) (parse_sub)

parse_sub :: Parser Expr
parse_sub = chainl (symbol "-" >> return (:-:)) (parse_mul)

parse_mul :: Parser Expr
parse_mul = chainl ((symbol "*" <|>  (fmap (const " ") whitespaces)) >> return (:*:)) (parse_div)

parse_div :: Parser Expr
parse_div = chainl (symbol "/" >> return (:/:)) (parse_pow)

parse_pow :: Parser Expr
parse_pow = chainr (symbol "^" >> return (:^:)) (parse_app)

parse_app :: Parser Expr
parse_app = (do
    func <- parse_var_name
    symbol "["
    args <- sepBy "," (parse_expr)
    symbol "]"
    return (Apply func args)) <|> parse_exp

parse_exp :: Parser Expr
parse_exp = (do
    symbol "exp"
    symbol "("
    x <- parse_expr
    symbol ")"
    return (Exp x)) <|> parse_log

parse_log :: Parser Expr
parse_log = (do
    symbol "log"
    symbol "("
    x <- parse_expr
    symbol ")"
    return (Log x)) <|> parse_var

parse_var :: Parser Expr
parse_var = fmap Var parse_var_name <|> parse_const

parse_const :: Parser Expr
parse_const = fmap Expr.Const parse_double <|> parse_parens

parse_parens :: Parser Expr
parse_parens = do
    symbol "("
    x <- parse_expr
    symbol ")"
    return x


parse_var_name :: Parser String
parse_var_name = do
    name <- word
    if name `elem` forbidden_words then fail "Invalid variable name" else return name

forbidden_words :: [String]
forbidden_words = ["exp", "log", "let", "func", "grad", "let", "var"] ++ commands

parse_double :: Parser Double
parse_double = ((symbol "-" >> return ((-1) * )) <|> return id) <*> (do
    x <- num
    char '.'
    y <- num
    return (fromIntegral x + fromIntegral y / (10.0 ^ (length $ show y)))) <|> (fmap fromIntegral num :: Parser Double)

commands :: [String]
commands = ["grad", "root", "minimize"]

parse_command :: Parser Stmt
parse_command = do
    name <- oneOfs commands
    symbol "["
    args <- sepBy "," parse_argument
    symbol "]"
    return $ Command name args

parse_argument :: Parser Argument
parse_argument = (do
    symbol "var"
    name <- parse_var_name
    return $ ArgVar name) <|> (do
    x <- parse_double
    return $ ArgConst x) <|> (do
    x <- parse_expr
    return $ ArgExpr x)