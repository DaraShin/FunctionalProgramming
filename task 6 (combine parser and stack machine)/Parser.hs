module Parser where

import Expression
import Data.Char
import Data.Functor
import Control.Applicative

newtype Parser a = Parser (String -> [(a, String)])

runParser :: Parser a -> String -> [(a, String)]
runParser (Parser f) s = f s 

instance Functor Parser where
    fmap f p = Parser g where
        g s = [(f y, t) | (y, t) <- runParser p s]

instance Applicative Parser where 
    pure x = Parser $ \s -> [(x, s)]
    Parser p <*> Parser q = Parser h where 
        h s = [(f y, s'') | (f, s') <- p s, (y, s'') <- q s']

instance Alternative Parser where
    empty = Parser $ const []
    p <|> q = Parser g where
        g s = (runParser p s) ++ (runParser q s)


acceptIf :: (Char -> Bool) -> Parser Char
acceptIf p = Parser f where
    f (c : cs) | p c = [(c, cs)]
    f _ = []

acceptChar c = acceptIf (==c)

letter :: Parser Char
letter = acceptIf isAlpha

digit :: Parser Char
digit = acceptIf isDigit

blank = acceptIf isSpace

ignoreIf :: (Char -> Bool) -> Parser ()
ignoreIf p = void (acceptIf p)

ignoreChar c = ignoreIf (==c)


addOpE :: Parser AddOp
addOpE = (const Add <$> acceptChar '+') <|> (const Sub <$> acceptChar '-')

mulOpE :: Parser MulOp
mulOpE = (const Mul <$> acceptChar '*') <|> (const Div <$> acceptChar '/')

sumE :: Parser Sum
sumE = liftA2 Sum prodE (many (liftA2 (,) addOpE prodE) )

prodE :: Parser Prod
prodE = liftA2 Prod termE (many (liftA2 (,) mulOpE termE))

numP :: Parser Term
numP = (Numeral . read) <$> (some digit)

subexprP :: Parser Term
subexprP = liftA3 (\_ x _ -> Subexpr x) (ignoreChar '(') sumE (ignoreChar ')')

varP :: Parser Term
varP  = Variable <$> (some letter)

termE :: Parser Term
termE = numP <|> varP <|> subexprP

exprParser :: Parser Expr
exprParser = Expr <$> sumE

parseExpr :: String -> Expr
parseExpr = requireComplete . head . runParser exprParser where
    requireComplete (x, "") = x
