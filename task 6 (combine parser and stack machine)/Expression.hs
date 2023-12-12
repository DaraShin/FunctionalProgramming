module Expression where

-- <Expr>       ::= <Sum>
-- <Sum>        ::= <Prod> { <AddOp> <Prod> }
-- <Prod>       ::= <Term> { <MulOp> <Term> }
-- <Term>       ::= <Numeral> | <Variable> | '(' <Sum> ')' 
-- <AddOp>      ::= '+' | '-'
-- <MulOp>      ::= '*' | '/'
-- <Numeral>    ::= <Digit> { <Digit> }

data AddOp = Add | Sub deriving (Show)
data MulOp = Mul | Div deriving (Show)
newtype Expr = Expr Sum deriving (Show)
data Sum = Sum Prod [(AddOp, Prod)] deriving (Show)
data Prod = Prod Term [(MulOp, Term)] deriving (Show)
data Term = Numeral Int | Variable String | Subexpr Sum deriving (Show)