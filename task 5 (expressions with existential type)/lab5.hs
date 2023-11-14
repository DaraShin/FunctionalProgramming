class Expr a where
  toString :: a -> String
  eval :: a  -> a

data IntExpr 
  = ConstInt Int
  | BinExprInt BinOpInt IntExpr IntExpr
  | TernExprInt BoolExpr IntExpr IntExpr

getValueInt (ConstInt x) = x

data BinOpInt
  = Add
  | Sub
  | Mul
  | Div

instance Show BinOpInt where
  show Add = "+" 
  show Sub = "-" 
  show Mul = "*" 
  show Div = "/" 

evalBinOpInt :: BinOpInt -> Int -> Int -> Int
evalBinOpInt Add = (+)
evalBinOpInt Sub = (-)
evalBinOpInt Mul = (*)
evalBinOpInt Div = (div)

instance Expr IntExpr where
  toString (ConstInt x) = show x
  toString (TernExprInt cond x y) = (toString cond) ++ " ? " ++ (toString x) ++ " : " ++ (toString y)
  toString (BinExprInt op e1 e2) = "(" ++ (toString e1) ++ (show op) ++ (toString e2) ++ ")"
  eval (ConstInt x) = ConstInt x
  eval (BinExprInt op x y) = ConstInt (evalBinOpInt op (getValueInt $ eval x) (getValueInt $ eval y))
  eval (TernExprInt cond x y) = if (getValueBool $ eval cond) then eval x else eval y
  

data BoolExpr 
  = ConstBool Bool
  | BinExprBool BinOpBool BoolExpr BoolExpr
  | BinExprCmp BinOpCmp IntExpr IntExpr
  | TernExprBool BoolExpr BoolExpr BoolExpr

data BinOpBool 
  = And
  | Or

instance Show BinOpBool where
  show And = "&" 
  show Or = "|"

evalBinOpBool :: BinOpBool -> Bool -> Bool -> Bool
evalBinOpBool And = (&&)
evalBinOpBool Or = (||)

data BinOpCmp
  = Eq
  | Less
  | Gt
  | NotEq

instance Show BinOpCmp where
  show Eq = "=" 
  show Less = "<" 
  show Gt = ">" 
  show NotEq = "#" 

evalBinOpCmp :: BinOpCmp -> Int -> Int -> Bool
evalBinOpCmp Eq = (==)
evalBinOpCmp Less = (<)
evalBinOpCmp Gt = (>)
evalBinOpCmp NotEq = (/=)

getValueBool (ConstBool x) = x

instance Expr BoolExpr where
  toString (ConstBool x) = show x
  toString (BinExprBool op e1 e2) = "(" ++ (toString e1) ++ (show op) ++ (toString e2) ++ ")"
  toString (BinExprCmp op e1 e2) = "(" ++ (toString e1) ++ (show op) ++ (toString e2) ++ ")"
  toString (TernExprBool cond x y) = (toString cond) ++ " ? " ++ (toString x) ++ " : " ++ (toString y)
  eval (ConstBool x) = ConstBool x
  eval (BinExprBool op x y) = ConstBool (evalBinOpBool op (getValueBool $ eval x) (getValueBool $ eval y))
  eval (BinExprCmp op x y) = ConstBool (evalBinOpCmp op (getValueInt $ eval x) (getValueInt $ eval y))
  eval (TernExprBool cond x y) = if (getValueBool $ eval cond) then eval x else eval y

data SomeExpr = forall a . Expr a => SomeExpr a



showSomeExpr ::  SomeExpr -> String
showSomeExpr (SomeExpr e) = toString e

evalSomeExpr ::  SomeExpr -> SomeExpr
evalSomeExpr (SomeExpr e) = SomeExpr (eval e)

expr1 = BinExprInt Add (ConstInt 1) (ConstInt 2)
expr2 = BinExprInt Mul (ConstInt 5) (ConstInt 2)
expr3 = BinExprCmp Less expr1 expr2
expr4 = TernExprInt expr3 (ConstInt 1) (ConstInt 2)
l1 = [SomeExpr expr1, SomeExpr expr2, SomeExpr expr3, SomeExpr expr4]

--  map showSomeExpr l1
--  map (\x -> showSomeExpr (evalSomeExpr x)) l1
