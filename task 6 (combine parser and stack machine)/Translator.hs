import Parser
import StackMachine
import Expression
import Data.Map ((!), empty, fromList, Map)

stAdd = BinOp (+)
stSub = BinOp (-)
stMul = BinOp (*)
stDiv = BinOp (/) 

newtype Memory a = Memory (Map String a) deriving (Show)

termToStack :: Term -> Memory Float -> [StackOp Float] -> [StackOp Float]
termToStack (Numeral x) _ st =  (Push (fromIntegral x)) : st
termToStack (Variable v) (Memory varMap) st =  (Push (varMap ! v)) : st
termToStack (Subexpr x) mem st = sumToStack x mem st

prodToStack :: Prod -> Memory Float -> [StackOp Float] -> [StackOp Float]
prodToStack (Prod a bl) mem st = termToStack a mem (foldr  (\(mulOp, t) curStack  -> (termToStack t mem ((mulOpToStackOp mulOp) : curStack))) st bl) where
    mulOpToStackOp Mul = stMul
    mulOpToStackOp Div = stDiv

sumToStack :: Sum -> Memory Float -> [StackOp Float] -> [StackOp Float]
sumToStack (Sum a bl) mem st = prodToStack a mem (foldr  (\(sumOp, t) curStack  -> (prodToStack t mem ((sumOpToStackOp sumOp) : curStack))) st bl) where
    sumOpToStackOp Add = stAdd
    sumOpToStackOp Sub = stSub

exprToStack :: Expr -> Memory Float -> [StackOp Float] -> [StackOp Float]
exprToStack (Expr s) mem st = sumToStack s mem st


parseAndRun :: String -> Memory Float -> [Float] -> [Float]
parseAndRun s mem l = run (exprToStack (parseExpr s) mem []) []


-- вычисление выражения без стековой машины
parseAndEvalExpr :: String -> Memory Float -> Float
parseAndEvalExpr s mem = evalExpr (parseExpr s) mem

evalExpr :: Expr -> Memory Float -> Float
evalExpr (Expr s) mem = evalSum s mem 

evalSum :: Sum -> Memory Float -> Float
evalSum (Sum a bs) mem = (evalProd a mem) + (sum $ map f bs) where
    f (op, b) = g op $ evalProd b mem
    g Add = id
    g Sub = negate

evalProd :: Prod -> Memory Float -> Float
evalProd (Prod a bs) mem = evalTerm a mem * (product $ map f bs) where
    f (op, b) = g op $ evalTerm b mem
    g Mul = id
    g Div = recip

evalTerm :: Term -> Memory Float -> Float
evalTerm (Numeral x) _ = fromIntegral x
evalTerm (Subexpr x) mem = evalSum x mem
evalTerm (Variable v) (Memory varMap) = varMap ! v


{-
emptyMem :: Memory Float
emptyMem = Memory empty

varMap :: Map String Float
varMap = fromList [ ("x", 1.0), ("y", 5) ]
mem = Memory varMap

expr = "(1+2)*(10-5)/y+x"
-}
