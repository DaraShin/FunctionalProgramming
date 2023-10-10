import Data.Map (Map, (!), (!?), fromList, member)
import Text.Read (readMaybe)
import Expressions2

data Operand a
  = Const a
  | Registry String
  deriving (Show)

data ArithmOperator unOp binOp
  = UnaryOperator unOp
  | BinaryOperator binOp
  deriving (Show)

data Command unOp binOp a 
  = Push (Operand a)
  | Pop
  | ArithmOperator (ArithmOperator unOp binOp)
  deriving (Show)

data MachineState a = MachineState 
  {
    stack  :: [a],
    memory :: Map String a
  }
  deriving (Show)

type Program unOp binOp a = [Command unOp binOp a]


type UnDecoder unOp a = unOp -> (a -> a)
type BinDecoder binOp a = binOp -> (a -> a -> a)
type ArithmOpParser unOp binOp = String -> Either String (ArithmOperator unOp binOp)


data IntUnaryOperator
  = Neg
  | Inc
  | Dec
  deriving (Show, Eq, Ord)

data IntBinaryOperator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  deriving (Show, Eq, Ord)

intUnaryOperatorSemantic = fromList [
    (Neg, negate),
    (Inc, (1+)),
    (Dec, (1-))
  ]

intBinaryOperatorSemantic = fromList [
    (Add, (+)),
    (Sub, (-)),
    (Mul, (*)),
    (Div, (div)),
    (Mod, (mod))
  ]

intOperatorStrings = fromList [
    ("neg", (UnaryOperator Neg)),
    ("inc", (UnaryOperator Inc)),
    ("dec", (UnaryOperator Dec)),
    ("add", (BinaryOperator Add)),
    ("sub", (BinaryOperator Sub)),
    ("mul", (BinaryOperator Mul)),
    ("div", (BinaryOperator Div)),
    ("mod", (BinaryOperator Mod))
  ]

---------------------------------------------------------

executeCommand :: UnDecoder unOp a -> BinDecoder binOp a -> Either String (MachineState a) -> Command unOp binOp a -> Either String (MachineState a) 
executeCommand _ _ (Left error) _ = Left error
executeCommand _ _  (Right (MachineState stack memory)) (Push (Const x)) = Right (MachineState (x : stack) memory)
executeCommand _ _ (Right (MachineState stack memory)) (Push (Registry x)) = 
  if member x memory
  then Right (MachineState ( (memory ! x) : stack) memory)
  else Left ("Error: Unknown push operand: " ++ x)
executeCommand _ _ (Right (MachineState stack memory)) Pop = 
  if ((not . null) stack)
  then Right (MachineState (tail stack) memory)
  else Left "Error: Pop from empty stack"
executeCommand unDecoder _ (Right (MachineState [] memory)) (ArithmOperator (UnaryOperator op)) = Left "Error: not enough operands"
executeCommand unDecoder _ (Right (MachineState (hStack : tStack) memory)) (ArithmOperator (UnaryOperator op)) = 
  Right (MachineState (res : tStack) memory)
  where res = unDecoder op hStack
executeCommand _ binDecoder (Right (MachineState [] memory)) (ArithmOperator (BinaryOperator op)) = Left "Error: not enough operands"
executeCommand _ binDecoder (Right (MachineState (hStack : tStack) memory)) (ArithmOperator (BinaryOperator op)) = 
  if null tStack 
  then Left "Error: not enough operands"
  else Right (MachineState ( (binDecoder op hStack (head tStack)) : (tail (tStack))) memory)

-- Функция для прогона программы на исходном стеке
executeProgram :: UnDecoder unOp a -> BinDecoder binOp a -> Program unOp binOp a -> MachineState a -> Either String (MachineState a)
executeProgram unDecoder binDecoder prog machineState = foldl (executeCommand unDecoder binDecoder) (Right machineState) prog

executeProgramInt = executeProgram (intUnaryOperatorSemantic !) (intBinaryOperatorSemantic !) 

---------------------------------------------------------

lineToCommand :: (Read a) => ArithmOpParser unOp binOp -> String -> Either String (Command unOp binOp a)
lineToCommand opParser line 
  | cmdWord == "push" && (length lineWords < 2)  = Left "Error: No operand"
  | cmdWord == "push" && (length lineWords > 2)  = Left "Error: Too much operands"
  | cmdWord == "push" && (length lineWords == 2) = 
      case (readMaybe (head (tail lineWords)) ) of 
          Just val    -> Right (Push (Const val))
          Nothing     -> Right (Push (Registry (head (tail lineWords))))
  | cmdWord == "pop"     = Right (Pop)
  | not (null cmdWord)   = 
      case (opParser cmdWord) of
          Right arithmOp    -> Right (ArithmOperator arithmOp)   
          Left error        -> Left error   
  | otherwise            = Left "Error while parsing program"
  where lineWords = words line
        cmdWord = head lineWords


linesToProgram :: (Read a) => ArithmOpParser unOp binOp -> [String] -> Either String (Program unOp binOp a)
linesToProgram  _ [] = Right []
linesToProgram  opParser (line : lineList) = 
  let cmd  = lineToCommand opParser line 
      prog = linesToProgram  opParser lineList
  in
  case cmd of
    Right cmdVal -> case prog of 
                         Right progVal -> Right (cmdVal : progVal)
                         Left error    -> Left error
    Left error   -> Left error


-- Функция для преобразования программы из текста в код
textToProgram :: (Read a) => ArithmOpParser unOp binOp -> String -> Either String (Program unOp binOp a)
textToProgram opParser progText = linesToProgram opParser (filter (not . null) (lines progText))


intCommandsParser :: ArithmOpParser IntUnaryOperator IntBinaryOperator 
intCommandsParser str = case (intOperatorStrings !? str) of 
    Just cmd    -> Right cmd
    Nothing     -> Left "Error: Unknown operator"

textToProgramInt :: (Read a) => String -> Either String (Program IntUnaryOperator IntBinaryOperator a)
textToProgramInt = textToProgram intCommandsParser

------------------------------------------------------------

type IntExpr = Expr IntUnaryOperator IntUnaryOperator Int
evalInt varMap = eval (intUnaryOperatorSemantic  !) (intBinaryOperatorSemantic  !) (varMap !)


exprTreeToProgramHelper :: Expr unOp binOp a -> Program unOp binOp a -> Program unOp binOp a
exprTreeToProgramHelper (ConstE x) prog = (Push (Const x)) : prog
exprTreeToProgramHelper (Var x) prog = (Push (Registry x)) : prog
exprTreeToProgramHelper (UnOpNode op expr) prog = exprTreeToProgramHelper expr ((ArithmOperator (UnaryOperator op)) : prog)
exprTreeToProgramHelper (BinOpNode op expr1 expr2) prog = 
  let subProg = exprTreeToProgramHelper expr1 ((ArithmOperator (BinaryOperator op)) : prog)
  in
  exprTreeToProgramHelper expr2 subProg

-- Функция преобразования дерева выражения в программу стековой ВМ
exprTreeToProgram :: Expr unOp binOp a -> Program unOp binOp a
exprTreeToProgram expr = exprTreeToProgramHelper expr []

------------------------------------------------------------

text = 
  "push 5\n\
\push 4\n\
\push x\n\
\mul\n\
\push 2\n\
\push x\n\
\push x\n\
\mul\n\
\mul\n\
\add\n\
\add"

text2 = 
  "push 5\n\
\mul"

state = MachineState [] (fromList [ ("x", 3) ])

testExecution progText mState = case (textToProgramInt progText) of
    Right prog    -> show (executeProgramInt prog state)
    Left error    -> error

expr = BinOpNode Add (BinOpNode Add (BinOpNode Mul (BinOpNode Mul (ConstE 2) (Var "x")) (Var "x")) (BinOpNode Mul (ConstE 4) (Var "x"))) (ConstE 5)






























