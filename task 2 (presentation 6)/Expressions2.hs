-- Алгебраические выражения с указанием выполняемых операций
module Expressions2 where 

import Data.Map (Map, (!), fromList)

data Expr unop binop a
    = UnOpNode unop (Expr unop binop a)
    | BinOpNode binop (Expr unop binop a) (Expr unop binop a)
    | ConstE a
    | Var String

type UnDecoderE unop a = unop -> (a -> a)
type BinDecoderE binop a = binop -> (a -> a -> a)
type Binding a = String -> a

eval :: UnDecoderE unop a -> BinDecoderE binop a -> Binding a -> Expr unop binop a -> a
eval _ _ _ (ConstE x) = x
eval _ _ binding (Var s) = binding s
eval decodeUn decodeBin binding (BinOpNode op e1 e2) = let
    v1 = eval decodeUn decodeBin binding e1
    v2 = eval decodeUn decodeBin binding e2
    f  = decodeBin op
  in f v1 v2
eval decodeUn decodeBin binding (UnOpNode op e) = let
    v  = eval decodeUn decodeBin binding e
    f  = decodeUn op
  in f v

-- type FloatingExpr = Expr FloatingUnOp FloatingBinOp Double

-- evalFloat varMap = eval (floatingUnSemantic !) (floatingBinSemantic !) (varMap !)



