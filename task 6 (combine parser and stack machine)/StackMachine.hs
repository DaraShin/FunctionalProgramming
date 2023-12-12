module StackMachine where

import Expression

data StackOp a
  = Push a
  | UnOp (a -> a)
  | BinOp (a -> a -> a)

applyStackOp :: StackOp a -> [a] -> [a]
applyStackOp (UnOp f) (x : xs) = (f x) : xs
applyStackOp (BinOp f) (x1 : (x2 : xs)) = (f x2 x1) : xs
applyStackOp (Push x) xs = x : xs


run :: [StackOp a] -> [a] -> [a]
run program stack = foldl (flip applyStackOp) stack program


