-- Алгебраические выражения
data Expression = 
      Const Double
    | Variable String 
    | UnaryOperator String Expression 
    | BinaryOperator String Expression Expression

showExpr :: Expression -> String
showExpr (Const a) = show a
showExpr (Variable v) = v
showExpr (UnaryOperator op expr) = "(" ++ op ++ " " ++ (showExpr  expr) ++ ")"
showExpr (BinaryOperator op expr1 expr2) = "(" ++ (showExpr expr1) ++ " " ++ op ++ " " ++ (showExpr expr2) ++ ")"

evaluateExpr :: Expression -> [(String, Double)] -> Maybe Double
evaluateExpr (Const a) _ = Just a
evaluateExpr (Variable v) vars = lookup v vars
evaluateExpr (UnaryOperator op expr) vars = applyUnaryOperator op (evaluateExpr expr vars)
evaluateExpr (BinaryOperator op expr1 expr2) vars = applyBinaryOperator op (evaluateExpr expr1 vars) (evaluateExpr expr2 vars)

applyUnaryOperator :: String -> Maybe Double -> Maybe Double
applyUnaryOperator _ Nothing = Nothing
applyUnaryOperator op (Just a)
    | op == "sin"        = Just (sin a)
    | op == "cos"        = Just (cos a)
    | op == "tan"        = Just (tan a)
    | op == "sqrt"       = Just (sqrt a)
    | op == "exp"        = Just (exp a)
    | op == "log"        = Just (log a)
    | otherwise          = Nothing

applyBinaryOperator :: String -> Maybe Double -> Maybe Double -> Maybe Double
applyBinaryOperator _ Nothing _ = Nothing
applyBinaryOperator _ _ Nothing = Nothing
applyBinaryOperator op (Just a) (Just b)
    | op == "+"        = Just (a + b)
    | op == "-"        = Just (a - b)
    | op == "*"        = Just (a * b)
    | op == "/"        = Just (a / b) 
    | otherwise          = Nothing


simplifyExpr :: Expression -> Expression
simplifyExprHelper (UnaryOperator op (Const a)) = case (applyUnaryOperator op (Just a)) of
    Just b  -> Const b
    Nothing -> UnaryOperator op (Const a)
simplifyExprHelper (BinaryOperator op (Const a) (Const b)) = case (applyBinaryOperator op (Just a) (Just b)) of
    Just c  -> Const c
    Nothing -> BinaryOperator op (Const a) (Const b)
simplifyExprHelper (BinaryOperator "+" (Const 0) expr) = expr
simplifyExprHelper (BinaryOperator "+" expr (Const 0)) = expr
simplifyExprHelper (BinaryOperator "*" (Const 1) expr) = expr
simplifyExprHelper (BinaryOperator "*" expr (Const 1)) = expr
simplifyExprHelper (BinaryOperator "*" (Const 0) expr) = Const 0
simplifyExprHelper (BinaryOperator "*" expr (Const 0)) = Const 0
simplifyExprHelper (BinaryOperator "-" (Const a) (Const b)) = if a == b then (Const 0) else (BinaryOperator "-" (Const a) (Const b)) 
simplifyExprHelper (BinaryOperator "-" (Variable a) (Variable b)) = if (a == b) then (Const 0) else (BinaryOperator "-" (Variable a) (Variable b)) 
simplifyExprHelper expr = expr

simplifyExpr (UnaryOperator op expr) = simplifyExprHelper (UnaryOperator op (simplifyExpr expr))
simplifyExpr (BinaryOperator op expr1 expr2) = simplifyExprHelper (BinaryOperator op (simplifyExpr expr1) (simplifyExpr expr2))
simplifyExpr expr = simplifyExprHelper expr

differentiateExpr :: Expression -> String -> Maybe Expression
differentiateExpr(Const _) _ = Just (Const 0)
differentiateExpr (Variable x) diffVar = if x == diffVar then Just (Const 1) else Just (Const 0)
differentiateExpr (UnaryOperator op expr) var = 
    let d1 = derivative (UnaryOperator op expr) 
        d2 = differentiateExpr expr var 
    in
    case d1 of
      Nothing        -> Nothing
      Just expr      -> (case d2 of 
                         Nothing    -> Nothing 
                         Just expr2 -> Just (simplifyExpr (BinaryOperator "*" expr expr2)))
differentiateExpr (BinaryOperator op expr1 expr2) var =
    let d1 = (differentiateExpr expr1 var)
        d2 = (differentiateExpr expr2 var)
    in 
    case d1 of 
      Nothing        -> Nothing
      Just dExpr1    ->  case d2 of 
                           Nothing    -> Nothing 
                           Just dExpr2 -> binDerivative op expr1 expr2 dExpr1 dExpr2


derivative (UnaryOperator op expr)  
    | op == "sin"        = Just (UnaryOperator "cos" expr)
    | op == "cos"        = Just (BinaryOperator "*" (Const (-1)) (UnaryOperator "sin" expr))
    | op == "tan"        = Just (BinaryOperator "/" (Const 1) (BinaryOperator "*" (UnaryOperator "cos" expr) (UnaryOperator "cos" expr) ))
    | op == "sqrt"       = Just (BinaryOperator "/" (Const 1) (BinaryOperator "*" (Const 2) (UnaryOperator "sqrt" expr) ))
    | op == "exp"        = Just (UnaryOperator "exp" expr)
    | op == "log"        = Just (BinaryOperator "/" (Const 1) expr)
    | otherwise          = Nothing

binDerivative op expr1 expr2 d1 d2
    | op == "+"        = Just (simplifyExpr (BinaryOperator "+" d1 d2))
    | op == "-"        = Just (simplifyExpr (BinaryOperator "-" d1 d2))
    | op == "*"        = Just (simplifyExpr (BinaryOperator "+" (BinaryOperator "*" d1 expr2) (BinaryOperator "*" expr1 d2)))
    | op == "/"        = Just (simplifyExpr (BinaryOperator "/" (BinaryOperator "-" (BinaryOperator "*" d1 expr2) (BinaryOperator "*" expr1 d2)) (BinaryOperator "*" d2 d2)))
    | otherwise          = Nothing

showDerivative (Just expr) = showExpr expr
showDerivative Nothing = "Nothing"

{-
vars = [("y", 4.0)]
expr = BinaryOperator "+" (BinaryOperator "+" (Const 1) (BinaryOperator "*" (UnaryOperator "cos" (Const 2)) (Const 3))) (Variable "y") -- (1 + (cos 2 * 3)) + y
expr2 = BinaryOperator "+" (Const 2) (BinaryOperator "*" (Const 1) (Variable "x")) -- 2 + (1 * x)
expr4 = BinaryOperator "+" (Const 2) (BinaryOperator "*" (Const 2) (BinaryOperator "*" (Const 3) (Const 5)))  -- 2 + (2 * (3 * 5))
expr5 = UnaryOperator "sqrt" (BinaryOperator "+" (Const 2) (Const 3))  -- sqrt (2 + 3)
expr6 = BinaryOperator "+" (Const 0) (BinaryOperator "+" (Const 2) (Const 3)) -- (0 + (2 + 3))
expr7 = UnaryOperator "sqrt" (UnaryOperator "cos" (Variable "x"))  -- sqrt (cos x)
-- (cos x) * (sqrt (log (x*x))) 
expr8 =  BinaryOperator "*" (UnaryOperator "cos" (Variable "x")) (UnaryOperator "sqrt" (UnaryOperator "log" (BinaryOperator "*" (Variable "x") (Variable "x") )))
-}
