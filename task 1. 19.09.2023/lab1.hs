import Control.Monad (join)

-- #1 НОД
myGcd a b
    | a == b        = a
    | a > b         = myGcd (a - b) b
    | otherwise     = myGcd a (b - a)  

-- #2 возведение в степень
power _ 0 = 1
power x n = if (n `mod` 2)  == 0 then power (x*x) (n `div` 2) else x * (power x (n - 1)) 

powerHelper _ 0 p = p 
powerHelper x n p = if (n `mod` 2) == 0 then powerHelper (x * x) (n `div` 2) p else powerHelper (x * x) (n `div` 2) (p * x)
power2 x n = powerHelper x n 1

-- обощенное возведение в степень для использования с матрицами
-- f - умножение
-- idElem - нейтральный элемент умножения
generalPowerHelper _ 0 p _ = p 
generalPowerHelper x n p f = if (n `mod` 2) == 0 then generalPowerHelper (f x x) (n `div` 2) p f else generalPowerHelper (f x x) (n `div` 2) (f p x) f
generalPower x n f idElem= generalPowerHelper x n idElem f

-- numberPower x n = generalPower x n (*) 1

-- #3 Числа Фибоначчи
-- матрица 2x2:
-- a1 a2
-- a3 a4
matrMultiplier [a1, a2, a3, a4] [b1, b2, b3, b4] = [((a1 * b1) + (a2 * b3)), ((a1 * b2) + (a2 * b4)), ((a3 * b1) + (a4 * b3)), ((a3 * b2) + (a4 * b4))]
fib 0 = 0
fib n = head ( tail (generalPower [0, 1, 1, 1] n matrMultiplier [1,0,0,1] ))

-- #4 совершенные числа
divisorsSum x y s
    | y * y > x        = s
    | y * y == x       = s + y
    | x `mod` y == 0   = divisorsSum x (succ y) (s + y + (x `div` y))
    | otherwise        = divisorsSum x (succ y) s
isPerfect x = x == (divisorsSum x 2 1)

-- #5 Длина сиракузской последовательности 
syracuseHelper 1 l = l
syracuseHelper n l = if n `mod` 2 == 0 then syracuseHelper  (n `div` 2)  (succ l) else syracuseHelper  (3 * n + 1)  (succ l)
syracuseLength n = syracuseHelper n 1

-- #6 Числа Деланнуа
delannoySimple 0 _ = 1
delannoySimple _ 0 = 1
delannoySimple m n = (delannoySimple (pred m) n) + (delannoySimple (pred m) (pred n)) + (delannoySimple m (pred n)) 

xReverseHelper r [] = r
xReverseHelper r (x: l) = xReverseHelper (x:r) l
xReverse l = xReverseHelper [] l

delannoyColumn (_ : []) x = [x]
delannoyColumn (prevX : prevL) x = x : (delannoyColumn prevL (x + prevX + head(prevL)))
delannoyHelper 0 s = s
delannoyHelper n s = delannoyHelper (pred n) (delannoyColumn s 1)
delannoy m n = head $ xReverse $ delannoyHelper n (myClone (m + 1) [1])


-- #7 Вычисление многочлена (схема Горнера)
evalPolynomialHelper _ [] p = p 
evalPolynomialHelper x (c : cl) p = evalPolynomialHelper x cl (p * x + c)
evalPolynomial coeffs x = evalPolynomialHelper x coeffs 0

-- #8 Клонирование элементов списка
cloneHelper _ [] = []
cloneHelper 0 s = s
cloneHelper n s = cloneHelper (pred n) (map (\(x:l) -> x : (x:l)) s)
myClone 0 s = []
myClone n l = join $ cloneHelper (pred n) (map (\x -> [x]) l) 

-- #9 Сшивание списков бинарной операцией
xZipWith _ [] _ = []
xZipWith _ _ [] = []
xZipWith f (x1 : l1) (x2 : l2) = (f x1 x2) : xZipWith f l1 l2

-- #10 Список чисел Фибоначчи
-- бесконечный список обобщённых чисел Фибоначчи:
getFib _ 0 s = s
getFib (x : l) m s = getFib l (pred m) (s + x) 
infGeneralizedFibHelper l = fib  : infGeneralizedFibHelper (take m (fib : l)) where 
    m = length l
    fib = getFib l m 0
infGeneralizedFibonacci l = l ++ infGeneralizedFibHelper (xReverse l) 

-- бесконечный список чисел Фибоначчи:
infFibonacci = infGeneralizedFibonacci [0, 1] 
-- список из n первых чисел Фибоначчи:
fibonacciList n = take n infFibonacci 

-- список из n первых чисел Фибоначчи (без использования бесконечного списка):
fibListHelper 0 _ _ = []
fibListHelper n a b = a : (fibListHelper (pred n) b (a + b))
fibonacciList2 n = fibListHelper n 0 1 

-- #11 Системы счисления
fromDigits n l = evalPolynomial l n

toDigitsHelper n 0 l = l
toDigitsHelper n x l = toDigitsHelper n (x `div` n) ((x `mod` n) : l) 
toDigits n x = toDigitsHelper n x []

addDigitwiseHelper _ [] [] r s = if r == 1 then r : s else s
addDigitwiseHelper n (x1 : l1) [] r s = addDigitwiseHelper n l1 [] ((r + x1) `div` n) (((r + x1) `mod` n) : s)
addDigitwiseHelper n [] (x2 : l2) r s = addDigitwiseHelper n (x2 : l2) [] r s 
addDigitwiseHelper n (x1 : l1) (x2 : l2) r s = addDigitwiseHelper n l1 l2 ((r + x1 + x2) `div` n) (((r + x1 + x2) `mod` n) : s)
addDigitswise n l1 l2 = addDigitwiseHelper n (xReverse l1) (xReverse l2) 0 []

-- #12 Перечисление путей в решётке
delannoyRoutesColumn (_ : []) routeList  = [routeList]
delannoyRoutesColumn (prevX : prevL) routeList = routeList : (delannoyRoutesColumn prevL ( (map (\r -> 1 : r) prevX) ++ (map (\r -> 0 : r) (head prevL)) ++ (map (\r -> 2 : r) routeList)) )
delannoyRoutesHelper 0 routeListColumn = routeListColumn 
delannoyRoutesHelper n routeListColumn = delannoyRoutesHelper (pred n) (delannoyRoutesColumn routeListColumn (map (\r -> 0:r) (head routeListColumn)))
delannoyStartColumn 0 _ = []
delannoyStartColumn m route = ([route]) : (delannoyStartColumn (pred m) (2 : route))
delannoyRoutes m n = head $ xReverse $ delannoyRoutesHelper n (delannoyStartColumn (succ m) [])


























