import Data.List (unfoldr, foldl)

-- #1 Развернуть натуральное число n в список всех чисел, меньших его.

unfoldToLessNumStep 0 = Nothing
unfoldToLessNumStep n = Just (n, pred n)  
unfoldToLessNums n = unfoldr unfoldToLessNumStep (pred n)

-- #2 Развернуть число в список разрядов его двоичного представления.
toDigitsStep 0 = Nothing
toDigitsStep n = Just (n `mod` 2, n `div` 2) 
toDigits n = reverse $ unfoldr toDigitsStep n

-- #3 Список разрядов преобразовать свёрткой в значение числа.
fromDigits l = foldl ((+) . (*2)) 0 l  

-- #4 Развернуть число в список его простых делителей.
hasNoDivisorsAbove x y
   | y * y > x         = True
   | (x `mod` y) == 0  = False
   | otherwise         = hasNoDivisorsAbove x (succ y)
isPrime x = x > 1 && hasNoDivisorsAbove x 2

nextPrimeDivider n x
    | x == n                           = Nothing
    | n `mod` x == 0 && (isPrime x)    = Just (x, succ x)
    | otherwise                        = nextPrimeDivider n (succ x)

unfoldToPrimeDividers n = unfoldr (nextPrimeDivider n) 2

-- #5 Выразить список первых n чисел Фибоначчи через развёртку
fibonaccyStep (a, b, 0) = Nothing
fibonaccyStep (a, b, n) = Just (a, (b, a + b, pred n)) 
fibonaccy n = unfoldr fibonaccyStep (0, 1, n)

-- модификация: бесконечный список
fibonaccyInf = unfoldr  (\(a, b) -> Just (a, (b, a + b)) ) (0, 1)

-- #6 Развернуть число в сиракузскую последовательность
syracuseStep n 
    | n == 1            = Nothing
    | n `mod` 2 == 0    = Just (n `div` 2, n `div` 2)
    | otherwise         = Just (3 * n + 1, 3 * n + 1)
unfoldToSyracuse n = n : unfoldr syracuseStep n


-- #7 Выразить список простых чисел, не превышающих n, через развёртку
eratosthenSieveStep [] = Nothing
eratosthenSieveStep (x : l) = Just (x, filter (\a -> a `mod` x /= 0) l)
eratosthenSieve n = unfoldr eratosthenSieveStep [2..n]

-- модификация: бесконечный список всех простых чисел
eratosthenSieveInfStep (x : l) = Just (x, filter (\a -> a `mod` x /= 0) l)
eratosthenSieveInf = unfoldr eratosthenSieveInfStep [2..]




































































