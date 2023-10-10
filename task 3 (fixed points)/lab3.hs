-- Задачи на неподвижные точки
import Data.Function (fix)

-- #1 наибольший общий делитель
gcdHelper f = \a b -> if a == b then 0 else
  if a > b then f (a - b) b else f a (b - a)
myGcd = fix gcdHelper

-- #2 бесконечный список чисел Фибоначчи
fibListHelper f = \a b -> a : (f b (a + b))
fibonacciList = fix fibListHelper 0 1 

-- #3 вычисление числа Фибоначчи по номеру
fibHelper f = \a b n -> if n == 0 then a else f b (a + b) (pred n)
fibonacci  = fix fibHelper 0 1

-- #4 вычисление суммы списка
sumHelper f = \s l -> if null l then s else f (s + (head l)) (tail l)
mySum = fix sumHelper 0   

-- #5 нахождение наименьшего элемента в списке
minHelper f = \m l -> if (null l) then m else 
  if head l < m then f (head l) (tail l) else f m (tail l)
myMin = fix minHelper (maxBound :: Int)

-- #6 реверс списка
reverseHelper f = \r l -> if null l then r else f ((head l) : r) (tail l)
reverseList = fix reverseHelper []
