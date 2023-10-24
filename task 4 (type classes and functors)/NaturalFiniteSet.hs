import Data.List ( (\\), sort )

class NaturalFiniteSet a b where
  contains :: a b -> b -> Bool
  toList :: a b -> [b]
  fromList :: [b] -> a b
  intersection :: a b -> a b -> a b
  union :: a b -> a b -> a b
  difference :: a b -> a b -> a b

intersectionHelper [] _ = []
intersectionHelper _ [] = []
intersectionHelper (x : xs) l = if (contains l x) then x : (intersection xs l) else (intersection xs l)

instance NaturalFiniteSet [] Integer where
  contains l x = elem x l
  toList l = l
  fromList l = sort l
  intersection l1 l2 = sort $ filter (\x -> elem x l2) l1
  union l1 l2 = sort $ ((difference l1 l2) ++ (difference l2 l1) ++ (intersection l1 l2))
  difference l1 l2 = sort $ filter (\x -> not $ elem x l2) l1

instance NaturalFiniteSet [] Bool where
  contains l x = elem x l
  toList l = l
  fromList l = l
  intersection l1 l2 = zipWith (&&) l1 l2
  union [] l2 = l2
  union l1 [] = l1 
  union (x : xs) (y : ys) = (x || y) : union xs ys
  difference l1 [] = l1
  difference [] _ = []
  difference (x : xs) (y : ys) = (x && not y) : difference xs ys
  
  