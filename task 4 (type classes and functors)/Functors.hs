data Pair a = Pair a a

instance Functor Pair a where 
  fmap f (Pair x y) = Pair (f x) (f y)


data Labelled e a = Labelled e a

instance Functor Labelled e a where
  fmap f (Labelled label val) = Labelled label (f val) 


data OneOrTwo a = One a | Two a a

instance Functor OneOrTwo a where
  fmap f (One val1) = One (f val1)
  fmap f (Two val1 val2) = Two (f val1) (f val2)


data Either e a = Left e | Right a


instance Functor Either e a where
  fmap _ (Left e) = Left e
  fmap f (Right a) = Right (f a)


data MultiTree a = Leaf | Node a [MultiTree a]

instance Functor MultiTree a where
  fmap _ Leaf = Leaf
  fmap f (Node val children) = Node (f val) (map (\node -> fmap f node) children)


data Stream = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons val tail) = Cons (f val) (fmap f tail)