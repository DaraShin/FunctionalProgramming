data Tree a = 
      None 
    | Tree a (Tree a) (Tree a) 
    deriving(Show)

-- Поэлементное преобразование, сохраняющее структуру (аналог map)
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f None = None
treeMap f (Tree value leftSubtree rightSubtree) = Tree (f value) (treeMap f leftSubtree) (treeMap f rightSubtree) 

-- Подсчёт элементов
countElements :: Tree a -> Int
countElements None = 0
countElements (Tree value leftSubtree rightSubtree) = 1 + (countElements leftSubtree) + (countElements rightSubtree)


-- Обход в глубину treeTraverseD
treeTraverseD :: (a -> b -> b) -> b -> Tree a -> b
treeTraverseD _ b None = b
treeTraverseD f b (Tree value leftSubtree rightSubtree) = treeTraverseD f (f value (treeTraverseD f b leftSubtree)) rightSubtree

-- Обход в ширину treeTraverseW
treeTraverseW :: (a -> b -> b) -> b -> Tree a -> b
treeTraverseWHelper f b [] = b
treeTraverseWHelper f b (None : treeList) = treeTraverseWHelper f b treeList 
treeTraverseWHelper f b ((Tree value leftSubtree rightSubtree) : treeList) 
    = treeTraverseWHelper f (f value b) ((treeList ++ [leftSubtree]) ++ [rightSubtree]) 
treeTraverseW f b tree = treeTraverseWHelper f b [tree]