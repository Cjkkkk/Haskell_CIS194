data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving (Show , Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

treeSize :: Tree a -> Integer
treeSize Empty = 0
treeSize (Node a _ c) = 1 + treeSize a + treeSize c

-- 计算树的所有节点之和
treeSum :: Tree Integer -> Integer
treeSum Empty = 0
treeSum (Node a b c) = b + treeSum a + treeSum c

-- 计算树的高度
treeDepth :: Tree a -> Integer
treeDepth Empty = 0
treeDepth (Node a _ c) = 1 + max (treeDepth a) (treeDepth c) 

-- 把树拍平
flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node a b c) = flatten a ++ [b] ++ flatten c

-- empty case
-- 如何操作左右子树的结果
treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty = e
treeFold e f (Node a b c) = f (treeFold e f a) b (treeFold e f c) 

treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\a _ c -> 1 + a + c) 

-- class Monoid' m where
--     mempty' :: m
--     mappend' :: m -> m -> m
--     mconcat' :: [m] -> m

-- (<>) :: Monoid' m => m -> m ->m
-- (<>) = mappend'

-- newtype Sum a = Sum a
--   deriving (Eq, Ord, Num, Show)

-- getSum :: Sum a -> a
-- getSum (Sum a) = a

-- instance Num a => Monoid (Sum a) where
--   mempty  = Sum 0
--   mappend = (+)

-- newtype Product a = Product a
--   deriving (Eq, Ord, Num, Show)

-- getProduct :: Product a -> a
-- getProduct (Product a) = a

-- instance Num a => Monoid (Product a) where
--   mempty  = Product 1
--   mappend = (*)