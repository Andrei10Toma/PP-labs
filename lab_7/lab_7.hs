-- map []
-- fmap
-- fmap :: (Functor f) => (a -> b) -> f a -> f b

-- class Functor f where
    -- fmap :: (a -> b) -> f a -> f b

sumExtremeties :: [Int] -> Maybe Int
sumExtremeties [] = Nothing
sumExtremeties l = Just $ head l + last l

foo :: Int -> Bool
foo x = 2 * x > 3

-- 1
data BTree a = BVoid | BNode a (BTree a) (BTree a) deriving (Show)

data Tree a = Void | Node a [Tree a] deriving (Show)

tree1 = Node 1 [Node 2 [Node 3 []], Node 4[]]

tree2 = BNode 1 (BNode 2 (BNode 4 BVoid BVoid) (BNode 5 BVoid BVoid)) (BNode 3 (BNode 6 BVoid BVoid) BVoid)

-- Enroll Tree in Functor
instance Functor Tree where
    fmap _ Void = Void
    fmap f (Node val children) = Node (f val) (map (fmap f) children)

-- Enroll Tree in Foldable
instance Foldable Tree where
    foldr _ acc Void = acc
    foldr f acc (Node val children) = val `f` (foldr (\child acc -> foldr f acc child) acc children)

        -- 1
    -- 2   3   4
-- 5   6

-- X1 X2 X3 ... Xn
-- x1 op (x2 op (x3 op ... (xn op acc)))

-- type Matrix = [[Int]]
-- sum_matrix m = foldr (\row acc -> foldr (+) acc row) 0 m

-- 4
class Zippable z where
    zipp :: (a -> b -> c) -> z a -> z b -> z c

-- 5 Enroll Haskell lists in Zippable
instance Zippable [] where
    zipp _ [] _ = []
    zipp _ _ [] = []
    zipp f (x:xs) (y:ys) = f x y:zipp f xs ys

-- 6 Enroll Maybe in Zippable
instance Zippable Maybe where
    zipp _ Nothing _ = Nothing 
    zipp _ _ Nothing = Nothing 
    zipp f (Just el1) (Just el2) = Just (f el1 el2)

sumExtremeties2 :: [Int] -> Either String Int
sumExtremeties2 [] = Left "Eroare"
sumExtremeties2 l = Right (head l + last l)

-- 7 Enroll Either in Zippable
instance Zippable (Either a) where
    zipp _ (Left a) _ = Left a
    zipp _ _ (Left a) = Left a
    zipp f (Right a) (Right b) = Right (f a b)

-- 8 Enroll the binary tree
instance Zippable BTree where
    zipp _ BVoid _ = BVoid
    zipp _ _ BVoid = BVoid
    zipp f (BNode key1 l1 r1) (BNode key2 l2 r2) = BNode (f key1 key2) (zipp f l1 l2) (zipp f r1 r2)

-- 9 Enroll Tree in Zippable
instance Zippable Tree where
    zipp _ Void _ = Void
    zipp _ _ Void = Void
    zipp f (Node key1 children1) (Node key2 children2) = Node (f key1 key2) (zipWith (zipp f) children1 children2)


-- 10
instance Zippable ((,) a) where
    zipp f (s1, d1) (s2, d2) = (s1, d1 `f` d2)

-- 11
instance Zippable ((->) a) where
    zipp f g h = \x -> f (g x) (h x)

    -- g (x) = 2 * x
    -- h (x) = 5 * x
    -- f = (+)
    -- f = g(x) + h(x)
