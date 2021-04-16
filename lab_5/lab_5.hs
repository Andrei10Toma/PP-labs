import Prelude
import Data.List
-- Data types
-- data DataName = Type 1 | Type 2 | ... | Type N

-- data Operator = Plus | Minus | Mul | Div deriving (Show)

-- toOperator :: Char -> Operator -- '+' '-' etc
-- toOperator op
--     | op == '+' = Plus
--     | op == '-' = Minus
--     | op == '*' = Mul
--     | op == '/' = Div

-- insert :: Integer -> [Integer] -> [Integer]
-- insert x [] = [x]
-- insert x (y:ys)
--     | x < y = x:y:ys
--     | otherwise = y:insert x ys

-- insertSort :: [Integer] -> [Integer]
-- insertSort = foldr insert []

data Point = Point Float Float deriving (Show)

displayPoint :: Point -> (Float, Float)
displayPoint (Point x y) = (x, y)

data Natural = Zero | Succ Natural deriving (Show)

add :: Natural -> Natural -> Natural
add Zero x = x
add (Succ x) y = Succ (add x y)

-- Succ (Succ Zero) + Succ(Zero)

data IList = IVoid | ICons Int IList

data List a = Void | Cons a (List a)


sample = [("1", 1), ("2", 2), ("3", 3)]

-- 6
distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))

-- 7
collinear :: Point -> Point -> Point -> Bool
collinear (Point x1 y1) (Point x2 y2) (Point x3 y3)
    | x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2) == 0 = True
    | otherwise = False

-- 9
mul :: Natural -> Natural -> Natural
mul Zero _ = Zero
mul _ Zero = Zero
mul (Succ Zero) x = x
mul x (Succ Zero) = x
mul (Succ x) y = add y (mul x y)

-- 10
myLength :: List a -> Integer
myLength Void = 0
myLength (Cons x xs) = 1 + myLength xs

-- 11
toHaskell :: List a -> [a]
toHaskell Void = []
toHaskell (Cons x xs) = x:toHaskell xs

-- 12
data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show)
exampleTree = Node 1 (Node 2 (Node 4 (Node 6 Nil Nil) Nil) (Node 5 Nil Nil)) (Node 3 Nil Nil)

-- 13
height :: Tree a -> Integer 
height Nil = 0
height (Node key l r) = 1 + max (height l) (height r)

-- 14
size :: Tree a -> Integer 
size Nil = 0
size (Node key l r) = 1 + size l + size r

-- 15
mirror :: Tree a -> Tree a
mirror Nil = Nil
mirror (Node key l r) = Node key (mirror r) (mirror l)

-- 16
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Nil = Nil
treeMap mapFunc (Node key l r) = Node (mapFunc key) (treeMap mapFunc l) (treeMap mapFunc r)

-- 17
flatten :: Tree a -> [a]
flatten Nil = []
flatten (Node key l r) = key:(flatten l ++ flatten r)

data Student = Student String String [Float]

-- 18
testStudent1 = Student "Andrei" "Toma" [5, 4, 6]
testStudent3 = Student "Lucian" "Stan" [10, 10, 10]
testStudent2 = Student "Aldea" "Raluca" [2, 4, 6]
studentList = [testStudent1, testStudent2, testStudent3]
-- 19
avg :: Student -> Float
avg (Student _ _ grades) = sum grades / fromIntegral (length grades)

studComp :: Student -> Student -> Ordering 
studComp s1 s2 = compare (avg s1) (avg s2)

displayNameAndSurname :: Student -> String
displayNameAndSurname (Student name surname _) = name ++ " " ++ surname 

-- 20
highestAverage :: [Student] -> String
highestAverage = displayNameAndSurname.maximumBy studComp

data AExpr = Const Integer | Var String | Add AExpr AExpr | Mul AExpr AExpr

data BExpr = Eq AExpr AExpr | Not BExpr | Gt AExpr AExpr

type Context = [(String, Integer)]

context = [("a", 1), ("b", 2) , ("c", 3), ("d", 4), ("e", 5)]
expresionExample = Mul (Add (Add (Var "a") (Var "b")) (Var "e")) (Const 3)
expressionB = Gt expresionExample (Mul (Var "d") (Var "d"))

search :: Context -> String -> Integer 
search [] _ = -1
search (x:xs) varName
    | fst x == varName = snd x
    | otherwise = search xs varName

evalA :: Context -> AExpr -> Integer 
evalA context (Const value) = value
evalA context (Var name) = search context name
evalA context (Add first second) = evalA context first + evalA context second
evalA context (Mul first second) = evalA context first * evalA context second

evalB :: Context -> BExpr -> Bool
evalB context (Eq first second) = evalA context first == evalA context second
evalB context (Not expr) = not (evalB context expr)
evalB context (Gt first second) = evalA context first > evalA context second