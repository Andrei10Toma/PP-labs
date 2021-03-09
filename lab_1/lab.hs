-- :l[oad] <fisier1.hs> <fisier2.hs> ... <fisierN.hs>
-- :r[reload]
-- :q [quit]

-- functionName :: <par1Type> -> <par2Type> -> ... -> <parNType> -> <returnType>

-- function functionName(par1Type par1, par2Type par2, ... ) {
--  returnType result = calcule()
--  return result
-- }

-- data types:
--      integers: Integer / Int
--      booleans: Bool
--      char: Char
--      floats: Float
--      any: a
--      String: [Char]

--      lists: [Integer], [Bool], ...
--      pairs: (Integer, Bool), ...

import Data.List

identityInteger :: Integer -> Integer
identityInteger x = x

identity :: a -> a
identity x = x

-- 1. Write a constant function in Haskell.
-- 2. Write the haskell implementation of the function below:
--      f(x, y) = x (Ox projection of the function)

constant :: a -> Integer
constant _ = 10

projection :: Integer -> Integer -> Integer
projection x y = x


-- And implementation
myAnd :: Bool -> Bool -> Bool
myAnd a b =
    a && b

myAndV2 :: Bool -> Bool -> Bool
myAndV2 False False = False
myAndV2 True False = False
myAndV2 False True = False
myAndV2 True True = True

--[1, 2, 3, 4]
-- operatori: ":", "++"
-- observatori: head, tail, init, last

example = [1, 2, 3, 4, 5, 6]

myreverse :: [Integer] -> [Integer]
myreverse [] = []
-- myreverse list = (myreverse (tail list)) ++ [(head list)]
myreverse (head:tail) = myreverse tail ++ [head]
-- [1, 2, 3, 4]
-- (1:[2, 3, 4])

-- [1, 2, 3] => 3: (rev[1, 2])
-- rev [1, 2, 3, 4] => (rev[2, 3, 4]) ++ [1] => 
--                  => (rev[3, 4]) ++ [2] ++ [1] =>
--                  => (rev[4]) ++ [3] ++ [2] ++ [1] =>
--                  => (rev[]) ++ [4] ++ [3] ++ [2] ++ [1]

myF :: Bool -> a -> a -> a
myF condition ifTrue ifFalse =
    if condition then
        ifTrue
    else ifFalse


max_3_integers :: Integer -> Integer -> Integer -> Integer
max_3_integers a b c = max a (max b c)

max_3_integers_v2 :: Integer -> Integer -> Integer -> Integer
max_3_integers_v2 a b c
  | a >= b && a >= c =
      a
  | b >= a && b >= c =
      b
  | otherwise = c


max_3_integers_v3 :: Integer -> Integer -> Integer -> Integer
max_3_integers_v3 a b c = maximum [a, b, c]

-- exercitiul 13
thirdLastNumberOdd :: [Integer] -> Bool
thirdLastNumberOdd list = ((head $ tail $ tail $ reverse list) `mod` 2) /= 0

-- exercitiul 14
listSum :: [Integer] -> Integer
listSum [] = 0
listSum (head : tail) = head + listSum tail

-- exercitul 15
listBooleans :: [Bool] -> Bool
listBooleans [] = True
listBooleans (False : tail) = False
listBooleans (True : tail) = listBooleans tail

-- exericitul 16
filterOddNumbers :: [Integer] -> [Integer]
filterOddNumbers [] = []
filterOddNumbers list = [x | x <- list, x `mod` 2 == 0]

-- another version of solving
-- filterOddNumbers (head : tail) = 
    -- if head `mod` 2 == 0 then
        -- [head] ++ filterOddNumbers tail 
           -- else filterOddNumbers tail

-- exericitiul 17
booleanToInteger :: [Bool] -> [Integer]
booleanToInteger [] = []
booleanToInteger (True : tail) = 1 : booleanToInteger tail
booleanToInteger (False : tail) = 0 : booleanToInteger tail

-- exercitiul 18
f_18 :: [[Integer]] -> [Bool]
f_18 [] = []
f_18 l = g (head l) : f_18 (tail l)
    where
        g[] = True
        g l = h (tail l)
        h [] = True
        h l = False

-- exercitiul 19
sumOfBooleans :: [Bool] -> Integer
sumOfBooleans [] = 0
sumOfBooleans (True : tail) = 1 + sumOfBooleans tail
sumOfBooleans (False : tail) = 0 + sumOfBooleans tail

-- exercitiul 20
insertList :: Integer -> [Integer] -> [Integer]
insertList element [] = [element]
insertList element (head : tail) | element < head = element : head : tail
                                  | otherwise = head : insertList element tail

insSort :: [Integer] -> [Integer]
insSort [] = []
insSort (head : tail) = insertList head $ insSort tail