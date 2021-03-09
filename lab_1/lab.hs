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

my_if :: Bool -> a -> a -> a
my_if condition ifTrue ifFalse =
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
max_3_integers_v3 a b c = head $ reverse $ sort [a, b, c]

-- exercitiul 13
third_last_number_odd :: [Integer] -> Bool
third_last_number_odd list = if (head $ tail $ tail $ reverse list) `mod` 2 == 0 then
                                False
                            else
                                True

-- exercitiul 14
list_sum :: [Integer] -> Integer
list_sum [] = 0
list_sum (head:tail) = head + list_sum tail

-- exercitul 15
list_booleans :: [Bool] -> Bool
list_booleans [] = True
list_booleans (False : tail) = False
list_booleans (True : tail) = list_booleans tail

-- exericitul 16
filter_odd_numbers :: [Integer] -> [Integer]
filter_odd_numbers [] = []
filter_odd_numbers list = [x | x <- list, x `mod` 2 == 0]

-- another version of solving
-- filter_odd_numbers (head : tail) = 
    -- if head `mod` 2 == 0 then
        -- [head] ++ filter_odd_numbers tai 
           -- else filter_odd_numbers tail

-- exericitiul 17
boolean_to_integer :: [Bool] -> [Integer]
boolean_to_integer [] = []
boolean_to_integer (True : tail) = 1 : boolean_to_integer tail
boolean_to_integer (False : tail) = 0 : boolean_to_integer tail

-- exercitiul 18
f_18 :: [[Integer]] -> [Bool]
f_18 [] = []
f_18 l = (g (head l)) : (f_18 (tail l))
    where
        g[] = True
        g l = h (tail l)
        h [] = True
        h l = False

-- exercitiul 19
sum_of_booleans :: [Bool] -> Integer
sum_of_booleans [] = 0
sum_of_booleans (True : tail) = 1 + sum_of_booleans tail
sum_of_booleans (False : tail) = 0 + sum_of_booleans tail

-- exercitiul 20
insert_list :: Integer -> [Integer] -> [Integer]
insert_list element [] = [element]
insert_list element (head : tail) | element < head = element : head : tail
                                  | otherwise = head : insert_list element tail

inssort :: [Integer] -> [Integer]
inssort [] = []
inssort (el : tail) = insert_list el (inssort tail)