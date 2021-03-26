import Data.Char
import Data.Bits

l = ["maTei@gmail.com", "mihAi@gmail.com", "tEst@mail.com", "emAil@mail.com", "short@AX.ro"]

-- functiile de ordin superior sunt functii care primesc ca parametru alte functii

-- map (+1) [1, 2, 3, 4] -> [2, 3, 4, 5]
mymap op (x:xs) = (op x):(mymap op xs)
mymap _ [] = []

-- filter (>0) [-2, -1, 0, 1, 2] -> [1, 2]
myfilter _ [] = []
myfilter op (x:xs)
    | (op x) = x:(myfilter op xs)
    | otherwise = myfilter op xs

-- fold
-- foldr[ight]
-- foldr op acc [x1, x2, ... , xn] = x1 op (x2 op (...(xn-1 op (xn op acc))))
-- foldr (/) 2 [8, 16] = 1          8 / (16 / 2) = 1
-- foldr (/) 2 [16, 8] = 4          16 / (8 / 2) = 4

-- foldl[left]
-- foldl op acc [x1, x2, ..., xn] = (((acc op x1) op x2)...) op xn
-- foldl (/) 2 [8, 16]             (2 / 8) / 16
-- foldl (/) 16 [8, 2]              acelasi rezultat ca mai sus

-- 1.1. Remove upperscases from emails

sample_1 = "MaTei@gmail.com" -- -> "matei@gmail.com"
remUpper = map (map toLower)

-- 1.2. Write a function which removes emails longer than a given size
longer :: Int -> [String] -> [String]
longer size = filter (\el -> length el <= size)

-- 1.3
-- count [] = 0
-- count (x:xs) = 1 + count(xs)

-- [1, 2, 3, 4]
-- foldr op acc [1, 2, 3, 4]
-- foldr op 0 [1, 2, 3, 4]
-- foldr op 1 [2, 3, 4]

count list = foldr (\x acc -> 1 + acc) 0 list

howMany = (foldr (\x acc -> 1 + acc) 0).(longer 14)

fx x = (g.h) x
    where
        g x = 2 * x
        h x = 5 + x

-- "ana@mere" -> ["ana", "mere"]
-- 'e' [] ->  [["e"]]
-- 'r' [["e"]] -> [["re"]]
-- 'e' [["re"]] -> [[["ere"]]]
-- 'm' ...
-- '@' [["mere"]] -> [[], ["mere"]]
-- 'a' [[] ,["mere"]] -> [["a"], ["mere"]]
mySplit :: String -> [String]
mySplit = foldr op []
    where
        op '@' acc = [] : acc
        op c [] = [[c]]
        op c (y:ys) = (c:y):ys

namesEmails :: [String] -> [[String]]

-- [["matei", "gmail.com"], ["mihai", "gmail.com"]]
namesEmails = map mySplit

-- "ana@mere" -> "mere"
-- 'e' -> "e"
-- 'r' -> "re"
-- 'e' -> "ere"
-- 'm' -> "mere"
-- '@' -> stop

-- 1.5 Identify the list of the employed domains names (e.g gmail.com)

getDomains :: [String] -> [String]
getDomains = map (head.(tail.mySplit))
remDuplicates :: [String] -> [String]
remDuplicates = foldr op []
    where
        op el acc
            | alreadyFound el acc = acc
            | otherwise = el:acc
                -- where
                    -- alreadyFound el acc = length (filter (\x -> x == el) acc) /= 0
        alreadyFound el = foldr (opp el) False
        opp _ _ True = True
        opp y x acc
            | y == x = True
            | otherwise = acc

domains:: [String] -> [String]
domains = remDuplicates.getDomains

{-
    "matei@gmail.com" -> ["matei", "gmail"]
    [] 'm' -> ["m"]
    ["m"] 'a' -> ["ma"]
    ["ma"] 't' -> ["mat"]
    ["mat"] 'e' -> ["mate"]
    ["mate"] 'i' -> ["matei"]
    ["matei"] '@' -> ["matei", ""]
    ["matei", ""] g -> ["matei", "g"]
    ["matei", "g"] 'm' -> ["matei", "gm"]
-}
splitL :: String -> [String]
splitL = reverse.foldl op []
    where
        op [] c = [[c]]
        op acc '@' = [] : acc
        op (y:ys) c = (y ++ [c]):ys

splitBy :: Char -> String -> [String]
splitBy del = reverse.foldl op []
    where
        op [] c = [[c]]
        op acc@(y:ys) c
            | c == del = [] : acc
            | otherwise = (y ++ [c]):ys

domain :: [String] -> [String]
domain = map (head.splitBy '.'.last.splitBy '@')

s1 1 = True
s1 2 = True
s1 _ = False

s2 x = mod x 2 == 0

s3 _ = False

mem :: (Integer -> Bool) -> Integer -> Bool
mem set el = set el

-- set for 2^n
-- set for natural numbers
sNatural :: Integer -> Bool
sNatural = (>= 0)

sPower2 :: Integer -> Bool
sPower2 x = x  .&. (x - 1) == 0

-- TODO: 2.4, 2.5, 2.6, 2.7

-- 2.4
intersection :: (Integer -> Bool) -> (Integer -> Bool) -> (Integer -> Bool)
intersection = \s1 -> \s2 -> (\x -> mem s1 x && mem s2 x)

-- 2.5
intersectionV2 :: (Integer -> Bool) -> (Integer -> Bool) -> Integer -> Bool
intersectionV2 s1 s2 x = mem s1 x && mem s2 x

-- 2.6
toSet :: [Integer] -> (Integer -> Bool)
toSet list = \x -> not (null (filter (== x) list))

-- 2.7
capList :: [Integer -> Bool] -> Integer -> Bool
capList setList x = foldr (&&) True (map (\el -> mem el x) setList)