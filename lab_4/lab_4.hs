{-
    1 2 3
    4 5 6 -> [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    7 8 9
-}

type Laborator = Integer
type Matrix = [[Integer]]


mstr = "1 2 3\n4 5 6\n7 8 9"
m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

logo = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]
    where l1 ="        ***** **            ***** **    "
          l2 ="     ******  ****        ******  ****   "
          l3 ="    **   *  *  ***      **   *  *  ***  "
          l4 ="   *    *  *    ***    *    *  *    *** "
          l5 ="       *  *      **        *  *      ** "
          l6 ="      ** **      **       ** **      ** "
          l7 ="      ** **      **       ** **      ** "
          l8 ="    **** **      *      **** **      *  "
          l9 ="   * *** **     *      * *** **     *   "
          l10="      ** *******          ** *******    "
          l11="      ** ******           ** ******     "
          l12="      ** **               ** **         "
          l13="      ** **               ** **         "
          l14="      ** **               ** **         "
          l15=" **   ** **          **   ** **         "
          l16="***   *  *          ***   *  *          "
          l17=" ***    *            ***    *           "
          l18="  ******              ******            "
          l19="    ***                 ***             "

mask = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]
    where l1 ="                       *****************"
          l2 ="                       *****************"
          l3 ="                       *****************"
          l4 ="                       *****************"
          l5 ="                       *****************"
          l6 ="                       *****************"
          l7 ="                       *****************"
          l8 ="                       *****************"
          l9 ="                       *****************"
          l10="                       *****************"
          l11="                       *****************"
          l12="                       *****************"
          l13="                       *****************"
          l14="                       *****************"
          l15="                       *****************"
          l16="                       *****************"
          l17="                       *****************"
          l18="                       *****************"
          l19="                       *****************"
testImage = [l1, l2, l3]
    where l1 = " * * * "
          l2 = "       "
          l3 = "   *   "

splitBy :: Char -> String -> [String]
splitBy del
    = foldr op []
    where
        op x [] = [[x]]
        op x (y:ys)
            | x /= del = (x:y):ys
            | otherwise = []:(y:ys)

-- show 5 -> "5"
-- read x :: Integer ("5"->5)

parseM :: String -> Matrix
-- [["1 2 3"], ["4 5 6"], ["7 8 9"]]
-- 
parseM = (map (map read)).map (splitBy ' ').splitBy '\n'

-- [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
toStringOneLine :: [Integer] -> String
toStringOneLine = foldr((++).(++" ")) "\n".(map show)

toString :: Matrix -> String
toString = foldr (++) [].map toStringOneLine

displayMatrix = putStrLn . toString

vprod :: Integer -> Matrix -> Matrix
vprod v = map (map (*v))

hjoin :: Matrix -> Matrix -> Matrix
hjoin = zipWith (++)

vjoin :: Matrix -> Matrix -> Matrix
vjoin = (++)

msum :: Matrix -> Matrix -> Matrix
msum = (zipWith(zipWith (+)))

prodLineCol :: [Integer] -> [Integer] -> Integer
prodLineCol l c = foldr (+) 0 (zipWith (*) l c)

prodLineMat :: Matrix -> [Integer] -> [Integer]
prodLineMat m l = map (prodLineCol l) m

tr :: Matrix -> Matrix
tr ([]:_) = []
tr m = (map head m):(tr (map tail m))

mprod :: Matrix -> Matrix -> Matrix
mprod m1 m2 = map (prodLineMat (tr m2)) m1

displayImg = putStrLn . toStringImg

type Image = [String]

toStringImg :: Image -> String
toStringImg = foldr ((++) . (++"\n")) []

flipH :: Image -> Image
flipH = map reverse

flipV :: Image -> Image
flipV = reverse

rotate90r :: Image -> Image
rotate90r ([]:_) = []
rotate90r img = reverse(map head img):rotate90r (map tail img)

rotate90l :: Image -> Image
rotate90l ([]:_) = []
rotate90l img = map last img:rotate90l(map init img)

invert :: Image -> Image
invert = foldr op []
        where
            op row acc = foldr op' [] row : acc
                where
                    op' pixel acc
                        | pixel == ' ' = '*':acc
                        | otherwise = ' ':acc

stringKeep :: String -> String -> String
stringKeep [] _ = []
stringKeep _ [] = []
stringKeep s1 s2
    | head s1 == '*' = head s2 : stringKeep (tail s1) (tail s2)
    | otherwise = ' ':stringKeep (tail s1) (tail s2)

maskKeep :: Image ->  Image -> Image
maskKeep = zipWith stringKeep

stringDiscard :: String -> String -> String
stringDiscard [] _ = []
stringDiscard _ [] = []
stringDiscard s1 s2
    | head s1 == '*' && head s2 == ' ' = '*' : stringDiscard (tail s1) (tail s2)
    | otherwise = ' ' : stringDiscard (tail s1) (tail s2)

maskDiscard :: Image -> Image -> Image
maskDiscard = zipWith stringDiscard

stringUnion :: String -> String -> String
stringUnion [] _ = []
stringUnion _ [] = []
stringUnion s1 s2
    | head s1 == '*' || head s2 == '*' = '*' : stringUnion (tail s1) (tail s2)
    | otherwise = ' ' : stringUnion (tail s1) (tail s2)

union' :: Image -> Image -> Image
union' = zipWith stringUnion

transformationSequence :: [Image -> Image] -> Image -> Image
transformationSequence seq img =
    foldr op img seq
        where
            op operation acc = operation acc

convertLastNElements :: Integer -> Integer -> [Integer] -> [Integer]
convertLastNElements _ _ [] = []
convertLastNElements m n list@(x:xs)
    | n < m = x:convertLastNElements m (n + 1) xs
    | otherwise = map (*0) list

convertFirstElements :: Integer -> Integer -> [Integer] -> [Integer]
convertFirstElements _ _ [] = []
convertFirstElements m n list@(x:xs)
    | n < m = 0:convertFirstElements m (n + 1) xs
    | otherwise = x:convertFirstElements m (n + 1) xs

aboveDiagonal :: Matrix -> [Integer] -> Matrix
aboveDiagonal mat [] = aboveDiagonal mat [1]
aboveDiagonal [] _ = []
aboveDiagonal mat@(matx:matxs) list@(x:xs) = convertLastNElements x 0 matx:aboveDiagonal matxs ((x+1):list)

belowDiagonal :: Matrix -> [Integer] -> Matrix
belowDiagonal mat [] = belowDiagonal mat [0]
belowDiagonal [] _ = []
belowDiagonal mat@(matx:matxs) list@(x:xs) = convertFirstElements x 0 matx:belowDiagonal matxs ((x+1):list)

aboveDiagonalV2 :: Matrix -> [Int] -> Matrix
aboveDiagonalV2 mat [] = []
aboveDiagonalV2 ([]:_) _ = []
aboveDiagonalV2 mat@(matx:matxs) acc@(x:xs) = (take x matx ++ map (*0) (drop x matx)):aboveDiagonalV2 matxs xs

diagonalMatrix :: Matrix -> [Int] -> Matrix
diagonalMatrix mat [] = []
diagonalMatrix ([]:_) _ = []
diagonalMatrix mat@(matx:matxs) acc@(x:xs) = (map (*0) (take (x-1) (take x matx)) ++ (drop (x-1) (take x matx) ++ map (*0) (drop x matx))):diagonalMatrix matxs xs