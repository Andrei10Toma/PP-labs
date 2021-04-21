{-
	PP Project 2021

	This is where you will write the implementation for the given tasks.
	You can add other modules aswell.
-}

{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Tasks where

import Dataset
import Data.List
import Text.Printf
import Text.Read
import Data.Maybe
import qualified Data.Map as Map

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

hwGradesTest1 = [
    ["Nume","Lab (1p)","T1 (0.5p)","T2 (1p)","T3 (1.5p)","Ex1 (0.25p)","Ex2 (0.25p)","Ex3 (0.25p)","Ex4 (0.25p)"],
    ["Olivia Noah","0.42","0.49","1","1.05","","10","0",""],
    ["Riley Jackson","0.85","0.5","1","1.5","0.25","0.13","0","0"],
    ["Emma Aiden","","0.5","1","0.9","","","0",""],
    ["Ava Elijah","0.86","0","","","0","0","0","0"]]

hwGradesTest2 = [
    ["Nume","Q1","Q2","Q3","Q4","Q5", "Ex1 (0.25p)", "Q6","Ex. Scris", "Ex2 (0.25p)"],
    ["Olivia Noah","0","0","2","2","2", "2.73", "2","0.93", "0.7"],
    ["Riley Jackson","2","2","2","2","2","", "1","1.2", ""],
    ["Emma Aiden","2","1","2","1","0","2.73", "1","0.8", "0.7"]]


{-
	TASK SET 1
-}
-- Task 1
examSumaryHeader = ["Q", "0", "1", "2"]

extractName :: Row -> Value
extractName = head

calculateGradeInterview :: Row -> Float
calculateGradeInterview =
    (/4).foldr op 0
        where
            op grade acc
                | grade == "" = acc
                | otherwise = acc + read grade :: Float

calculateGrade :: Row -> Value
calculateGrade row = printf "%.2f" ( calculateGradeInterview (init row) + read (last row) :: Float)

compute_exam_grades :: Table -> Table
compute_exam_grades =
    (["Nume", "Punctaj Exam"]:).foldr op [].drop 1
        where
            op elem acc = (extractName elem:[calculateGrade (tail elem)]):acc

-- Task 2
-- Number of students who have passed the exam:

get_passed_students_num :: Table -> Int
get_passed_students_num = length.filter ((>=2.5).read.last).drop 1.compute_exam_grades

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage grades = (fromIntegral (get_passed_students_num grades) :: Float) / fromIntegral (length grades - 1) :: Float

-- Average exam grade
sumOfTheGrades :: Table -> Float
sumOfTheGrades =
    foldr op 0.drop 1.compute_exam_grades
        where
            op elem acc = acc + read (last elem)

get_exam_avg :: Table -> Float
get_exam_avg grades = sumOfTheGrades grades / (fromIntegral (length grades - 1) :: Float)

-- Number of students who gained at least 1.5p from homework:
sumOfTheHwGrades :: Row -> Float
sumOfTheHwGrades =
    foldr op 0.take 3.drop 2
        where
            op grade acc
                | grade == "" = acc
                | otherwise = acc + read grade

get_passed_hw_num :: Table -> Int
get_passed_hw_num =
    foldr op 0.drop 1
        where
            op row acc
                | sumOfTheHwGrades row >= 1.5 = 1 + acc
                | otherwise = acc

-- Task 3
-- calculates the average grade for question {question}
avgQ :: Int -> Table -> Float
avgQ _ [] = 0
avgQ question table@(row:rows)
    | question /= 0 = avgQ (question - 1) (map tail table)
    | otherwise = foldr (op.head) 0 rows / fromIntegral (length (map head rows)) :: Float
        where
            op grade acc
                | grade == "" = acc
                | otherwise = acc + read grade :: Float

-- extract question names from the table
extractQs :: Table -> Row
extractQs = take 6.drop 1.head

get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs table = [extractQs table,
    foldr op [] [1..length (extractQs table)]]
        where
            op el acc = printf "%.2f" (avgQ el table) : acc

-- Task 4
-- count how many of a grade are in a question
countGrades :: Table -> Value -> Int -> Int
countGrades [] _ _ = 0
countGrades table@(row:rows) grade question
    | question /= 0 = countGrades (map tail table) grade (question - 1)
    | otherwise = foldr (op.head) 0 rows
        where
            op points acc
                | grade == points = 1 + acc
                | points == "" && grade == "0" = 1 + acc
                | otherwise = acc

get_exam_summary :: Table -> Table
get_exam_summary table = [examSumaryHeader,
    [ "Q1", show (countGrades table "0" 1), show (countGrades table "1" 1), show (countGrades table "2" 1)],
    [ "Q2", show (countGrades table "0" 2), show (countGrades table "1" 2), show (countGrades table "2" 2)],
    [ "Q3", show (countGrades table "0" 3), show (countGrades table "1" 3), show (countGrades table "2" 3)],
    [ "Q4", show (countGrades table "0" 4), show (countGrades table "1" 4), show (countGrades table "2" 4)],
    [ "Q5", show (countGrades table "0" 5), show (countGrades table "1" 5), show (countGrades table "2" 5)],
    [ "Q6", show (countGrades table "0" 6), show (countGrades table "1" 6), show (countGrades table "2" 6)]]

-- Task 5
get_ranking :: Table -> Table
get_ranking =
    (["Nume", "Punctaj Exam"]:).sortBy sortCriteria.drop 1.compute_exam_grades
        where
            sortCriteria first second
                | last first /= last second = compare (read (last first) :: Float) (read (last second) :: Float)
                | last first == last second = compare (head first) (head second)

-- Task 6
get_exam_diff_table :: Table -> Table
get_exam_diff_table =
    (["Nume", "Punctaj interviu", "Punctaj scris", "Diferenta"]:).sortBy sortCriteria.foldr op [].drop 1
        where
            op row acc =
                ([extractName row] ++ [printf "%.2f" $ calculateGradeInterview $ init $ tail row] ++ [printf "%.2f" (read (last row) :: Float)] ++
                [printf "%.2f" $ abs ((calculateGradeInterview $ init $ tail row) - (read (last row) :: Float) :: Float)]):acc
            sortCriteria first second
                | last first /= last second = compare (read (last first) :: Float) (read (last second) :: Float)
                | last first == last second = compare (head first) (head second)

{-
    TASK SET 2
-}

-- splits a string by a character
splitBy :: Char -> String -> [String]
splitBy del = foldr op []
    where
        op c []
            | c == del = ["", ""]
            | otherwise = [[c]]
        op c acc@(y:ys)
            | c == del = "" : acc
            | otherwise = (c:y):ys

-- unifies a list of strings into one string with a delimiter
bindWith :: Char -> [String] -> String
bindWith del = init.foldr op []
    where
        op el [] = el ++ [del]
        op el acc = el ++ [del] ++ acc

read_csv :: CSV -> Table
read_csv = map (splitBy ',').splitBy '\n'

write_csv :: Table -> CSV
write_csv = bindWith '\n'.map (bindWith ',')

-- TASK 1
getColNumber :: String -> Row -> Integer -> Integer
getColNumber _ [] _ = -1
getColNumber colName (x:xs) acc
    | x == colName = acc
    | otherwise = getColNumber colName xs (acc + 1)

as_list :: String -> Table -> [String]
as_list colName table = drop 1 $ map (!! colNumber) table
    where colNumber = fromIntegral $ getColNumber colName (head table) 0

-- TASK 2
tsort :: String -> Table -> Table
tsort colName table = head table:sortBy sortCriteria (drop 1 table)
    where
        sortCriteria first second
            | (first !! colNumber) == (second !! colNumber) = compare (head first) (head second)
            | isNothing (readMaybe (first !! colNumber) :: Maybe Float) && isNothing (readMaybe (second !! colNumber) :: Maybe Float) = compare (first !! colNumber) (second !! colNumber)
            | isJust (readMaybe (first !! colNumber) :: Maybe Float) && isJust (readMaybe (second !! colNumber) :: Maybe Float) = compare (read (first !! colNumber) :: Float) (read (second !! colNumber) :: Float)
            | isJust (readMaybe (first !! colNumber) :: Maybe Float) && isNothing (readMaybe (second !! colNumber) :: Maybe Float) = GT
            | isNothing (readMaybe (first !! colNumber) :: Maybe Float) && isJust (readMaybe (second !! colNumber) :: Maybe Float) = LT
        colNumber = fromIntegral $ getColNumber colName (head table) 0

-- TASK 3
vmap :: (Value -> Value) -> Table -> Table
vmap func = map (map func)

-- TASK 4
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap func newCol = (newCol:).map func.drop 1

get_hw_grade_total :: Row -> Row
get_hw_grade_total row = [head row] ++ [printf "%.2f" $ sum $ map op (drop 2 row)]
    where
        op el
            | el == "" = 0
            | otherwise = read el :: Float

-- TASK 5
vunion :: Table -> Table -> Table
vunion table1 table2
    | head table1 == head table2 = table1 ++ tail table2
    | otherwise = table1

-- TASK 6
fillWithDummies :: Int -> Int -> Table -> Table
fillWithDummies x y = (++ replicate x (replicate y ""))

hunion :: Table -> Table -> Table
hunion table1 table2
    | length table1 == length table2 = zipWith (++) table1 table2
    | length table1 > length table2 = zipWith (++) table1 (fillWithDummies (length table1 - length table2) (length $ head table2) table2)
    | length table1 < length table2 = zipWith (++) (fillWithDummies (length table2 - length table1) (length $ head table1) table1) table2

-- TASK 7
uniqueFields :: Row -> Row -> Row
uniqueFields [] acc = acc
uniqueFields (x:xs) acc
    | x `elem` acc = uniqueFields xs acc
    | otherwise = uniqueFields xs (acc ++ [x])

-- extract the common columns except the key column
-- return a map with the common columns name to the positions in the tables
getCommonColsExceptKey :: String -> Row -> Row ->  Map.Map String Integer
getCommonColsExceptKey key r2 r1 = Map.filter (> 0) $ foldr op Map.empty r1
    where
        op el acc
            | el == key = acc
            | otherwise = Map.insert el (getColNumber el r2 0) acc

-- build the row with the information from joining the tables
buildRow :: Map.Map String Integer -> String -> Row -> Row -> Row -> Row -> Row -> Row
buildRow _ _ r1 [] h1 _ = foldr op []
    where
        op colName acc
            | colNumber1 /= -1 = (r1 !! colNumber1):acc
            | otherwise = "":acc
            where
                colNumber1 = fromIntegral $ getColNumber colName h1 0
buildRow commonColMap key r1 r2 header1 header2 = foldr op []
    where
        op colName acc
            | colName == key = (r1 !! colNumber1):acc
            | Map.member colName commonColMap && not (null (r2 !! fromIntegral (Map.findWithDefault (-1) colName commonColMap))) = (r2 !! fromIntegral (Map.findWithDefault (-1) colName commonColMap)):acc
            | colNumber1 /= -1 = (r1 !! colNumber1):acc
            | otherwise = (r2 !! colNumber2):acc
            where
                colNumber1 = fromIntegral $ getColNumber colName header1 0
                colNumber2 = fromIntegral $ getColNumber colName header2 0

tjoin :: String -> Table -> Table -> Table
tjoin fieldName t1 t2 = fields:foldr op [] (drop 1 t1)
    where
        fields = uniqueFields (head t2) (head t1)
        -- 2 maps with the common collumns except the key from the tables
        commonColsExceptKey = getCommonColsExceptKey fieldName (head t2) (head t1)
        -- column number that contains the key from the first table
        colNumber1 = fromIntegral $ getColNumber fieldName (head t1) 0
        -- column number that contains the key from the second table
        colNumber2 = fromIntegral $ getColNumber fieldName (head t2) 0
        op row acc
            | not $ null colFound = buildRow commonColsExceptKey fieldName row (head colFound) (head t1) (head t2) fields:acc
            | otherwise = buildRow commonColsExceptKey fieldName row [] (head t1) (head t2) fields:acc
            where
                -- search the row with the given key from the second table
                colFound = filter ((== row !! colNumber1).(!! colNumber2)) t2

-- TASK 8
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian func newCol t1 t2 = newCol:foldr op [] (tail t1)
    where
        op el acc = foldr op' [] (tail t2) ++ acc
            where
                op' el' acc' = func el el':acc'

-- TASK 9
extractColNumbers :: [String] -> Row -> [Int]
extractColNumbers [] _ = []
extractColNumbers (x:xs) r = colNumber:extractColNumbers xs r
    where
        colNumber = fromIntegral $ getColNumber x r 0

projection :: [String] -> Table -> Table
projection colNames table = foldr op [] table
    where
        colNumbers = extractColNumbers colNames (head table)
        op el acc = foldr op' [] colNumbers:acc
            where
                op' el' acc' = (el !! el'):acc'
