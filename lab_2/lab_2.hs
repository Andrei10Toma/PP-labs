import Data.Char
import Data.Map

listSize:: [a] -> Integer
listSize [] = 0
listSize (h:t) = 1 + listSize t

concatenateLists:: [[Integer]] -> [Integer]
concatenateLists [] = []
concatenateLists (h:t) = h ++ concatenateLists t

concatenateListsV2:: [[Integer]] -> [Integer]
concatenateListsV2 [] = []
concatenateListsV2 ([]:xs) = concatenateListsV2 xs
concatenateListsV2 ((y:x):xs) = y:concatenateListsV2 (x:xs)

removeDuplicateIntegers:: [Integer] -> Map Integer Bool -> [Integer]
removeDuplicateIntegers [] acc = []
removeDuplicateIntegers (h:t) acc
    | member h acc = removeDuplicateIntegers t acc
    | otherwise = h : removeDuplicateIntegers t (insert h True acc)


upperCaseFirstLetter:: [String] -> [String]
upperCaseFirstLetter [] = []
upperCaseFirstLetter ((x:h):t) = (toUpper x : h) : upperCaseFirstLetter t

upperCaseAllLetters:: [String] -> [String]
upperCaseAllLetters [] = []
upperCaseAllLetters (h:t) =
    upper h : upperCaseAllLetters t
        where
            upper:: String -> String
            upper [] = []
            upper (h:t) = toUpper h : upper t

searchPatterns:: String -> String -> String -> Integer
searchPatterns [] [] _ = 1
searchPatterns [] _ _ = 0
searchPatterns (hString:tString) [x] (hAcc:tAcc)
    | hString == x = 1 + searchPatterns (hString:tString) ((hAcc:tAcc) ++ [x]) []
    | otherwise = searchPatterns (hString:tString) (x:hAcc:tAcc) []
searchPatterns (hString:tString) [] (hAcc:tAcc) = 1 + searchPatterns (hString:tString) (hAcc:tAcc) []
searchPatterns (hString:tString) (hPattern:tPattern) acc
    | hString == hPattern = searchPatterns tString tPattern (acc ++ [hPattern])
    | otherwise = searchPatterns tString (acc ++ (hPattern:tPattern)) []

studentsWithM:: String-> [(String, [String])] -> [String]
studentsWithM [] [] = []
studentsWithM _ [] = []
studentsWithM group ((studentGroup, studentList):t)
    | group == studentGroup = extractStudentsWithMFromList studentList
    | otherwise = studentsWithM group t
        where
            extractStudentsWithMFromList:: [String] -> [String]
            extractStudentsWithMFromList [] = []
            extractStudentsWithMFromList (h:t)
                | head h == 'M' = h : extractStudentsWithMFromList t
                | otherwise = extractStudentsWithMFromList t

listToPair:: [String] -> [String] -> [(String, String)]
listToPair [] [] = []
listToPair firstList secondList
    | length firstList == length secondList = (head firstList, head secondList) : listToPair (tail firstList) (tail secondList)
    | otherwise = []

combineLists:: [String] -> [String] -> [String]
combineLists [] [] = []
combineLists firstList secondList
    | length firstList == length secondList = ((head(head firstList)) : (head secondList)) : combineLists (tail firstList) (tail secondList)

filterStudents:: [(String, Integer)] -> [([String], Integer)]
filterStudents [] = []
filterStudents ((name, grade):t)
    | length(words name) >= 3 && grade >= 5 = (words name, grade + 1) : filterStudents t
    | length(words name) < 3 && grade >= 5 = (words name, grade) : filterStudents t
    | otherwise = filterStudents t 