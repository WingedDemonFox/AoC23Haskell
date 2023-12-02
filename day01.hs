import Data.Char (isDigit)
testInput = ["1abc2","pqr3stu8vwx","a1b2c3d4e5f","treb7uchet"];


sumCalibration :: [String] -> Int
sumCalibration s = sum (extractSomeNumbers s)

extractSomeNumbers :: [String] -> [Int]
extractSomeNumbers [] = [0]
extractSomeNumbers (x:xs) = foldr (\ x -> (:) (read (extractNumbers x) :: Int)) [0] xs

extractNumbers :: [Char] -> [Char]
extractNumbers s = [head (filter isDigit s)] ++ [last (filter isDigit s)]

sumCalibration2 :: [String] -> Int
sumCalibration2 s = sum (extractAllNumbers s)

extractAllNumbers :: [String] -> [Int]
extractAllNumbers [] = [0]
extractAllNumbers (x:xs) = foldr (\ x -> (:) (read (extractNumbers (replaceTextWithNumbers x)) :: Int)) [0] xs

replaceTextWithNumbers :: String -> String
replaceTextWithNumbers ('o':'n':'e':xs) = "o1e" ++ replaceTextWithNumbers ('e':xs)
replaceTextWithNumbers ('t':'w':'o':xs) = "t2o" ++ replaceTextWithNumbers ('o':xs)
replaceTextWithNumbers ('t':'h':'r':'e':'e':xs) = "t3e" ++ replaceTextWithNumbers ('e':xs)
replaceTextWithNumbers ('f':'o':'u':'r':xs) = "f4r" ++ replaceTextWithNumbers ('r':xs)
replaceTextWithNumbers ('f':'i':'v':'e':xs) = "f5e" ++ replaceTextWithNumbers ('e':xs)
replaceTextWithNumbers ('s':'i':'x':xs) = "s6x" ++ replaceTextWithNumbers ('x':xs)
replaceTextWithNumbers ('s':'e':'v':'e':'n':xs) = "s7n" ++ replaceTextWithNumbers ('n':xs)
replaceTextWithNumbers ('e':'i':'g':'h':'t':xs) = "e8t" ++ replaceTextWithNumbers ('t':xs)
replaceTextWithNumbers ('n':'i':'n':'e':xs) = "n9e" ++ replaceTextWithNumbers ('e':xs)
replaceTextWithNumbers (x:xs) = x: replaceTextWithNumbers xs
replaceTextWithNumbers "" = ""

testInput2 = ["0",
    "two1nine",
    "eightwothree",
    "abcone2threexyz",
    "xtwone3four",
    "4nineeightseven2",
    "zoneight234",
    "7pqrstsixteen"];


realInput = [""]
