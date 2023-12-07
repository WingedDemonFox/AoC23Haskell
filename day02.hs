import Data.Char (isDigit)
testInput1 = ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"];

day2part1 :: [String] -> Int
day2part1 s = sum (validGame s)

validGame :: [String] -> [Int] 
validGame (s:xs) = [extractGameNrAndCheck s] ++ validGame xs
validGame [] = []


extractGameNrAndCheck :: String -> Int
extractGameNrAndCheck ('G':'a':'m':'e':_:d1:':':xs) = if all (==True) (map hasValidAmount (splitForUsage xs))  then (read [d1] :: Int) else 0
extractGameNrAndCheck ('G':'a':'m':'e':_:d1:d2:':':xs) = if all (==True) (map hasValidAmount (splitForUsage xs))  then (read (d1:[d2]) :: Int) else 0
extractGameNrAndCheck ('G':'a':'m':'e':_:d1:d2:d3:':':xs) = if all (==True) (map hasValidAmount (splitForUsage xs))  then (read (d1:d2:[d3]) :: Int) else 0
extractGameNrAndCheck [] = 0
extractGameNrAndCheck (x:xs) = 0

toSingleArray :: [[a]] -> [a]
toSingleArray = foldr (++) []

splitForUsage s = toSingleArray (map splitRounds (splitSubGames s))

splitSubGames :: String -> [String]
splitSubGames = wordsWhen (==';')

splitRounds :: String -> [String]
splitRounds = wordsWhen (==',')

hasValidAmount :: String -> Bool
hasValidAmount (_:d1:d2:_:'r':'e':'d':xs) = (read (filter isDigit (d1:[d2])) :: Int)  <= 12
hasValidAmount (_:d1:d2:_:'g':'r':'e':'e':'n':xs) = (read (filter isDigit (d1:[d2])) :: Int) <= 13
hasValidAmount (_:d1:d2:_:'b':'l':'u':'e':xs) = (read (filter isDigit (d1:[d2])) :: Int) <= 14
hasValidAmount (d1:d2:_:'r':'e':'d':xs) = (read (filter isDigit (d1:[d2])) :: Int)  <= 12
hasValidAmount (d1:d2:_:'g':'r':'e':'e':'n':xs) = (read (filter isDigit (d1:[d2])) :: Int) <= 13
hasValidAmount (d1:d2:_:'b':'l':'u':'e':xs) = (read (filter isDigit (d1:[d2])) :: Int) <= 14
hasValidAmount s = True

-- thank you https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

day2part2 s = sum  (part2 s )

part2 :: [String] -> [Int] 
part2 (s:xs) = [extractGameNr s] ++ part2 xs
part2 [] = []

extractGameNr :: String -> Int
extractGameNr ('G':'a':'m':'e':_:d1:':':xs) = maximum (map getBlueAmount (splitForUsage xs)) * maximum (map getRedAmount (splitForUsage xs)) * maximum (map getGreenAmount (splitForUsage xs))
extractGameNr ('G':'a':'m':'e':_:d1:d2:':':xs) = maximum (map getBlueAmount (splitForUsage xs)) * maximum (map getRedAmount (splitForUsage xs)) * maximum (map getGreenAmount (splitForUsage xs))
extractGameNr ('G':'a':'m':'e':_:d1:d2:d3:':':xs) = maximum (map getBlueAmount (splitForUsage xs)) * maximum (map getRedAmount (splitForUsage xs)) * maximum (map getGreenAmount (splitForUsage xs))
extractGameNr [] = 0
extractGameNr (x:xs) = 0


getBlueAmount :: String -> Int
getBlueAmount (d1:d2:_:'b':'l':'u':'e':xs) = read (filter isDigit (d1:[d2])) :: Int
getBlueAmount (_:d1:d2:_:'b':'l':'u':'e':xs) = read (filter isDigit (d1:[d2])) :: Int
getBlueAmount (d1:_:'b':'l':'u':'e':xs) = read (filter isDigit [d1]) :: Int
getBlueAmount (x:xs) = 0

getRedAmount (_:d1:d2:_:'r':'e':'d':xs) = read (filter isDigit (d1:[d2])) :: Int
getRedAmount (d1:d2:_:'r':'e':'d':xs) = read (filter isDigit (d1:[d2])) :: Int
getRedAmount (d1:_:'r':'e':'d':xs) = read (filter isDigit [d1]) :: Int
getRedAmount (x:xs) = 0

getGreenAmount (_:d1:d2:_:'g':'r':'e':'e':'n':xs) = read (filter isDigit (d1:[d2])) :: Int
getGreenAmount (d1:d2:_:'g':'r':'e':'e':'n':xs) = read (filter isDigit (d1:[d2])) :: Int
getGreenAmount (d1:_:'g':'r':'e':'e':'n':xs) = read (filter isDigit [d1]) :: Int
getGreenAmount (x:xs) = 0



realInput = [""]
