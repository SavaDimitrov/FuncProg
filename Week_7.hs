import Data.Char
main :: IO()
main = do
 print (splitWords '/' "AB123t2Cr56")

{-Task 1- homework-}
sumNumbersHelp :: [Char] -> Int -> Int -> Int
sumNumbersHelp [] num sum = (num + sum) 
sumNumbersHelp (x:str) num sum
 |x >= '0' && x <= '9' && (head (x:str)) >= '0' && (head (x:str)) <= '9' = sumNumbersHelp str (num*10 + (ord x - ord '0')) sum
 |x >= '0' && x <= '9' && not((head (x:str)) >= '0' && (head (x:str)) <= '9') = sumNumbersHelp str (num + (ord x - ord '0')) sum
 |otherwise = sumNumbersHelp str 0 (sum + num)

sumNumbers :: [Char] -> Int
sumNumbers str = sumNumbersHelp str 0 0

{-1-}
whisper :: [Char] -> [Char]
whisper [] = []
whisper (x:str)
 |x >= 'A' && x <= 'Z' = chr (ord 'a' - ord 'A' + ord x) : whisper str
 |otherwise = x : whisper str

{-2-}
rmSpaces :: [Char] -> [Char]
rmSpaces [] = []
rmSpaces (x:str)
 |x == ' ' = rmSpaces str
 |otherwise = x : rmSpaces str

{-3-}
switchcaps :: [Char] -> [Char]
switchcaps [] = []
switchcaps (x:str)
 |x >= 'A' && x <= 'Z' = chr (ord 'a' - ord 'A' + ord x) : switchcaps str
 |x >= 'a' && x <= 'z' = chr (ord 'A' - ord 'a' + ord x) : switchcaps str
 |otherwise = x : switchcaps str

{-4-}
splitWords :: Char -> [Char] -> [[Char]]
splitWords _ [] = []
splitWords sym (x:y:str) = [ [x] ++ [sym] ++ [y] | x <- str, y<-str]