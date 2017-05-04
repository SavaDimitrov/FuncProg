import Data.Char
main :: IO()
main = do
 print (hex2bin "A2E")

{-First task-}
splitIncreasing :: Ord b => [b] -> b -> [b] -> [[b]]
splitIncreasing [] _ acc = [acc]
splitIncreasing (x:xs) last acc = 
 if(last <= x) 
 then splitIncreasing xs x (acc ++ [x])
 else acc : splitIncreasing xs x [x]

longest :: [[Int]] -> [Int]
longest xss = foldr1 (\xs acc -> if(length xs > length acc) then xs else acc) xss

maxOrderedSublist :: [Int] -> [Int]
maxOrderedSublist (x:xss) = longest (splitIncreasing xss x [])

{-{-Second task-}
hexChar2Int :: Char -> Int
hexChar2Int c
 |c >= '0' && c <= '9' = ord c - ord '0'
 |c >= 'a' && c <= 'f' = 10 + ord c - ord 'a'
 |c >= 'A' && c <= 'F' = 10 + ord c - ord 'A'
 |otherwise = error "Invalid Hex Digit!"

int2Bin :: Int -> [Int]
int2Bin x
 |x < 2 = [x]
 |otherwise = [int2Bin (div x 2)] ++ [mod x 2]

hex2bin :: String -> [Int]
hex2bin str = concat (map (int2Bin.hexChar2Int) str)-}

{-Third task-}
prim :: (Double -> Double) -> Double -> (Double -> Double)
prim f delta = \ x -> (f(x + delta) - f(x - delta)) / 2*delta

{-Fourth task-}
newtonRhapshon :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double
newtonRhapshon f f' e x = 
