main :: IO()
main = do
 print (countMinimum [1,2,3,1,4])

{-1-}
intersectSets :: Eq t => [t] -> [t] -> [t]
intersectSets xs ys = [ x | x <- xs, x `elem` ys]

{-2-}
checkPrime :: Integer -> Integer -> Bool
checkPrime n d
 |d == n = True
 |mod n d == 0 = False
 |otherwise = checkPrime n (d+1)

primeFactor :: Integer -> [Integer]
primeFactor n = [d | d <- [2..n], mod n d == 0, (checkPrime d 2)]

{-3-}
{-zipElem :: [Integer] -> [(Integer,Integer)]
zipElem xs = zip xs [2.. ]-}

filterPrimePosition :: [Integer] -> [Integer]
filterPrimePosition xs = [fst (x , y) | (x , y) <- (zip xs [2.. ]), (checkPrime (snd (x , y)) 2)]

{-4-}
findIndices :: Integer -> [Integer] -> [Integer]
findIndices _ [] = []
findIndices num xs = [snd (x , y) | (x , y) <- (zip xs [0.. ]), (num == fst (x , y))]

{-5-}
countMinimum :: [Integer] -> Int
countMinimum xs = length [x | x <- xs, x == minx]
 where minx = minimum xs