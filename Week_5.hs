main :: IO()
main = do
 print (removeAllOcc 1 [1,5,1,6,1,7])

{-1-}
sumIntList :: [Integer] -> Integer
sumIntList [] = 0
sumIntList (x:xs) = x + sumIntList xs
{-2-}
sumDoubleList :: [Double] -> Double
sumDoubleList [] = 0
sumDoubleList (x:xs) = x + sumDoubleList xs
{-3-}
sumNumList :: Num t => [t] -> t
sumNumList [] = 0
sumNumList (x:xs) = x + sumNumList xs
{-4-}
countList :: [t] -> Integer
countList [] = 0
countList (x:xs) = 1 + countList xs
{-5-}
memberOf :: Eq t => t -> [t] -> Bool
memberOf _ [] = False
memberOf num (x:xs) = if(x==num) then True else memberOf num xs
{-6-}
removeFirstOcc :: Eq t => t -> [t] -> [t]
removeFirstOcc _ [] = []
removeFirstOcc x (y:ys) = if(y==x) then ys else y : removeFirstOcc x ys
{-7-}
elemAtIndex :: Integer -> [t] -> t
elemAtIndex 0 (x:xs) = x
elemAtIndex i (x:xs) = elemAtIndex (i-1) xs
{-8-}
removeAllOcc :: Eq t => t -> [t] -> [t]
removeAllOcc _ (x:xs) = xs
removeAllOcc num (x:xs) = if(x==num) then (removeAllOcc num xs) else x : (removeAllOcc num xs)