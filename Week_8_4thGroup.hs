--Practice with 4th group
import Prelude hiding (sum, product, length, any, all, minimum, maximum,
    reverse, concat)
    
main :: IO()
main = do
 print (concat [[1,2,3], [(-1), 2,3]], [0])

{-
length :: Num b => [a] -> b
length xs = foldr (\_ L -> L + 1) 0 xs

(or)
any f xs = foldr (||) False (map f xs)

(and)
all f xs = foldr (&&) True (map f xs)


minimum :: Ord a => [a] -> a
minimum [] = error "No items in the list."
minimum (x:xs) = foldr min x xs
 
maximum [] = error "No items in the list."
maximum (x:xs) = foldr max x xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ (concat xss)

-}

concat :: [[a]] -> [a]
concat [] = []
concat xss = foldr (++) xs xss

reverse :: [a] -> [a]
reverse [] = []
reverse xs = foldl (\ acc x -> (x:acc)) [] xs