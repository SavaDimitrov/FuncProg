main :: IO()
main = do
 --print (members (\x -> x * x) [(1, 1), (1, 2), (2, 4)])
 --print (images (\x -> x * x) (\x -> x + 2) [(2, 2), (1, 2), (3, 7)])
 --print (isIterator (+2) [1, 3, 5])
 --print (elementsOf [1, 2, 3] 3)
 print (maximize [(\x -> x ** 3), (\x -> x + 1)] (-2))
 
 
{-
compose :: [(a -> a)] -> a -> a
compose fs x = foldr (.) id fs x
-}

{-First-}
members :: (Double -> Double) -> [(Double, Double)] -> [(Double, Double)]
members f [] = []
members f xys = [(x,y) | (x,y) <- xys, f x == y]

{-Second-}
images :: (Double -> Double) -> (Double -> Double) -> [(Double, Double)] -> [(Double, Double)]
images f g [] = []
images f g xys = [(x,y) | (x,y)<-xys, f x == g y]

{-Third-}
isIterator :: (Eq a) => (a -> a) -> [a] -> Bool
isIterator f xs = and[f x == y | (x,y) <- zip xs (drop 1 xs)]

{-Fourth-}
elementsOf :: [Integer] -> (Integer -> Integer)
elementsOf xs = f
 where 
 f x = if(elem x xs) then x else 0

{-Fifth-}
maximize :: [(Double -> Double)] -> (Double -> Double)
maximize fs x = foldr1 (\ x y -> if (abs x > abs y) then x else y) [f x | f <- fs]

--maximize :: [(Double -> Double)] -> (Double -> Double)
--maximize fs x = snd(maximum[(abs (f x), f x) | f <- fs])