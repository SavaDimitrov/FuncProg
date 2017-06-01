main :: IO()
main = do
 --print (checkSpecificPeak b1 5)
 --print (putNew b1 7)
 print (sortTree Empty [7,5,1,2,4,6,3])

data BTree = Empty | Node Int BTree BTree
 deriving (Show, Eq)

b1 :: BTree
b1 = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 6 (Node 5 Empty Empty) (Node 7 Empty Empty))

checkSpecificPeak :: BTree -> Int -> Bool
checkSpecificPeak Empty _ = False
checkSpecificPeak (Node a lt rt) v = a == v || (checkSpecificPeak lt v) || (checkSpecificPeak rt v)
 {-|x == v = True
   |x < v = checkSpecificPeak rt v
   |otherwise = checkSpecificPeak lt v-}

putNew :: BTree -> Int -> BTree
putNew Empty v = Node v Empty Empty
putNew bt@(Node a lt rt) v 
 |a == b = bt
 |a > v = (Node a (putNew lt v) rt)
 |otherwise = (Node a lt (putNew rt v))
--if(a > v) then (Node a (putNew lt v) rt) else (Node a lt (putNew rt v))

insertIntoTree :: BTree -> Int -> BTree
insertIntoTree Empty v = Node v Empty Empty
insertIntoTree bt@(Node x lt rt) v
 |x == v = bt
 |x < v = Node x lt (insertIntoTree rt v)
 |otherwise = Node x (insertIntoTree lt v) rt

f3_1 :: [Int] -> BTree
f3_1 xs = foldl insertIntoTree Empty xs

f3_2 :: BTree -> [Int]
f3_2 Empty = []
f3_2 (Node v lt rt) = f3_2 lt ++ [v] ++ f3_2 rt

f3 :: [Int] -> [Int]
f3 = f3_2 . f3_1

deletingNode :: BTree -> Int -> BTree
deletingNode Empty _ = Empty
deletingNode (Node a lt rt) v
 |