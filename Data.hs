main :: IO()
main = do
 print (someFunc t3)


data List = EmptyList | Cons Int  List

ys :: List
ys = Cons 1 (Cons 2 (Cons 3 EmptyList))

listLength :: List -> Int
listLength EmptyList = 0
listLength (Cons _ xs) = 1 + (listLength xs)
--xs e bez 1viq si element, _ predstavq 1q element

toList :: List -> [Int]
toList EmptyList = []
toList (Cons x xs) = x : (toList xs)

data BTree = EmptyBT | Node Int BTree BTree

t1 :: BTree
t1 = EmptyBT

t2 :: BTree
t2 = Node 1 EmptyBT EmptyBT

t3 :: BTree
t3 = Node 1 (Node 2 EmptyBT EmptyBT) (Node 3 (Node 4 EmptyBT EmptyBT) EmptyBT)

count :: BTree -> Int
count EmptyBT = 0
count (Node _ lt rt) = 1 + (count lt) + (count rt)

sumT :: BTree -> Int
sumT EmptyBT = 0
sumT (Node x lt rt) = x + (sumT lt) + (sumT rt)

someFunc :: BTree -> [Int]
someFunc EmptyBT = []
someFunc (Node x lt rt) = [x] ++ (someFunc lt) ++ (someFunc rt)

