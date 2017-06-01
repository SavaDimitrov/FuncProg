main :: IO()
main = do
 print (f4 bt1 1 2)

data BTree = Empty | Node Int BTree BTree
 deriving Show --print bt is possible
 
bt :: BTree
bt = Node 5 (Node 1 Empty Empty) Empty

bt1 :: BTree
bt1 = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 5 Empty Empty)

--data Btree a = Empty BT | Node a Btree Btree
--bt1 :: Btree Int

f1 :: BTree -> Int -> Bool
f1 Empty _ = False
--f1 (Node a Empty Empty) b = b == a
f1 (Node a ltree rtree) b = a == b || (f1 ltree b) || (f1 rtree b)

f2 :: BTree -> Int -> [Int]
f2 Empty _ = []
f2 (Node a ltree rtree) 0 = [a]
f2 (Node a ltree rtree) n = f2 ltree (n - 1) ++ f2 rtree (n - 1)

f3 :: BTree -> Int -> Int -> Bool
f3 Empty _ _ = False
f3 (Node a ltree rtree) n m = 
 (a == n && (f1 ltree m || f1 rtree m)) 
 || (f3 ltree n m) || (f3 rtree n m)

f4 :: BTree -> Int -> Int -> [Int]
f4 Empty _ _ = []
f4 (Node a ltree rtree) n m 
 |a == n && f1 ltree m = [a] ++ f4 ltree n m
 |otherwise = [a] ++ f4 rtree n m