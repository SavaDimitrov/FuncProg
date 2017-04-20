main :: IO()
main = do
 print (maxOrderedSublist [1,12,5,6,7,8])


maxOrderHelp :: [Int] -> [[Int]]
maxOrderHelp = helper []
 where
  helper :: [Int] -> [Int] -> [[Int]]
  helper axs [] = [axs, []]
  helper axs [x] = [axs ++ [x], []]
  helper axs (x1:x2:xs) =
   if(x1 > x2) then [axs ++ [x1], (x2:xs)]
   else helper (axs ++ [x1]) (x2:xs)

maxOrderedSublist :: [Int] -> [Int]
maxOrderedSublist = helper []
 where 
  helper :: [Int] -> [Int] -> [Int]
  helper res [] = res
  helper res rests =
   helper (if length axs > length res then axs else res) xs
    where [axs, xs] = maxOrderHelp rests

