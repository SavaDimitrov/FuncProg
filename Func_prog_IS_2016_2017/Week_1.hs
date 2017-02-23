import Prelude hiding (max)

main :: IO()
main = do
 --print ("123")
 --print (fact 5)
 --print (dev 3)
 --print (max 1 5)
 print (fib 0)
 print (fib 1)
 print (fib 5)
 print (fib 50)
 
 
max :: Int -> Int -> Int
max a b = 
    if (a < b) then a else b

fact :: Int -> Int
fact n = 
 if n < 2 then 1 
 else n * (fact(n - 1))

dev :: Int -> Int
dev s = 
 if s < 1 then 1 
 else (div (fact s) 2)




f2 :: Int -> Int -> Int
f2 a b =
 if ( a > 4) then (if b > 3 then b else a)
 else (if b <= 3 then a else b)









fibHelper :: Integer -> Integer -> Integer -> Integer
fibHelper a b n 
  |(n == 0) = a
  |(n == 1) = b
  |otherwise = fibHelper b (a + b) (n - 1)

fib :: Integer -> Integer
fib n = fibHelper 1 1 n

{-
fib :: Integer -> Integer 
 fib n = if (n <= 1) then 1 else fib (n - 1) + fib (n - 2)
-}