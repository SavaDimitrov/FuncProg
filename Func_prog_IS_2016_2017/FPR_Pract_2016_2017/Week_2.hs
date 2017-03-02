main :: IO()
main = do
 print (rev_digits 123)


digitsum :: Integer -> Integer
digitsum num
  |num<=9 = num
  |otherwise = digitsum(div num 10) + mod num 10
 
num_int_sum :: Integer -> Integer -> Integer
num_int_sum a b = if (a == b) then b else a + (num_int_sum (a+1) b)

help_func_divs:: Integer -> Integer -> Integer
help_func_divs num d 
 |d > div num 2 = 0
 |mod num d == 0 = d + (help_func_divs num (d+1))
 |otherwise = help_func_divs num (d+1)

num_divs :: Integer -> Integer
num_divs num = (help_func_divs num 2)

help_func_primenum :: Integer -> Integer -> Bool
help_func_primenum num d
 |d == num = True
 |mod num d == 0 = False
 |otherwise = help_func_primenum num (d+1)

isprime :: Integer -> Bool
isprime num = help_func_primenum num 2


helper_rev :: Integer -> Integer -> Integer
helper_rev n res = if (n<10) then (res*10 + n)
                   else helper_rev (div n 10) (res*10 + mod n 10) 
    
rev_digits :: Integer -> Integer
rev_digits n = helper_rev n 0

