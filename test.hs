sum2 :: Int -> Int -> Int
sum2 a b = a + b

fizzbuzz :: Int -> String 
fizzbuzz n 
    | mod n 15 == 0 = "FizzBuzz"
    | mod n 3  == 0 = "Fizz"
    | mod n 5  == 0 = "Buzz"
    | otherwise     = show n

fizzbuzzTo n = map fizzbuzz [1..n]

main = print (fizzbuzzTo 30) 