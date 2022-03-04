module MyMath
    where

abs' :: (Ord p, Num p) => p -> p
abs' x = if x >= 0 then x else -x


abs'' :: (Ord p, Num p) => p -> p
abs'' x
    | x >= 0    =  x
    | otherwise = -x

clip :: Ord a => a -> a -> a -> a
clip minimum maximum x
    | x > maximum = maximum
    | x < minimum = minimum
    | otherwise   = x

clip' :: Ord c => c -> c -> c -> c
clip' minimum maximum = min maximum . max minimum

xyzzy :: Double -> Integer
xyzzy x = negate (ceiling (abs (sin x)))

xyzzy' :: Double -> Integer
xyzzy' x = negate $ ceiling $ abs $ sin x

xyzzy'' :: Double -> Integer
xyzzy'' x = (negate . ceiling . abs . sin) x

xyzzy''' :: Double -> Integer
xyzzy''' = negate . ceiling . abs . sin

sinTaylorLoop :: Double -> Int -> Int -> Double -> Double -> Int -> Int -> Double
sinTaylorLoop x n currentn total lastx lastfac lastsign
    | n == currentn = total
    | otherwise = sinTaylorLoop x n (currentn + 1)
        (total + fromIntegral lastsign * lastx / fromIntegral lastfac)
        (lastx * x * x)
        (lastfac * (currentn * 2) * (currentn * 2 + 1))
        (-lastsign)

sinTaylor x n 
    | n > 0 = sinTaylorLoop x (n+1) 1 0 x 1 1
    | otherwise = 0/0