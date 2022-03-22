module MyMath
    where

import Data.List

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

silnia :: Integer -> Integer
silnia n
    | n == 1 = 1
    | otherwise = n * silnia (n-1)

silnia' :: Integer -> Integer
silnia' n = silniarec n 1
    where
        silniarec 1 prod = prod
        silniarec k prod = silniarec (k-1) $! (prod * k)

powers :: Num t => t -> [t]
powers x = powersloop 1 x
    where powersloop cum x = let next = cum * x in cum:powersloop next x

powers' :: Num t => t -> [t]
powers' x = [x^i | i <- [0,1..]]

powers'' :: Num t => t -> [t]
powers'' x = 1:[x*i | i <- powers'' x]

powers''' :: Num a => a -> [a]
powers''' x = 1:map (*x) (powers''' x)

powersGreater :: (Ord a, Num a) => a -> a -> [a]
powersGreater lb = filter (>lb) . powers'''

polynomialValue :: Num a => [a] -> a -> a
polynomialValue coef x = sum $ zipWith (*) coef $ powers x

sumujListe :: (Num t) => [t] -> t
sumujListe = foldl (+) 0

sumujListeStrict :: (Num t) => [t] -> t
sumujListeStrict = foldl' (+) 0

pp :: [t] -> [t] -> [t]
pp = flip $ foldr (:)

cat :: [[t]] -> [t]
cat = foldl pp []

leibPi :: (Fractional a) => Int -> [a]
leibPi n = take n $ map (\i -> (-1)^i/(1 + 2 * fromInteger i)) [0,1..]

leibPi' :: (Fractional a) => Int -> [a]
leibPi' n = take n [(-1)^i/(1 + 2 * fromInteger i) | i <- [0,1..]]