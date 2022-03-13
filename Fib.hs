{-# OPTIONS -Wall #-}
module Fib
    where

fibNaive :: Integer -> Integer
fibNaive n 
    | n == 1 || n == 0 = 1
    | otherwise = fibNaive (n - 1) + fibNaive (n - 2)

fibSensible :: Integer -> Integer
fibSensible n 
    | n == 1 || n == 0 = 1
    | otherwise = fibLoop n 1 0
    where
        fibLoop :: Integer -> Integer -> Integer -> Integer 
        fibLoop n_0 n_1 n_2 
            | n_0 <= 1 = n_1 + n_2
            | otherwise = fibLoop (n_0-1) (n_1 + n_2)  n_1

fibSensible' :: Integer -> Integer
fibSensible' n 
    | n == 1 || n == 0 = 1
    | otherwise = fibLoop' n 1 0
        where
            fibLoop' :: Integer -> Integer -> Integer -> Integer 
            fibLoop' n_0 n_1 n_2 
                | n_0 <= 1 = cum
                | otherwise = cum `seq` fibLoop' (n_0-1) cum n_1
                    where 
                        cum = n_1 + n_2
