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

fibLoop :: Integer -> Integer -> Integer -> Integer 
fibLoop n n_1 n_2 
    | n <= 1 = n_1 + n_2
    | otherwise = fibLoop (n-1) (n_1 + n_2)  n_1