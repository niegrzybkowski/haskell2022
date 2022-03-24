{-# OPTIONS -Wall #-}
module Lab42 where

-- zad 2
-- 3.1
theSeqTail :: Int -> Int -> Double -> Double -> Double -> Double 
theSeqTail 1 _ _ _ _ = 3
theSeqTail 2 _ _ _ _ = 1.5
theSeqTail 3 _ _ _ _ = 2
theSeqTail n n_target lastone prelast preprelast  
    | n == n_target = cur
    | otherwise     = cur `seq` theSeqTail (n+1) n_target cur lastone prelast
        where cur = (fromIntegral n + preprelast) / lastone

theSeqN :: Int -> Double 
theSeqN n 
    | n == 1 = 3
    | n == 2 = 1.5
    | n == 3 = 2
    | otherwise = theSeqTail 4 n 2 1.5 3

--3.2
theSeqNext :: Int -> Double -> Double -> Double 
theSeqNext n lastone preprelast = (fromIntegral n + preprelast) / lastone

theSeq :: [Double] 
theSeq = 3.0:1.5:2.0:zipWith3 theSeqNext [4,5..] (drop 2 theSeq) theSeq

-- zad 3
-- 4.1
sortPairs :: (Ord a) =>  [Bool] -> [a] -> [a] -> [a]
sortPairs (lh:lt) (ah:at) (bh:bt) 
    | not lh = sortPairs lt at bt
    | ah < bh = ah:bh:sortPairs lt at bt
    | otherwise = bh:ah:sortPairs lt at bt
sortPairs [] (ah:at) (bh:bt) 
    | ah < bh = ah:bh:sortPairs [] at bt
    | otherwise = bh:ah:sortPairs [] at bt
sortPairs _ [] _ = []
sortPairs _ _ [] = []


-- 4.2
sumProd :: [Int] -> Int 
sumProd list = foldl sumProd' 0 $ zip3 list (drop 1 list) (drop 2 list)
    where 
        sumProd' ::  Int ->(Int, Int, Int)-> Int
        sumProd' cum (pre,cur,nex) = 
            if even (pre + nex) then cum + cur else cum * cur