module PktowaneTreningowe where

data Mine =  RightTunnel Mine
    | DownTunnel Mine
    | BranchTunnel Mine Mine -- Right, Down
    | Gold
    | Coal
    deriving Eq

instance Show Mine where
    show Gold = "Au"
    show Coal = "C"
    show (RightTunnel r) = "r[" ++ show r ++ "]"
    show (DownTunnel d) = "d(" ++ show d ++ ")"
    show (BranchTunnel r d) = "+(" ++ show d ++ ")[" ++ show r ++ "]"

exampleMine = RightTunnel (
    BranchTunnel (RightTunnel $ BranchTunnel Gold Coal) (DownTunnel Coal))

-- insertD :: Mine -> [Int] -> Mine
-- insertD () l =

infiniteDeep :: Mine
infiniteDeep = DownTunnel infiniteDeep

infiniteRightWithCoal :: Mine
infiniteRightWithCoal = BranchTunnel infiniteRightWithCoal Coal

containsCoal :: Mine -> Bool
containsCoal Coal = True
containsCoal Gold = False
containsCoal (DownTunnel d) = containsCoal d
containsCoal (RightTunnel r) = containsCoal r
containsCoal (BranchTunnel r d) = containsCoal d || containsCoal r

seqA :: [Double]
seqA = 4.0:2.0:0.5:zipWith3 a [4,5..] seqA (drop 1 seqA)
    where
        a n prelast preprelast
            | even n = prelast * preprelast + fromIntegral  n
            | otherwise = (prelast + preprelast) / fromIntegral n

twoOneInterleave :: [a] -> [a] -> [a]
twoOneInterleave (a1:a2:ar) (b1:br) = a1:a2:b1:twoOneInterleave ar br
twoOneInterleave [a1] _ = [a1]
twoOneInterleave _ _ = []

seqNextB :: Int -> Double -> Double -> (Double, Double)
seqNextB n last lastfac = (last + 3^n/fac, fac)
    where
        fac :: Double
        fac = lastfac* fromIntegral n

seqLoopB :: Int -> Int -> Double -> Double -> Double
seqLoopB n_target n last lastfac
    | n_target == n = let (out, _) = seqNextB n last lastfac in out
    | otherwise = cur `seq` cur_fac `seq` seqLoopB n_target (n+1) cur cur_fac
        where
            (cur,cur_fac) = seqNextB n last lastfac

seqB :: Int -> Double
seqB 1 = 3
seqB n = seqLoopB n 2 3 1

listDeformation :: [Int] -> [Int]
listDeformation = foldr deform []
    where
        deform :: Int -> [Int] -> [Int]
        deform i outlist
            | i `mod` 3 == 0 = 0:outlist
            | i `mod` 3 == 1 = i:i:outlist
            | i `mod` 3 == 2 = outlist
            | otherwise = error ""



isProperList :: [Int] -> Bool
isProperList list = foldr (||) False $ 
    zipWith3 proper list (drop 1 list) (drop 2 list)
    where 
        proper :: Int -> Int -> Int -> Bool
        proper a b c  = a < b && a < c && b `mod` c == 0