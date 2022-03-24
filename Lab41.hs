{-# OPTIONS -Wall #-}
module Lab41 where

type Chance = Double

-- 1
data Gra = Choice Gra Gra Gra
    | Random Chance Gra Gra
    | Loss Double
    deriving Show
--2
instance Eq Gra where
    (Choice x1 x2 x3) == (Choice y1 y2 y3) =
        x1 == y1 && x2 == y2 && x3 == y3
    (Random x gx1 gx2) == (Random y gy1 gy2)
        | x == y = gx1 == gy1 && gx2 == gy2
        | x == (1.0 - y) = gx1 == gy2 && gx2 == gy1
        | otherwise = False
    (Loss x) == (Loss y) = x == y
    _ == _ = False
--3
containsX :: Double -> Gra -> Bool
containsX needle (Choice b1 b2 b3) =
    containsX needle b1 || containsX needle b2 || containsX needle b3
containsX needle (Random _ b1 b2) =
    containsX needle b1 || containsX needle b2
containsX needle (Loss hay) = needle == hay

-- 4
loseMore :: Double -> Gra -> Gra
loseMore amount (Choice b1 b2 b3) =
    Choice (loseMore amount b1) (loseMore amount b2) (loseMore amount b3)
loseMore amount (Random p b1 b2) =
    Random p (loseMore amount b1) (loseMore amount b2)
loseMore amount (Loss current) =
    Loss (current - amount)

-- 5
data Decision = First | Second | Third

playList :: [Decision] -> Gra -> Double
playList (First:lt) (Choice b _ _) =  playList lt b
playList [] (Choice b _ _) = playList [] b
playList (Second:lt) (Choice _ b _) =  playList lt b
playList (Third:lt) (Choice _ _ b) =  playList lt b
playList list (Random ch b1 b2) = playList list b
    where b = if ch >= 0.5 then b1 else b2
playList _ (Loss amount) = amount

-- 6
longDecide :: Gra
longDecide = longDecideStep 1
    where longDecideStep n = Choice (Loss (-n)) (Loss (-1)) (longDecideStep (n+1))

-- testy 
subtree :: Gra
subtree = Choice (Loss (-4)) (Loss (-3)) (Loss (-5))

tree1 :: Gra
tree1 = Choice (Loss (-1)) (Random 0.4 subtree (Loss(-2))) (Loss (-1))

tree2 :: Gra
tree2 = Choice (Loss (-1)) (Random 0.6 (Loss(-2)) subtree) (Loss (-1))

-- *Lab41> tree1 == tree1
-- True
-- *Lab41> tree1 == tree2
-- True
-- *Lab41> tree1 == subtree 
-- False
-- *Lab41> subtree == longDecide 
-- False

-- *Lab41> loseMore 1.0 subtree 
-- Choice (Loss (-5.0)) (Loss (-4.0)) (Loss (-6.0))

-- *Lab41> playList [] subtree
-- -4.0
-- *Lab41> playList [Second] subtree
-- -3.0
