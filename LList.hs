module LList where
import Control.Arrow (Arrow(first))

elem' :: (Eq t) => t ->[t] -> Bool
elem' _ [] = False
elem' el (lh:lt)
    | lh == el  = True
    | otherwise = elem' el lt

elem'' :: (Eq t) => t -> [t] -> Bool
elem'' el = foldr (\x cum -> el == x || cum) False

isDecreasing :: (Ord t) => [t] -> Bool 
isDecreasing [] = True
isDecreasing (lh:lt) = let (res, _) = foldr decrease (True, lh) lt in res
    where decrease cur (cum, last) = (cur < last && cum , cur)
