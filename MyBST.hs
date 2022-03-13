module MyBST where

data Bst a = Nil | Node a (Bst a) (Bst a) deriving Show

contains :: (Ord a) => Bst a -> a -> Bool 
contains Nil _ = False 
contains (Node x left right) needle 
    | x == needle = True 
    | x < needle = contains left needle
    | x > needle = contains right needle

add :: Ord a => Bst a -> a -> (Bst a, Bool)
add Nil val = ((Node val Nil Nil), True)
add n@(Node a left right) val 
    | val == a = (n, False)
    | val > a && not la = (n, False)
    | val > a = (Node a lp right, True)
    | val < a && not ra = (n, False)
    | val < a = (Node a left rp, True)
    where
        (lp, la) = add left val
        (rp, ra) = add right val

addIgn :: Ord a => Bst a -> a -> Bst a
addIgn tree val = fst $ add tree val
