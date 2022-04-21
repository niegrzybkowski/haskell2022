module Monadyy where
import Data.Char ( toUpper, isLower )
import Control.Monad (liftM2)

capitalize :: [Char] -> Maybe [Char]
capitalize s
    | all isLower s = Just $ map toUpper s
    | otherwise = Nothing

unaryNumber :: Int -> Maybe String
unaryNumber i
    | i >= 0 = Just $ replicate i '0'
    | otherwise = Nothing

interleave :: String -> String -> String 
interleave (ah:at) (bh:bt) = ah:bh:interleave at bt
interleave [] _ = ""
interleave _ [] = ""

capitalizeM :: String -> Maybe String 
capitalizeM "" = Just ""
capitalizeM (sh:st) = 
    (if isLower sh then Just $ toUpper sh else Nothing) >>= 
    (\c -> 
    capitalizeM st >>= 
    (\rest ->
    Just $ c:rest))

unaryNumberM :: Int -> Maybe String
unaryNumberM i
    | i > 0 = do 
        rest <- unaryNumberM (i-1)
        Just $ "0" ++ rest
    | i == 0 = Just ""
    | otherwise = Nothing

interleaveM :: String -> String -> String 
interleaveM l1 l2 = do
    l1h <- l1
    l2h <- l2
    l1h:l2h:' ':interleaveM (drop 1 l1) (drop 1 l2)


-- capitalizeM :: String -> Maybe String
-- capitalizeM (sh:st) = capitalizeCharM sh >>= 
--     (\x -> capitalizeM)