module Zad3 where
import Control.Monad (foldM)
import Text.Read (readMaybe)

data Table = Table Double Double deriving Show

makeTable :: Double -> Double -> Maybe Table
makeTable x y
  | x >=0 && x <= 1 && y >=0 && y <=1 = Just $ Table x y
  | otherwise = Nothing

pushBall :: Double -> Double -> Table -> Maybe Table
pushBall x y (Table cx cy) = makeTable (x+cx) (y+cy)

averageTable :: Table -> Table -> Table
averageTable (Table x1 y1) (Table x2 y2) = Table ((x1+x2)/2) ((y1+y2)/2)

pushSequence :: Table -> [(Double, Double)] -> Maybe Table
pushSequence = foldM (\t (x, y) -> pushBall x y t)

maybeAverage :: Maybe Table -> Maybe Table -> Maybe Table
maybeAverage f1 f2 = averageTable <$> f1 <*> f2

playGame :: [String] -> Maybe Table
playGame s =  maybeMoves s >>= pushSequence (Table 0.5 0.5)
    where
        maybeMoves :: [String] -> Maybe [(Double, Double)]
        maybeMoves s = mapM (readMaybe :: String -> Maybe (Double, Double)) s

averageSequences :: [(Double, Double)] -> [String] -> Maybe Table
averageSequences parsed unparsed = maybeAverage 
    (pushSequence (Table 0.5 0.5) parsed)
    (playGame unparsed)