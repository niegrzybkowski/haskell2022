module Main where

import System.Timeout
import Text.Read (readMaybe)
import qualified Data.Maybe
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.State.Strict (StateT, get, modify, execStateT)
import Control.Monad.Trans.Class
import Control.Monad (when)

timedRead :: (Read a) => a -> IO a
timedRead x = timeout 2000000 getLine  >>=
    (\inp -> pure (Data.Maybe.fromMaybe x $ inp >>= readMaybe))

timedAsk :: (Read a) => String -> a -> IO a
timedAsk prompt x = putStrLn prompt >> timedRead x

charByCharPrint :: String -> IO ()
charByCharPrint = mapM_ blinkChar
    where
        blinkChar :: Char -> IO ()
        blinkChar c = do
            putChar c
            threadDelay 500000
            putChar '\b'

data ClubMaterial = Steel | Wood | Ivory | Mud deriving (Read,Show,Eq)

data Hit = Hit {
   material :: ClubMaterial
  ,force :: Double
  ,angle :: Double
               }

calculateDistance :: Hit -> Double
calculateDistance (Hit Mud _ _) = 1.0
calculateDistance (Hit Ivory force _) = force * 0.2
calculateDistance (Hit Wood _ angle) = sin angle+cos angle
calculateDistance (Hit Steel force angle) = (sin angle+cos angle)+force

data GolfGame = GolfGame {
   ballDistance :: Double
  ,hitCount :: Int
  ,hitLimit :: Int
  } deriving Show

newGame :: GolfGame
newGame = GolfGame 16.0 0 12

isGameOver :: GolfGame -> Bool
isGameOver (GolfGame ballDistance hitCount hitLimit) = abs ballDistance < 1e-1 || hitCount >= hitLimit

type GameIO a = StateT GolfGame IO a

getHit :: GameIO Hit
getHit = lift (Hit <$>
    timedAsk "Club" Wood <*>
    timedAsk "Force" 1.0 <*>
    timedAsk "Angle" 1.0)

gameLoop :: Bool -> GameIO ()
gameLoop continue = when continue $ do
    hit <- getHit;
    modify (
        \(GolfGame dist hits limit) ->
        GolfGame (dist - calculateDistance hit) (hits + 1) limit)
    gamestate <- get
    lift $ print gamestate;
    when (isGameOver gamestate) $ lift (charByCharPrint "Game Over" >> print gamestate)
    gameLoop $ not (isGameOver gamestate)


main :: IO ()
main = do
    execStateT (gameLoop True) newGame
    pure ()