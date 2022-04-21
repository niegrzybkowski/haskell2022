module Zad2 where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import Control.Monad (replicateM, replicateM_, zipWithM, zipWithM_)
import System.IO (withFile, IOMode (ReadMode), hGetChar)

catFileHead :: Int -> String -> IO ()
catFileHead n filename = withFile filename ReadMode
    (\handle -> replicateM_ n $ hGetChar handle >>= putChar)

main :: IO ()
main = do
    args <- getArgs;
    case mapM readMaybe args of
        Nothing -> putStrLn "Bad invocation. Better luck next time."
        Just numbers -> do
            files <- replicateM (length numbers) getLine
            zipWithM_ catFileHead numbers files
