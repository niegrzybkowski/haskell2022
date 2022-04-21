module Main where

import System.Environment ( getArgs )

singleCat :: [String] -> IO ()
singleCat [] = return ()
singleCat (lhead:ltail) = 
    readFile lhead >>= 
    putStr >>
    singleCat ltail

main :: IO ()
main = getArgs >>= fmap (const ()) . mapM (\file -> readFile file >>= putStr)