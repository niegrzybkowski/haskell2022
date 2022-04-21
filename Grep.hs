module Grep where

import System.Environment ( getArgs )
import Data.List (isPrefixOf)
import System.IO.Error (isEOFError, isPermissionError, isDoesNotExistError)
import System.IO ( stderr, hPutStrLn, withFile, IOMode (ReadMode), Handle, hGetContents, hGetLine )
import Control.Exception ( handle )
import System.Exit (exitWith, ExitCode (ExitFailure), exitSuccess)
import GHC.IO.Exception (IOException(IOError), IOErrorType (EOF))

mainLoop :: String -> IO ()
mainLoop prefix = getLine >>=
    (\input ->
    (if prefix `isPrefixOf` input then putStrLn input else pure () ) >>
    mainLoop prefix)

mainLoop' :: String -> IO ()
mainLoop' prefix =
    sequence_ grepLoopList
    where
        grepLoopList = handle handler (getLine >>= grepLine prefix):grepLoopList

        handler :: IOError -> IO ()
        handler e
            | isEOFError e = exitSuccess
            | otherwise    = hPutStrLn stderr ("** Exception:" ++ show e) >>
                exitWith (ExitFailure 1)

grepLine :: String -> String -> IO ()
grepLine prefix input = if prefix `isPrefixOf` input then putStrLn input else pure ()

grepHandle  :: String -> Handle -> IO ()
grepHandle prefix fileHandle = sequence_ $ repeat $ hGetLine fileHandle >>= grepLine prefix 


grepAll :: String -> [String] -> IO ()
grepAll prefix = mapM_ (grepOne prefix)

grepOne :: String -> String -> IO ()
grepOne prefix filename = handle handler $
        withFile filename ReadMode $
        grepHandle prefix
    where
        handler :: IOError -> IO ()
        handler e
            | isEOFError e = pure ()
            | isPermissionError e = hPutStrLn stderr 
                ("Permission denied: " ++ show e)
            | isDoesNotExistError e = hPutStrLn stderr 
                ("File does not exist: " ++ show e)
            | otherwise    = hPutStrLn stderr 
                ("** Exception ** " ++ show e) >>
                exitWith (ExitFailure 1)

main :: IO ()
main = do
    prefix:files <- getArgs;
    grepAll prefix files;

