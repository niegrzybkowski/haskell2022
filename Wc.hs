module Wc where
import System.Environment (getArgs)
import qualified Data.ByteString as BS (readFile, ByteString, length, filter)
import qualified Data.ByteString.Char8 as C
import Control.Exception (handle, IOException)

countLines :: BS.ByteString -> Int
countLines x = 1 + BS.length (C.filter (== '\n') x)

countBytes :: BS.ByteString -> Int
countBytes =  BS.length

processFiles :: [String] -> IO ()
processFiles [] = pure ()
processFiles (lh:lt) = handle handler $ do
    fileData <- BS.readFile lh;
    let lineCount = countLines fileData;
        byteCount = countBytes fileData;
    putStrLn $ lh ++ " - lines: " ++ show lineCount ++ ", bytes: " ++ show byteCount
    processFiles lt
    where
        handler :: IOException -> IO ()
        handler e = putStrLn (lh ++ " - i/o error: " ++ show e) >>
            processFiles lt

processFiles' :: [String] -> IO ()
processFiles' = mapM_ processFile

processFile :: String -> IO ()
processFile filename = handle handler $ do
    fileData <- BS.readFile filename;
    let lineCount = countLines fileData;
        byteCount = countBytes fileData;
    putStrLn $ filename ++ " - lines: " ++ show lineCount ++ ", bytes: " ++ show byteCount
    where
        handler :: IOException -> IO ()
        handler e = putStrLn (filename ++ " - i/o error: " ++ show e)

main :: IO ()
main = do
    files <- getArgs;
    processFiles' files
    