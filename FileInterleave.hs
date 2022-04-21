module FileInterleave where
import System.Environment (getArgs)
import System.IO
import Control.Monad (when)
import System.IO.Error
import System.Exit
import Control.Exception (bracket)

fileInterleave :: Handle -> [Handle] -> IO ()
fileInterleave hOut hsIn = do
    eofs <- mapM hIsEOF hsIn
    if or eofs then
        hFlush hOut
    else do
        interleaved <- mapM hGetChar hsIn
        hPutStr hOut interleaved
        fileInterleave hOut hsIn

printError :: String -> IO ()
printError = hPutStrLn stderr

handleArgs :: [String] -> IO [String]
handleArgs args@(_:_) = pure args
handleArgs _ = printError "" >> exitFailure >> pure []

openWithHandle :: String -> IO Handle 
openWithHandle filename = bracket 
    (openFile filename ReadMode)
    (hClose)
    pure 

main :: IO ()
main = do
    args <- getArgs;
    outputFilename:inputFilenames <- getArgs;
    outputHandle <- openFile outputFilename WriteMode
    inputHandles <- mapM (\name -> openFile name ReadMode) inputFilenames
    fileInterleave outputHandle inputHandles
    hClose outputHandle
    mapM_ hClose inputHandles