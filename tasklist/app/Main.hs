module Main where

import Lib
import Control.Monad.Trans.State.Strict (StateT, execStateT, modify')
import qualified Data.Map.Strict as M
import Control.Monad (when, (<$!>))
import System.IO.Error (isDoesNotExistError, catchIOError)
import Control.Monad.Trans.State.Strict ( execStateT, StateT, get )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import System.Environment ( getArgs )

type Priority = Int
type TaskName = String
type TaskContents = String
type Task = (TaskName, TaskContents)
type TaskQueue = (M.Map Priority [Task])

type StIO = StateT TaskQueue IO

loadQueue :: String -> IO TaskQueue
loadQueue filename = catchIOError
        ((read :: String -> M.Map Priority [Task]) <$!> readFile filename)
        (\ex -> if isDoesNotExistError ex then pure M.empty else ioError ex)

ask :: String -> StIO String
ask query = lift (putStrLn (query ++ "\n") >> getLine)

addTask :: String -> String -> Int -> StIO ()
addTask name content priority = let newTask = (name, content) in
    modify' (\m -> M.insert priority (newTask:M.findWithDefault [] priority m) m)


addTaskCommand :: StIO ()
addTaskCommand = do 
    name <- ask "Enter task name"
    content <- ask "Enter task contents"
    priorityString <- ask "Enter task priority"
    let priority = (read :: String -> Priority) priorityString in
        addTask name content priority

-- listTasksCommand :: StIO ()
-- listTasksCommand = 

commands :: M.Map String (StIO Bool)
commands = M.fromList [
    ("exit", pure False),
    ("add", addTaskCommand >> pure True)
    ]

repl :: StIO Bool
repl = do
    command <- lift getLine;
    pure True

mainLoop :: StIO ()
mainLoop = do
    continue <- repl
    when continue mainLoop

inMemoryTasklist :: IO ()
inMemoryTasklist = do
    putStrLn "Creating new in-memory tasklist"
    execStateT mainLoop (TaskQueue M.empty)
    putStrLn "Goodbye"

onDiskTasklist :: String -> IO ()
onDiskTasklist file = do
    putStrLn ("Loading tasklist from " ++ file)
    loadedQueue <- loadQueue file
    newQueue <- execStateT mainLoop loadedQueue
    putStrLn ("Loading tasklist to " ++ file)
    writeFile file $ show newQueue


main :: IO ()
main = do
    args <- getArgs;
    case args of
        (file:_) -> onDiskTasklist file
        _ -> inMemoryTasklist
