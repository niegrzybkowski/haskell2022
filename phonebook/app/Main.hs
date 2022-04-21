module Main where

import Lib
import Data.List
-- import qualified aby uniknąć konfliktów
-- nazw funkcji w List i Map
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Environment
import System.IO.Error
import Data.Maybe (isJust)


-- aliasy typów do poprawy czytelności
type Phone = String
type Name = String
-- i skrócenia zapisu
type StIO a = StateT (M.Map Name Phone) IO a


-- funkcje pomocnicze operujące na zapisanych numerach
storePhone :: Name -> Phone -> StIO ()
storePhone name phone = modify' (M.insert name phone)

findByPrefix :: Name -> StIO [(Name, Phone)]
findByPrefix prefix =
  fmap (filter (\(n,_) -> prefix `isPrefixOf` n) . M.toAscList)  get

ask :: String -> StIO String
ask prompt = lift (putStrLn prompt >> getLine)

storePhoneCommand :: StIO ()
storePhoneCommand = do
  name <- ask "Name?"
  phone <- ask "Phone?"
  storePhone name phone

findPhoneCommand :: StIO ()
findPhoneCommand = ask "Name prefix?" >>= findByPrefix >>= lift . print

findExact :: Name -> StIO (Maybe Phone)
findExact name = M.lookup name <$> get

deleteExact :: Name -> StIO ()
deleteExact name = modify' (M.delete name)

confirmAction :: StIO () -> StIO ()
confirmAction action = confirm >>= (`when` action)
    where
        confirm = do
            response <- ask "Confirm action [y/N]?";
            case response of
                "Y" -> pure True
                "y" -> pure True
                _   -> pure False


deleteCommand :: StIO ()
deleteCommand = do
    nameToDelete <- ask "Exact name?";
    numberToDelete <- findExact nameToDelete;
    case numberToDelete of
        Nothing -> lift $ putStrLn "No such entry";
        Just number -> confirmAction (deleteExact nameToDelete >>
            lift (putStrLn "Entry deleted"));


commands :: M.Map String (StIO Bool)
commands = M.fromList [
  ("add", storePhoneCommand >> return True),
  ("find", findPhoneCommand >> return True),
  ("delete", deleteCommand >> return True),
  ("exit", return False)
 ]

unknownCommand :: StIO Bool
unknownCommand = lift (putStrLn "Unknown command") >> return True

processCommand :: String -> StIO Bool
processCommand cmd = M.findWithDefault unknownCommand cmd commands

readCommand :: StIO Bool
readCommand = lift getLine >>= processCommand

mainLoop :: StIO ()
mainLoop = do
  result <- readCommand
  when result mainLoop

main :: IO ()
main = do
  (file:_) <- getArgs
  initialMap <- catchIOError (do
       m <- read <$!> readFile file
       print m
       return m
    ) (\ex -> if isDoesNotExistError ex then return M.empty else ioError ex)

  finalMap <- execStateT mainLoop initialMap
  writeFile file $ show finalMap