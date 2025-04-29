module IO.Todo where

import           System.Environment
import           System.Directory
import           System.IO
import           Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add), ("view", view), ("remove", remove), ("bump", bump)]

main = do
  (command : args) <- getArgs
  progName         <- getProgName
  mapM putStrLn args
  putStrLn progName
  let (Just action) = lookup command dispatch
  action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks =
        zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  handle                 <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents               <- hGetContents handle
  let number       = read numberString
      todoTasks    = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName


bump :: [String] -> IO ()
bump [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks    = lines contents
      task         = todoTasks !! (read numberString)
      newTodoItems = task : delete task todoTasks
  removeFile fileName
  writeFile fileName $ unlines newTodoItems
