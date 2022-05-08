module Main where

import System.Environment
import System.Directory
import System.IO
import Data.List

todoFile = "./todo-hs.txt"

guide :: IO()
guide = do
  putStrLn "COMMANDS:"
  putStrLn "\tlist"
  putStrLn "\tadd <todo message>"
  putStrLn "\tremove <todo id>"
  putStrLn "\tedit <todo id> <todo message>"

hGetLastLine :: Handle -> IO String
hGetLastLine hdl = go "" (negate 1)
  where
  go s i = do
    hSeek hdl SeekFromEnd i
    c <- hGetChar hdl
    if c == '\n'
      then pure s
      else go (c:s) (i-1)

findTodo "0" = do
  handle <- openFile "file.txt" ReadMode
  lastLine <- hGetLastLine handle

findTodo strIdx = do
  putStrLn strIdx

catTodoList = do
  handle <- openFile todoFile ReadMode
  contents <- hGetContents handle
  putStrLn contents
  hClose handle

listCmd = do
  doesTodoFileExist <- doesFileExist todoFile
  if doesTodoFileExist
    then catTodoList
    else writeFile todoFile ""

addCmd content = do
  doesTodoFileExist <- doesFileExist todoFile
  -- let id = if doesTodoFileExist
  --   -- then show $ read (fst (strBreak " | " (findTodo "0"))) :: Integer + 1
  --   then show $ succ $ read $ fst $ strBreak " | " $ findTodo "0"
  --   else '1'
  -- in appendFile todoFile $ id ++ " | " ++ content
  appendFile todoFile $ (if doesTodoFileExist then show $ succ $ read $ fst $ strBreak " | " $ findTodo "0" else '1') ++ " | " ++ content

mapArgs [] = guide
mapArgs (arg:_)
  | arg == "list" = listCmd
  | arg == "add" = addCmd
  -- | arg == "remove" = removeCmd
  -- | arg == "edit" = editCmd
  | otherwise = guide

main :: IO ()
main = do
  args <- getArgs
  mapArgs args
