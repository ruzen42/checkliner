module Main where

import System.Environment

main :: IO ()
main = do 
  args <- getArgs
  lines <- countLines $ args !! 0
  print lines

countLines :: FilePath -> IO Int
countLines filePath = do
  content <- readFile filePath
  return $ length $ lines content

