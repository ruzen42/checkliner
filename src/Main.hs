module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import Control.Exception
import System.IO.Error
import Control.Monad (when)
import System.Directory
import System.FilePath

data Options = Options
  { target    :: [FilePath]
  , summarize :: Bool
  }

options :: Parser Options
options = Options
  <$> many (strArgument
      ( metavar "TARGET..."
      <> help "Paths to input files or directories" ))
  <*> switch
      ( long "summarize"
      <> short 's'
      <> help "Print a summary of the line count for each file" )

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "Count the number of lines in the specified files or directories"
  <> header "checkliner - a simple file line counting tool" )

main :: IO ()
main = do
  Options{target, summarize} <- execParser opts
  if null target
    then putStrLn "Error: No files or directories specified"
    else do
      allFiles <- concatMapM getFiles target
      results <- mapM processFile allFiles
      let totalLines = sum $ map snd results
      print totalLines
      when summarize $ do
        putStrLn "Summary:"
        mapM_ (\(path, count) -> putStrLn $ "  " ++ path ++ ": " ++ show count ++ " lines") results
        putStrLn $ "Total: " ++ show totalLines ++ " lines"

processFile :: FilePath -> IO (FilePath, Int)
processFile path = do
  result <- try (TIO.readFile path) :: IO (Either IOError T.Text)
  case result of
    Left _ -> return (path, 0) 
    Right content -> do
      let count = countLines content
      return (path, count)

getFiles :: FilePath -> IO [FilePath]
getFiles path = do
  exists <- doesPathExist path
  if not exists
    then do
      putStrLn $ "Warning: Path does not exist: " ++ path
      return []
    else do
      isDir <- doesDirectoryExist path
      if isDir
        then do
          contents <- listDirectory path
          let fullPaths = map (path </>) contents
          concatMapM getFiles fullPaths
        else return [path]

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

countFileLines :: FilePath -> IO Int
countFileLines path = do
  content <- TIO.readFile path
  return $ countLines content

countLines :: T.Text -> Int
countLines = length . T.lines
