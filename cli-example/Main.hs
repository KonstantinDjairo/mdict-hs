{-# LANGUAGE OverloadedStrings #-}
module Main where

import MDict
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Control.Exception (try, SomeException)
import Control.Monad (unless)
import System.Exit (exitSuccess)

-- Highest Priority: Safe dictionary initialization with error handling
loadDictionary :: FilePath -> IO (Either String MDict)
loadDictionary path = do
  result <- try $ withMDict path (return . Right) 
  case result of
    Left (e :: SomeException) -> return $ Left $ "Error loading dictionary: " ++ show e
    Right dict -> return dict

-- Medium Priority: Interactive lookup loop with graceful exit
lookupLoop :: MDict -> IO ()
lookupLoop dict = do
  putStr "Enter word to lookup (or :quit to exit): "
  hSetBuffering stdout NoBuffering
  input <- getLine
  unless (input == ":quit") $ do
    result <- lookupWord dict input
    case result of
      Just definition -> putStrLn $ "\nDefinition:\n" ++ definition ++ "\n"
      Nothing -> putStrLn $ "\n'" ++ input ++ "' not found in dictionary.\n"
    lookupLoop dict
  putStrLn "Exiting..."
  exitSuccess

-- Critical: Command-line argument validation
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    [mdxPath] -> do
      putStrLn $ "Loading dictionary: " ++ mdxPath
      eitherDict <- loadDictionary mdxPath
      case eitherDict of
        Left err -> putStrLn err
        Right dict -> do
          putStrLn "Dictionary loaded successfully!\n"
          lookupLoop dict
    _ -> putStrLn $ unlines
      [ "Invalid arguments!"
      , "Usage: mdict-lookup <path-to-mdict.mdx>"
      , "Example: mdict-lookup /dictionaries/cantonese.mdx"
      ]
