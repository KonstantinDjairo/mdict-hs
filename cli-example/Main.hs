{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import MDict



main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) $ do
    putStrLn "Usage: Main <dictionary_file> <query_key>"
    exitFailure

  let dictFile = args !! 0
      queryKey = args !! 1

  withMDict dictFile $ \dict -> do
    result <- lookupWord dict dictFile queryKey
    either putStrLn putStrLn result



