{-# LANGUAGE OverloadedStrings #-}


import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import qualified MDict as M


main :: IO ()
main = do
  args <- getArgs
  case args of
    [dictPath, word] -> 
      M.withMDict dictPath $ \dict -> do
        result <- M.lookupWord dict word
        case result of
          Left err -> putStrLn $ "Error: " ++ err
          Right txt -> do
            putStrLn "Definition:"
            TIO.putStrLn txt
    _ -> putStrLn "Usage: my_program <dictionary.mdx> <word>"
