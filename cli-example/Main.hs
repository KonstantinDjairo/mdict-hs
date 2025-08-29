{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Monad
import System.FilePath (replaceExtension)
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
        -- Lookup the main query
        result <- lookupWord dict dictFile queryKey
        case result of
            Left err -> putStrLn err
            Right html -> do
                -- Replace all local <img> sources with base64 from .mdd
                htmlWithMedia <- replaceMedia dictFile html
                putStrLn htmlWithMedia
