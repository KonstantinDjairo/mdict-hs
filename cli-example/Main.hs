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
        putStrLn "Usage: Main <dictionary_dir> <query_key>"
        exitFailure

    let dictDir  = args !! 0
        queryKey = args !! 1

    -- Process all files (lookupInCollection lists files itself)
    lookupInCollection dictDir queryKey
