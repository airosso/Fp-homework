module Main where

import Control.Monad (when)
import Control.Exception (throwIO)
import Parser (file)
import Runner (execBash)
import System.IO (readFile)
import System.IO.Error (userError)
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty, runParser)

main :: IO ()
main = do
    args <- getArgs
    when (null args) (throwIO $ userError "No arguments supplied. Expected script.")
    input <- readFile (head args)
    case runParser file (head args) input of
        Left e -> putStrLn $ errorBundlePretty e
        Right st -> execBash st (tail args)