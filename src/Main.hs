module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import Words (randomWord)

main :: IO ()
main = do
  putStrLn "Hangman"
