module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import Words (randomWord)
import Puzzle

main :: IO ()
main = do
  putStrLn "Hangman"
