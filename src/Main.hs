module Main where

import Data.Char (toLower)
import System.Exit (exitSuccess)
import Words (randomWord)
import Puzzle (runGame, initPuzzle)

main :: IO ()
main = do
  word <- randomWord
  let puzzle = initPuzzle word
  runGame puzzle