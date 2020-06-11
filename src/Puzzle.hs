module Puzzle where

import Data.List
import Data.Maybe (isJust)
import Control.Monad (forever)
import System.Exit (exitSuccess)

maxGuesses = 7

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ matched guessed) = 
    (intersperse ' ' $
    fmap renderPuzzleChar matched)
    ++ "\nGuessed so far: " ++ (intersperse ' ' guessed)

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just x) = x

initPuzzle :: String -> Puzzle
initPuzzle xs =  
  Puzzle xs (map (const Nothing) xs) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle xs _ _) = flip elem xs

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ xs) = flip elem xs

fillInChar :: Puzzle -> Char -> Puzzle
fillInChar (Puzzle word matchedSoFar guessed) x =
  Puzzle word newMatched (x:guessed)
  where
    zipper x wordChar matchedChar = 
      if wordChar == x then Just x else matchedChar
    newMatched = zipWith (zipper x) word matchedSoFar

parseGuess :: Puzzle -> Char -> IO Puzzle
parseGuess p g = do
  putStrLn ""
  putStrLn $ "You guessed " ++ [g]
  let result = fillInChar p g
  case (charInWord p g, alreadyGuessed p g) of
    (_, True) -> do
      putStrLn "You've already guessed this letter!"
      return result
    (True, _) -> do
      putStrLn "You matched a letter!"
      return result
    (False, _) -> do
      let guessesLeft = show $ maxGuesses - (countMisses result)
      putStrLn $ "Uh oh, try again! You have " ++ guessesLeft ++ " guesses left."
      return result



countMisses :: Puzzle -> Int
countMisses (Puzzle word _ guesses) = 
  length . filter (\x -> not $ elem x word) $ guesses


gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle word _ guessed) = 
  if (countMisses p >= 7) then
    do 
      putStrLn "You guessed incorrectly too many times :("
      putStrLn $ "The word was:" ++ word
      exitSuccess
  else return ()

gameWon :: Puzzle -> IO ()
gameWon (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do 
      putStrLn "You win!"
      exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWon puzzle
  putStrLn $
    show puzzle
  putStrLn "Guess a letter: "
  guess <- getLine

  case guess of 
    [c] -> parseGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single letter."
