module Puzzle where

import Data.List
import System.Exit (exitSuccess)

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ matched guessed) = 
    (intersperse ' ' $
    fmap renderPuzzleChar matched)
    ++ " Guessed so far: " ++ guessed

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
  putStrLn $ "You guessed " ++ [g]
  let result = fillInChar p g
  case (charInWord p g, alreadyGuessed p g) of
    (_, True) -> do
      putStrLn "You've already guessed this letter!"
      return result
    (True, _) -> do
      putStrLn "You've matched a letter!"
      return result
    (False, _) -> do
      putStrLn "Bad guess!"
      return result


gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ guessed) = 
  if (length guessed) > 7 then
    do 
      putStrLn "You guessed too many times! Game over!"
      putStrLn $ "The word was:" ++ word
      exitSuccess
  else return ()
