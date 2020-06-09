module Words where
  
import System.Random (randomRIO)

type WordList = [String]

readDict :: IO WordList
readDict = do
  dict <- readFile "./words"
  return (lines dict)

filterDict :: WordList -> WordList
filterDict = filter (\x -> sufficientLength x && noApostrophe x && noBackslash x)
  where 
    minSize = 5
    sufficientLength = flip (>) minSize . length
    noApostrophe = not . elem '\''
    noBackslash = not . elem '\\'

gameWords :: IO WordList
gameWords = do
  words <- readDict
  return (filterDict words)

getRandomWord :: WordList -> IO String
getRandomWord wl = do
  idx <- randomRIO (0, length wl - 1)
  return (wl !! idx)

randomWord :: IO String
randomWord = gameWords >>= getRandomWord