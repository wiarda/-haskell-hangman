module Words where
  
import System.Random (randomRIO)

newtype WordList = 
  WordList [String]
  deriving (Eq, Show)

readDict :: IO WordList
readDict = do
  dict <- readFile "src/dict"
  return $ WordList (lines dict)

filterDict :: WordList -> WordList
filterDict = WordList . filter (\x -> sufficientLength x && noApostrophe x && noBackslash x) . (\(WordList x) -> x)
  where 
    minSize = 5
    sufficientLength = flip (>) minSize . length
    noApostrophe = not . elem '\''
    noBackslash = not . elem '\\'

gameWords :: IO WordList
gameWords = do
  words <- readDict
  return $ filterDict words

getRandomWord :: WordList -> IO String
getRandomWord (WordList wl) = do
  idx <- randomRIO (0, length wl - 1)
  return (wl !! idx)

randomWord :: IO String
randomWord = gameWords >>= getRandomWord

