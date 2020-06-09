module Words where

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
