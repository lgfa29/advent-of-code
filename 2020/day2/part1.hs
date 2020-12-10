import qualified Text.Parsec as Parsec

data InputCase = InputCase { range :: (Int, Int)
                           , letter :: Char
                           , password :: String
                           } deriving (Show)

rangeParser :: Parsec.Parsec String () (Int,Int)
rangeParser = do
  lowerBound <- Parsec.many1 Parsec.digit
  Parsec.char '-'
  upperBound <- Parsec.many1 Parsec.digit
  return (read lowerBound, read upperBound)

inputCaseSeparator :: Parsec.Parsec String () ()
inputCaseSeparator = do
  Parsec.many (Parsec.oneOf " :")
  return ()

inputCaseParser :: Parsec.Parsec String () InputCase
inputCaseParser = do
  range <- rangeParser
  inputCaseSeparator
  char <- Parsec.anyChar
  inputCaseSeparator
  password <- Parsec.many1 Parsec.letter
  return (InputCase range char password)

inputParser :: Parsec.Parsec String () [InputCase]
inputParser = Parsec.many $ do
  inputCase <- inputCaseParser
  Parsec.char '\n'
  return inputCase

upperBoundValid :: String -> Char -> Int -> Bool
upperBoundValid [] _ i = i >= 0
upperBoundValid (firstChar:s) c i = if firstChar == c
                                    then upperBoundValid s c (i - 1)
                                    else upperBoundValid s c i

lowerBoundValid :: String -> Char -> Int -> Bool
lowerBoundValid [] _ i = i <= 0
lowerBoundValid (firstChar:s) c i = if firstChar == c
                                    then lowerBoundValid s c (i - 1)
                                    else lowerBoundValid s c i

part1 :: InputCase -> Bool
part1 input = let
                  pwd = password input
                  ltr = letter input
                  lowerBound = fst (range input)
                  upperBound = snd (range input)
              in
                lowerBoundValid pwd ltr lowerBound
                && upperBoundValid pwd ltr upperBound

main = do
  contents <- readFile "input.txt"
  let input = Parsec.parse inputParser "input.txt" contents
  putStr "Part 1: "
  case input of
    Right v -> print (length (filter part1 v))
    Left err -> print err
