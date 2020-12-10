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

part2 :: InputCase -> Bool
part2 input = let
                  pwd = password input
                  ltr = letter input
                  firstPos = fst (range input)
                  secondPos = snd (range input)
                  firstValid = pwd !! (firstPos-1) == ltr
                  secondValid = pwd !! (secondPos-1) == ltr
                in
                  firstValid /= secondValid

main = do
  contents <- readFile "input.txt"
  let input = Parsec.parse inputParser "input.txt" contents
  putStr "Part 2: "
  case input of
    Right v -> print (length (filter part2 v))
    Left err -> print err
