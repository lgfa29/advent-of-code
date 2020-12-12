import Text.Parsec
import Data.List

inputFile :: Parsec String () [[String]]
inputFile = sepEndBy groupAnswers (string "\n\n")

groupAnswers :: Parsec String () [String]
groupAnswers = sepEndBy answers endOfGroup

answers :: Parsec String () String
answers = many (noneOf " \n")

endOfGroup :: Parsec String () ()
endOfGroup = try (do {oneOf " \n"; notFollowedBy (char '\n')})

groupChars :: [String] -> [String]
groupChars s = group (sort (concat s))

dedup :: [String] -> String
dedup xs = map head (groupChars xs)

part1 :: [[String]] -> Int
part1 input = sum (map (length . dedup) input)

main = do
  contents <- readFile "input.txt"
  let input = parse inputFile "input.txt" contents
  putStr "Part 1: "
  case input of
    Right v -> print (part1 v)
    Left err -> print err
