import Text.Parsec
import Data.List

inputFile :: Parsec String () [[String]]
inputFile = sepEndBy groupAnswers (string "\n\n")

groupAnswers :: Parsec String () [String]
groupAnswers = sepEndBy answers endOfGroup

answers :: Parsec String () String
answers = try (many1 (noneOf "\n"))

endOfGroup :: Parsec String () ()
endOfGroup = try (do {oneOf "\n"; notFollowedBy (char '\n')})

groupChars :: [String] -> [String]
groupChars s = group (sort (concat s))

common :: [String] -> String
common xs = map head (filter (hasLength (length xs)) (groupChars xs))

hasLength :: Int -> [a] -> Bool
hasLength n xs = length xs == n

part2 :: [[String]] -> Int
part2 input = sum (map (length . common) input)

main = do
  contents <- readFile "input.txt"
  let input = parse inputFile "input.txt" contents
  putStr "Part 2: "
  case input of
    Right v -> print (part2 v)
    Left err -> print err
