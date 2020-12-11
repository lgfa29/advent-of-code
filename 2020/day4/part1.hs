import Text.Parsec
import qualified Data.Map as Map

valid :: Map.Map String String -> Bool
valid m = let
    hasByr = Map.member "byr" m
    hasIyr = Map.member "iyr" m
    hasEyr = Map.member "eyr" m
    hasHgt = Map.member "hgt" m
    hasHcl = Map.member "hcl" m
    hasEcl = Map.member "ecl" m
    hasPid = Map.member "pid" m
  in
    hasByr && hasIyr && hasEyr && hasHgt && hasHcl && hasEcl && hasPid

fieldSep = " \n"
endOfField = try (do {oneOf fieldSep; notFollowedBy (char '\n')})

docs :: Parsec String () [Map.Map String String]
docs = sepBy doc (string "\n\n")

doc :: Parsec String () (Map.Map String String)
doc = Map.fromList <$> sepEndBy docField endOfField

docField :: Parsec String () (String, String)
docField = do
  field <- many (noneOf ":")
  char ':'
  value <- many (noneOf fieldSep)
  return (field, value)

part1 :: [Map.Map String String] -> Int
part1 m = length (filter valid m)

main = do
  contents <- readFile "input.txt"
  let input = parse docs "input.txt" contents
  putStr "Part 1: "
  case input of
    Right m -> print (part1 m)
    Left err -> print err
