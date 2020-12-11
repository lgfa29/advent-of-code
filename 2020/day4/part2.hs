import Text.Parsec
import qualified Data.Map as Map

invalid = "INVALID"

-- Validation
valid :: Map.Map String String -> Bool
valid m = validByr m
            && validIyr m
            && validEyr m
            && validHgt m
            && validHcl m
            && validEcl m
            && validPid m

validByr :: Map.Map String String -> Bool
validByr m = let k = "byr" in Map.member k m && validValue k m && validIntRange 1920 2002 k m

validIyr :: Map.Map String String -> Bool
validIyr m = let k = "iyr" in Map.member k m && validValue k m && validIntRange 2010 2020 k m

validEyr :: Map.Map String String -> Bool
validEyr m = let k = "eyr" in Map.member k m && validValue k m && validIntRange 2020 2030 k m

validHgt :: Map.Map String String -> Bool
validHgt m = validHgtCm m || validHgtIn m

validHgtCm :: Map.Map String String -> Bool
validHgtCm m = let k = "hgt_cm" in validValue k m && validIntRange 150 193 k m

validHgtIn :: Map.Map String String -> Bool
validHgtIn m = let k = "hgt_in" in validValue k m && validIntRange 59 76 k m

validHcl :: Map.Map String String -> Bool
validHcl m = let k = "hcl" in Map.member k m && validValue k m

validEcl :: Map.Map String String -> Bool
validEcl m = let k = "ecl" in Map.member k m && validValue k m

validPid :: Map.Map String String -> Bool
validPid m = let k = "pid" in Map.member k m && validValue k m

validIntRange :: Int -> Int -> String -> Map.Map String String -> Bool
validIntRange min max k m = let
    i = read (Map.findWithDefault "0" k m) :: Int
  in
    i >= min && i <= max

validValue :: String -> Map.Map String String -> Bool
validValue k m = Map.findWithDefault "" k m /= invalid

-- Parsing
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
  case field of
    "byr" -> byrField
    "iyr" -> iyrField
    "eyr" -> eyrField
    "hgt" -> hgtField
    "hcl" -> hclField
    "ecl" -> eclField
    "pid" -> pidField
    "cid" -> cidField

byrField :: Parsec String () (String, String)
byrField = do
  value <- tryOrInvalid yearField
  return ("byr", value)

iyrField :: Parsec String () (String, String)
iyrField = do
  value <- tryOrInvalid yearField
  return ("iyr", value)

eyrField :: Parsec String () (String, String)
eyrField = do
  value <- tryOrInvalid yearField
  return ("eyr", value)

hgtField :: Parsec String () (String, String)
hgtField = do
  value <- tryOrInvalid (many digit)
  unit <- tryOrInvalid (try (string "cm") <|> try (string "in"))
  return ("hgt_"++unit, value)

hclField :: Parsec String () (String, String)
hclField = do
  value <- tryOrInvalid hexColor
  return ("hcl", value)

eclField :: Parsec String () (String, String)
eclField = do
  value <- tryOrInvalid colorName
  return ("ecl", value)

pidField :: Parsec String () (String, String)
pidField = do
  value <- tryOrInvalid passportIdField
  return ("pid", value)

cidField :: Parsec String () (String, String)
cidField = do
  value <- many1 (noneOf fieldSep)
  return ("cid", value)

yearField :: Parsec String () String
yearField = count 4 digit <* notFollowedBy digit

passportIdField :: Parsec String () String
passportIdField = count 9 digit <* notFollowedBy digit

hexColor :: Parsec String () String
hexColor = char '#' >> count 6 hexDigit <* notFollowedBy hexDigit

colorName :: Parsec String () String
colorName = try (string "amb")
             <|> try (string "blu")
             <|> try (string "brn")
             <|> try (string "gry")
             <|> try (string "grn")
             <|> try (string "hzl")
             <|> string "oth"
             <?> "color"

tryOrInvalid :: Parsec String () String -> Parsec String () String
tryOrInvalid p = try p <|> invalid <$ many (noneOf fieldSep)

part2 :: [Map.Map String String] -> Int
part2 m = length (filter valid m)

main = do
  contents <- readFile "input.txt"
  let input = parse docs "input.txt" contents
  putStr "Part 2: "
  case input of
    Right m -> print (part2 m)
    Left err -> print err
