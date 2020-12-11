import Text.Parsec

seats :: Parsec String () [(Int, Int)]
seats = sepEndBy seat (char '\n')

seat :: Parsec String () (Int, Int)
seat = do
  r <- row
  c <- col
  return (r, c)

row :: Parsec String () Int
row = do
  r <- count 7 (oneOf "FB")
  return (decodeRow r)

col :: Parsec String () Int
col = do
  c <- count 3 (oneOf "LR")
  return (decodeCol c)

decodeRow :: String -> Int
decodeRow input = decode input 0 127

decodeCol :: String -> Int
decodeCol input = decode input 0 7

decode :: String -> Int -> Int -> Int
decode [] min _ = min
decode (x:xs) min max
  | x == 'F' || x == 'L' = decode xs min (mid min max)
  | x == 'B' || x == 'R' = decode xs (mid min max + 1) max

mid :: Int -> Int-> Int
mid min max = (min + max) `div` 2

seatId :: (Int, Int) -> Int
seatId (r, c) = r * 8 + c

seatIds :: [(Int, Int)] -> [Int]
seatIds = map seatId

part1 :: [(Int, Int)] -> Int
part1 seats = maximum (seatIds seats)

main = do
  contents <- readFile "input.txt"
  let input = parse seats "input.txt" contents
  putStr "Part 1: "
  case input of
    Right r -> print (part1 r)
    Left err -> print err
