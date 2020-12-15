import Day13.Parser

timeSync :: (Int, Int) -> (Int, Int) -> Int -> Int -> Int
timeSync a@(aggId, t0) b@(busId, offset) n0 m0 = let
    n = if n0 /= 0 then n0 else t0 `div` aggId
    m = if m0 /= 0 then m0 else t0 `div` busId
    t1 = aggId * n + t0
    t2 = departAfter m t1 busId
    t1' = (aggId * (n + 1) + t0)
    m'' = t2 `div` busId
  in
    case compare t2 (t1 + offset) of
      EQ -> t1
      GT -> timeSync a b (n+1) (t1' `div` busId)
      LT -> timeSync a b n (m'' + 1)

findSync :: (Int, Int) -> [(Int, Int)] -> (Int, Int)
findSync a [] = a
findSync (0, 0) (x:xs)  = findSync x xs
findSync current@(a, _) (bus@(b, _):xs) = let
    syncedId = lcm a b
    syncedTime = timeSync current bus 0 0
  in
    findSync (syncedId, syncedTime) xs

departAfter :: Int -> Int -> Int -> Int
departAfter start ts id = head (take 1 [t | x <- [start..], let t = x*id, t >= ts])

part2 :: [Int] -> Int
part2 ids = let
    idsOffset = filter (\a -> fst a /= -1) (zip ids [0..])
  in
    snd (findSync (head idsOffset) (tail idsOffset))

main = do
  contents <- readFile "input.txt"
  let input = parseIds "input.txt" contents
  putStr "Part 2: "
  case input of
    Right v -> print (part2 v)
    Left err -> print err
