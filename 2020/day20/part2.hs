import Day20.Data
import Day20.Parser

import Data.Maybe
import System.Environment
import qualified Data.Map as Map
import qualified Data.List as List

countHashtags :: Image -> Int
countHashtags (Image img) = let
    merged = concat img
  in
    foldr (\c acc -> if c == '#' then acc+1 else acc) 0 merged

countMonsters :: Image -> Image -> Int
countMonsters i@(Image img) monster = let
    imageMaxRow = length img - 1
    imageMaxCol = length (head img) - 1
    positions = [(row, col) | row <- [0..imageMaxRow], col <- [0..imageMaxCol]]
    monsters = filter (hasMonster i monster) positions
  in
    length monsters

hasMonster :: Image -> Image -> (Int, Int) -> Bool
hasMonster img monster position = let
    area = maskImage img monster position
    monsterFingerprint = imageFingerprint monster
  in
    matchFingerprint area monsterFingerprint

maskImage :: Image -> Image  -> (Int, Int) -> Image
maskImage (Image img) (Image mask) (row, col) = let
    maskMaxRow = length mask - 1
    maskMaxCol = length (head mask) - 1
    imageMaxRow = length img - 1
    imageMaxCol = length (head img) - 1
    area = [[img!!r!!c | c <- [col..col+maskMaxCol], c < imageMaxCol]
                       | r <- [row..row+maskMaxRow], r < imageMaxRow]
  in
    Image area

matchFingerprint :: Image -> [(Int, Int)] -> Bool
matchFingerprint _ [] = True
matchFingerprint i@(Image img) ((row,col):xs)
  | row >= length img || col >= length (head img) = False
  | otherwise = img!!row!!col == '#' && matchFingerprint i xs

imageFingerprint :: Image -> [(Int, Int)]
imageFingerprint (Image img) = let
    maxRow = length img - 1
    maxCol = length (head img) - 1
    hashtags = [[(r,c) | c <- [0..maxCol], img!!r!!c == '#'] | r <- [0..maxRow]]
  in
    concat hashtags

buildImage :: [Tile] -> Image
buildImage tiles = let
    m = placeTiles Map.empty tiles
    dim = round (sqrt (fromIntegral (length tiles)))
    img = [mergeTileRow [m Map.! (j, i) | i <- [0..dim-1]] | j <- [0..dim-1]]
  in
    Image (concat img)

mergeTileRow :: [Tile] -> [String]
mergeTileRow ts = let
    cleanTiles = map removeBorder ts
    rowLengh = length cleanTiles
    tileLength = length (head cleanTiles)
    foldLine l = foldr (\x acc -> cleanTiles!!x!!l ++ acc) "" [0..rowLengh-1]
  in
    map foldLine [0..tileLength-1]

removeBorder :: Tile -> [String]
removeBorder (Tile _ t) = let
    max = length t - 1
  in
    [[t!!j!!i | i <- [1..max-1]] | j <- [1..max-1]]

placeTiles :: Map.Map (Int, Int) Tile -> [Tile] -> Map.Map (Int, Int) Tile
placeTiles placed [] = placed
placeTiles placed remaining
  | null placed = placeFirstTile placed remaining
  | otherwise = let
      ((j, i), t) = Map.findMax placed
      rightAdj = rightAdjacent remaining t
    in case rightAdj of
      Just r -> placeTiles (Map.insert (j, i+1) r placed) (removeTile r remaining)
      Nothing -> let
          bottomAdj = bottomAdjacent remaining (placed Map.! (j, 0))
        in case bottomAdj of
          Nothing -> placed
          Just b -> placeTiles (Map.insert (j+1, 0) b placed) (removeTile b remaining)

placeFirstTile :: Map.Map (Int, Int) Tile -> [Tile] -> Map.Map (Int, Int) Tile
placeFirstTile placed remaining = let
    t00 = findTopLeft remaining
    placed' = Map.insert (0, 0) t00 placed
    remaining' = removeTile t00 remaining
  in placeTiles placed' remaining'

removeTile :: Tile -> [Tile] -> [Tile]
removeTile t tiles = let
    tileIndex = List.findIndex (\t' -> tileId t' == tileId t) tiles
  in case tileIndex of
    Nothing -> tiles
    Just i -> take i tiles ++ drop (i+1) tiles

findTopLeft :: [Tile] -> Tile
findTopLeft tiles = let
    corners = filter (isCorner tiles) tiles
    topLeft = head corners
    topBottomComb = filter (isJust . snd) (map (\t -> (t, bottomAdjacent tiles t)) (tileVariations topLeft))
    leftRightComb = filter (isJust . snd) (map (\t -> (fst t, rightAdjacent tiles (fst t))) topBottomComb)
  in
    fst (head leftRightComb)

isCorner :: [Tile] -> Tile -> Bool
isCorner tiles t = length (adjacents tiles t) == 2

adjacents :: [Tile] -> Tile -> [Tile]
adjacents tiles t = filter (\t' -> adjacent t t' && tileId t /= tileId t') tiles

rightAdjacent :: [Tile] -> Tile -> Maybe Tile
rightAdjacent tiles t
  | null adjs = Nothing
  | null rightAdj = Nothing
  | otherwise = Just (head rightAdj)
  where
    rightBorder = borders t!!1
    adjs = adjacents tiles t
    adjsVar = foldr (\v acc -> acc++tileVariations v) [] adjs
    rightAdj = filter (\t' -> let leftBorder = borders t'!!2 in leftBorder == rightBorder) adjsVar

bottomAdjacent :: [Tile] -> Tile -> Maybe Tile
bottomAdjacent tiles t
  | null adjs = Nothing
  | null bottomAdj = Nothing
  | otherwise = Just (head bottomAdj)
  where
    bottomBorder = last (borders t)
    adjs = adjacents tiles t
    adjsVar = foldr (\v acc -> acc++tileVariations v) [] adjs
    bottomAdj = filter (\t' -> let topBorder = head (borders t') in topBorder == bottomBorder) adjsVar

adjacent :: Tile -> Tile -> Bool
adjacent t1 t2 = let
    v1 = tileVariations t1
    v2 = tileVariations t2
    maxV1 = length v1 - 1
    maxV2 = length v2 - 1
    combinations = [(v1!!i, v2!!j) | i <- [0..maxV1], j <- [0..maxV2]]
  in
    any (uncurry matchingBorders) combinations

borders :: Tile -> [String]
borders (Tile _ t) = let
    top = head t
    bottom = last t
    max = length t - 1
    left = [head (t!!y) | y <- [0..max]]
    right = [t!!y!!max | y <- [0..max]]
  in
    [top, right, left, bottom]

matchingBorders :: Tile -> Tile -> Bool
matchingBorders t1 t2 = let
    b1 = borders t1
    b2 = reverse (borders t2)
    zipped = zip b1 b2
  in
    any (uncurry (==)) zipped

imageVariations :: Image -> [Image]
imageVariations (Image img) = let
    r90 = rotate img
    r180 = rotate r90
    r270 = rotate r180
    rotations = [img, r90, r180, r270]
    variations = rotations ++ map flipX rotations ++ map flipY rotations
  in
    map Image variations

tileVariations :: Tile -> [Tile]
tileVariations t = let
    r90 = rotateTile t
    r180 = rotateTile r90
    r270 = rotateTile r180
    rotations = [t, r90, r180, r270]
  in
    rotations ++ map flipTileX rotations ++ map flipTileY rotations

rotateTile :: Tile -> Tile
rotateTile (Tile tId t) = Tile tId (rotate t)

flipTileX :: Tile -> Tile
flipTileX (Tile tId t) = Tile tId (flipX t)

flipTileY :: Tile -> Tile
flipTileY (Tile tId t) = Tile tId (flipY t)

rotate :: [[a]] -> [[a]]
rotate a = let
    max = length a - 1
  in [[a!!(max-y)!!x | y <- [0..max]] | x <- [0..max]]

flipX :: [[a]] -> [[a]]
flipX a = let
    max = length a - 1
  in [[a!!(max-y)!!x | x <- [0..max]] | y <- [0..max]]

flipY :: [[a]] -> [[a]]
flipY a = let
    max = length a - 1
  in [[a!!y!!(max-x) | x <- [0..max]] | y <- [0..max]]

part2 :: [Tile] -> Int
part2 tiles = let
    img = buildImage tiles
    monster = Image ["..................#."
                    ,"#....##....##....###"
                    ,".#..#..#..#..#..#..."]
    hashtagMonster = countHashtags monster
    hashtagImg = countHashtags img
    numMonsters = maximum (map (`countMonsters` monster) (imageVariations img))
  in
    hashtagImg - (numMonsters * hashtagMonster)

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 2: "
  case input of
    Right v  -> print (part2 v)
    Left err -> print err
