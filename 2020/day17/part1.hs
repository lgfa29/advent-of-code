import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Day17.Parser
import Day17.Data

runCycle :: CubeSet -> CubeSet
runCycle cs = let
    expanded = Map.union cs (boundaries cs)
  in
    Map.foldrWithKey
      (\coord _ acc -> Map.insert coord (nextState cs coord) acc) Map.empty expanded

boundaries :: CubeSet -> CubeSet
boundaries cs = Map.foldrWithKey
  (\coord _ acc -> Map.union acc (boundary cs coord)) Map.empty cs

boundary :: CubeSet -> Coord -> CubeSet
boundary cs c = let
    ns = neighborsCoords c
    cs' = Map.fromList (map (\c' -> (c', Cube False)) ns)
  in
    Map.difference cs' cs

nextState :: CubeSet -> Coord -> Cube
nextState cs c = let
    n = neighbors cs c
    activeNeighbors = length (filter active n)
    state = Map.findWithDefault (Cube False) c cs
  in
    if active state
    then
      if activeNeighbors == 2 || activeNeighbors == 3
      then Cube True
      else Cube False
    else
      if activeNeighbors == 3
      then Cube True
      else Cube False

neighbors :: CubeSet -> Coord -> [Cube]
neighbors cs c = let
    maybeNeighbors = map (`Map.lookup` cs) (neighborsCoords c)
  in
    catMaybes maybeNeighbors

neighborsCoords :: Coord -> [Coord]
neighborsCoords (Coord x y z) = [
  Coord (x+x') (y+y') (z+z')
  | x' <- [-1..1]
  , y' <- [-1..1]
  , z' <- [-1..1]
  , (x', y', z') /= (0, 0, 0) ]

countActive :: CubeSet -> Int
countActive = Map.foldr (\cube acc -> if active cube then acc+1 else acc) 0

part1 :: CubeSet -> Int
part1 cs = countActive (iterate runCycle cs !! 6)

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 1: "
  case input of
    Right v  -> print (part1 v)
    Left err -> print err
