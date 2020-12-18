import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Day17.Parser
import Day17.Data

runCycle :: CubeSet4D -> CubeSet4D
runCycle cs = let
    expanded = Map.union cs (boundaries cs)
  in
    Map.foldrWithKey
      (\coord _ acc -> Map.insert coord (nextState cs coord) acc) Map.empty expanded

boundaries :: CubeSet4D -> CubeSet4D
boundaries cs = Map.foldrWithKey
  (\coord _ acc -> Map.union acc (boundary cs coord)) Map.empty cs

boundary :: CubeSet4D -> Coord4D -> CubeSet4D
boundary cs c = let
    ns = neighborsCoords c
    cs' = Map.fromList (map (\c' -> (c', Cube False)) ns)
  in
    Map.difference cs' cs

nextState :: CubeSet4D -> Coord4D -> Cube
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

neighbors :: CubeSet4D -> Coord4D -> [Cube]
neighbors cs c = let
    maybeNeighbors = map (`Map.lookup` cs) (neighborsCoords c)
  in
    catMaybes maybeNeighbors

neighborsCoords :: Coord4D -> [Coord4D]
neighborsCoords (Coord4D a b c d) = [
  Coord4D (a+a') (b+b') (c+c') (d+d')
  | a' <- [-1..1]
  , b' <- [-1..1]
  , c' <- [-1..1]
  , d' <- [-1..1]
  , (a', b', c', d') /= (0, 0, 0, 0) ]

countActive :: CubeSet4D -> Int
countActive = Map.foldr (\cube acc -> if active cube then acc+1 else acc) 0

toCubeSet4D :: CubeSet -> CubeSet4D
toCubeSet4D = Map.foldrWithKey
      (\(Coord x y z) cube acc -> Map.insert (Coord4D x y z 0) cube acc) Map.empty

part1 :: CubeSet -> Int
part1 cs = countActive (iterate runCycle (toCubeSet4D cs) !! 6)

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents

  case input of
    Right v  -> print (part1 v)
    Left err -> print err
