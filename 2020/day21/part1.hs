import Day21.Data
import Day21.Parser

import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set

updateMap :: Map.Map String AllergenCount -> Map.Map String AllergenCount
updateMap m
  | null candiates = m
  | otherwise = updateMap m'
  where
    candiates = Map.assocs (Map.filter hasAllergen m)
    (removeIngredient, allergens) = head candiates
    removeAllergen = findAllergen allergens
    m' = removeIngredientWithAllergen m removeIngredient removeAllergen

removeIngredientWithAllergen :: Map.Map String AllergenCount -> String -> String -> Map.Map String AllergenCount
removeIngredientWithAllergen m i a = let
    updatedAllergenCount = Map.map (Map.delete a) m
  in Map.delete i updatedAllergenCount

findAllergen :: AllergenCount -> String
findAllergen m
  | null allergenList || length allergenList > 1 = ""
  | otherwise = head allergenList
  where
    allergenList = Map.keys (hasAllergenFilter m)

hasAllergen :: AllergenCount -> Bool
hasAllergen m = length (hasAllergenFilter m) == 1

hasAllergenFilter :: AllergenCount -> AllergenCount
hasAllergenFilter = Map.filter (== 1)

applyAllergenCountFraction :: AllergenCount -> Map.Map String AllergenCount -> Map.Map String AllergenCount
applyAllergenCountFraction ac m = Map.map (\v -> Map.intersectionWith div v ac) m

allergenCount :: [Food] -> Map.Map String AllergenCount
allergenCount [] = Map.empty
allergenCount ((Food ingredients allergens):fs) = let
    counts = map (\i -> (i, allergensMap allergens)) ingredients
  in Map.unionWith mergeAllergenCounts (Map.fromList counts) (allergenCount fs)

mergeAllergenCounts :: AllergenCount -> AllergenCount -> AllergenCount
mergeAllergenCounts = Map.unionWith (+)

allergensMap :: [String] -> AllergenCount
allergensMap allergens = Map.fromList (map (\a -> (a, 1)) allergens)

countIngredient :: [Food] -> String -> Int
countIngredient [] _ = 0
countIngredient ((Food ingredients _):fs) i =
  foldr (\v acc -> if v == i then acc+1 else acc) 0 ingredients + countIngredient fs i

countFoodsWithAllergen :: [Food] -> String -> Int
countFoodsWithAllergen [] _ = 0
countFoodsWithAllergen ((Food _ allergens):fs) a =
  foldr (\v acc -> if v == a then acc+1 else acc) 0 allergens + countFoodsWithAllergen fs a

ingredients :: [Food] -> Set.Set String
ingredients [] = Set.empty
ingredients ((Food i _):fs) = Set.union (Set.fromList i) (Main.ingredients fs)

allergens :: [Food] -> Set.Set String
allergens [] = Set.empty
allergens ((Food _ a):fs) = Set.union (Set.fromList a) (Main.allergens fs)

--hack :: [Food] -> Map.Map String (Map.Map String Float)
--hack foods = let
--    keys =

--part1 :: [Food] -> Int
part1 foods = let
    allergenCountFoods = Map.fromList (map (\f -> (f, countFoodsWithAllergen foods f)) (Set.elems (Main.allergens foods)))
    m = applyAllergenCountFraction allergenCountFoods (allergenCount foods)
    m' = updateMap m
    noAllergen = Map.keys (Map.filter null m')
  in sum (map (countIngredient foods) noAllergen)

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 1: "
  case input of
    Right v  -> print (part1 v)
    Left err -> print err
