module Day3 where

import Prelude

import Control.Alt ((<|>))
import Data.Array (concat, filter, intercalate, mapMaybe, snoc, (!!))
import Data.Foldable (foldl, sum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits (slice)
import Day3Input as Input
import Effect (Effect)
import Effect.Console (logShow)
import Geometry (Grid, NeighborType(..), Point(..), findAllInGrid, getRow, gridPoints, gridValueAt, toGrid, validNeighbors)
import Parsing (linesFrom)

type Schematic = Grid String

type PartNumber =
  { i :: Int
  , p :: Point
  }

findLastDigit :: Int -> Array String -> Maybe Int
findLastDigit start input = case input !! start >>= Int.fromString of
  Just _ -> findLastDigit (start + 1) input <|> Just start
  Nothing -> Nothing

findFirstDigit :: Int -> Array String -> Maybe Int
findFirstDigit start input = case input !! start >>= Int.fromString of
  Just _ -> findFirstDigit (start - 1) input <|> Just start
  Nothing -> Nothing

getNumberAt :: Point -> Schematic -> Maybe PartNumber
getNumberAt (Point { x, y }) grid = do
  row <- getRow grid y
  lIdx <- findFirstDigit x row
  rIdx <- findLastDigit x row
  i <- Int.fromString $ slice lIdx (rIdx + 1) (intercalate "" row)
  pure { i, p: Point ({ x: lIdx, y }) }

findSymbols :: Schematic -> Array Point
findSymbols grid = foldl checkSymbol [] points
  where
  points = gridPoints grid
  checkSymbol a p = case gridValueAt p grid of
    Nothing -> a
    Just "." -> a
    Just v -> case Int.fromString v of
      Just _ -> a
      _ -> snoc a p

parseInput :: String -> Maybe Schematic
parseInput = linesFrom (String.split (Pattern "") >>> pure) >=> toGrid

solve1 :: String -> Maybe Int
solve1 input = do
  grid <- parseInput input
  symbols <- pure $ findSymbols grid
  allNeighbors <- pure $ symbols <#> (\p -> validNeighbors p Diagonal grid) # concat # Set.fromFoldable
  pure $ foldl (\a { i } -> a + i) 0 $ Set.mapMaybe (flip getNumberAt grid) allNeighbors

solve2 :: String -> Maybe Int
solve2 input = do
  grid <- parseInput input
  asterisks <- findAllInGrid grid "*"
  possibleGears <- pure $ asterisks <#>
    (\p -> validNeighbors p Diagonal grid # mapMaybe (flip getNumberAt grid) # Set.fromFoldable)
  gearSets <- pure $ possibleGears # filter (Set.size >>> eq 2)
  pure $ sum $ foldl (\a { i } -> a * i) 1 <$> gearSets

main :: Effect Unit
main = do
  logShow $ solve1 Input.real
  logShow $ solve2 Input.real
