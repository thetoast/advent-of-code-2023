module Day2 where

import Prelude

import Data.Array (filter, transpose, (!!))
import Data.Array as Array
import Data.Foldable (foldM, foldl)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Day2Input as Input
import Debug (spy)
import Effect (Effect)
import Effect.Console (logShow)
import Parsing (linesFrom)

data Color = Red | Blue | Green

derive instance Eq Color
derive instance Ord Color

type Reveal = Array (Tuple Color Int)

type Game =
  { id :: Int
  , reveals :: Array Reveal
  , max :: Map Color Int
  }

parsePair :: String -> Maybe (Tuple Color Int)
parsePair input =
  case split (Pattern " ") input of
    [ i, "red" ] -> Int.fromString i <#> Tuple Red
    [ i, "blue" ] -> Int.fromString i <#> Tuple Blue
    [ i, "green" ] -> Int.fromString i <#> Tuple Green
    _ -> Nothing

parseReveal :: String -> Maybe Reveal
parseReveal input = do
  s <- pure $ split (Pattern ", ") input
  traverse parsePair s

calcMax :: Array Reveal -> Map Color Int
calcMax reveals = foldl (\a (Tuple c i) -> Map.insertWith max c i a) Map.empty (Array.concat reveals)

parseGame :: String -> Maybe Game
parseGame input = do
  s1 <- pure $ split (Pattern ": ") input
  id <- s1 !! 0 <#> split (Pattern " ") >>= flip (!!) 1 >>= Int.fromString
  reveals <- s1 !! 1 <#> split (Pattern "; ") >>= traverse parseReveal
  max <- pure $ calcMax reveals
  pure $ { id, reveals, max }

parseGames :: String -> Maybe (Array Game)
parseGames input = linesFrom parseGame input

part1Reqs :: Map Color Int
part1Reqs = Map.fromFoldable
  [ (Tuple Red 12)
  , (Tuple Green 13)
  , (Tuple Blue 14)
  ]

meetReqs :: Map Color Int -> Game -> Boolean
meetReqs reqs { max: game } =
  case
    do
      rRed <- Map.lookup Red reqs
      rGreen <- Map.lookup Green reqs
      rBlue <- Map.lookup Blue reqs
      gRed <- Map.lookup Red game
      gGreen <- Map.lookup Green game
      gBlue <- Map.lookup Blue game
      pure $ (rRed >= gRed) && (rGreen >= gGreen) && (rBlue >= gBlue)
    of
    Just b -> b
    Nothing -> false

solve1 :: String -> Maybe Int
solve1 input = do
  games <- parseGames input
  filtered <- pure $ filter (meetReqs part1Reqs) games
  pure $ foldl (\a { id } -> a + id) 0 filtered

calcPower :: Game -> Maybe Int
calcPower { max } = do
  r <- Map.lookup Red max
  g <- Map.lookup Green max
  b <- Map.lookup Blue max
  pure $ r * g * b

solve2 :: String -> Maybe Int
solve2 input = do
  games <- parseGames input
  foldM (\a g -> calcPower g <#> (+) a) 0 games

main :: Effect Unit
main = do
  logShow $ solve1 Input.real
  logShow $ solve2 Input.real
