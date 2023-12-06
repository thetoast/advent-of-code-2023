module Day4 where

import Prelude

import Data.Array (foldl, mapMaybe, uncons, (..))
import Data.Array as Array
import Data.Array.NonEmpty ((!!))
import Data.Foldable (sum)
import Data.Int (pow)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Day4Input as Input
import Effect (Effect)
import Effect.Console (logShow)
import Parsing (linesFrom)

type Card =
  { num :: Int
  , winners :: Set Int
  , mine :: Set Int
  }

cardRegex :: Regex
cardRegex = unsafeRegex """Card\s+(\d+): (.*) \| (.*)""" RegexFlags.noFlags

toNumbers :: String -> Set Int
toNumbers = String.split (Pattern " ") >>> mapMaybe Int.fromString >>> Set.fromFoldable

parseCard :: String -> Maybe Card
parseCard input = do
  parts <- match cardRegex input
  num <- parts !! 1 >>= map Int.fromString # join
  winners <- parts !! 2 >>= map toNumbers
  mine <- parts !! 3 >>= map toNumbers
  pure { num, winners, mine }

cardWins :: Card -> Int
cardWins card = Set.size $ Set.intersection card.winners card.mine

scoreCard :: Card -> Int
scoreCard card = 2 `pow` (flip sub 1 $ cardWins card)

parseInput :: String -> Maybe (Array Card)
parseInput = linesFrom parseCard

solve1 :: String -> Maybe Int
solve1 input = do
  cards <- parseInput input
  pure $ sum $ scoreCard <$> cards

addWinnings :: Card -> Int -> Map Int Int -> Map Int Int
addWinnings card multiplier cardCounts =
  let
    wins = cardWins card
    toAdd = if wins > 0 then (card.num + 1) .. (card.num + wins) else []
  in
    doAdd cardCounts toAdd
  where
  doAdd cardCounts' toAdd' = case uncons toAdd' of
    Nothing -> cardCounts'
    Just { head, tail } -> doAdd (Map.insertWith (+) head (1 * multiplier) cardCounts') tail

solve2 :: String -> Maybe _
solve2 input = do
  allCards <- parseInput input
  cardCounts <- pure $ Map.fromFoldable $ (\c -> (Tuple c.num 1)) <$> allCards
  pure $ sum $ Map.values $ processCards cardCounts allCards
  where
  processCards counts cards = case uncons cards of
    Nothing -> counts
    Just { head, tail } -> case Map.lookup head.num counts of
      Nothing -> counts
      Just n -> processCards (addWinnings head n counts) tail

main :: Effect Unit
main = do
  logShow $ solve1 Input.real
  logShow $ solve2 Input.real
