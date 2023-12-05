module Day1 where

import Prelude

import Data.Array (findMap, foldl, fromFoldable, reverse, zip)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..), fst)
import Day1Input (realInput)
import Effect (Effect)
import Effect.Console (logShow)
import Parsing (linesFrom)

type CalibrationValue = Tuple Int Int

parseLine1 :: String -> Maybe CalibrationValue
parseLine1 input = do
  arr <- pure $ String.split (Pattern "") input
  first <- findMap Int.fromString arr
  last <- findMap Int.fromString $ reverse arr
  pure $ (Tuple first last)

parseInput1 :: String -> Maybe (Array CalibrationValue)
parseInput1 = linesFrom parseLine1

solve1 :: String -> Maybe Int
solve1 input = do
  values' <- parseInput1 input
  pure $ foldl (\a (Tuple l r) -> a + (l * 10 + r)) 0 values'

values :: Map Pattern Int
values = Map.fromFoldable $
  [ Tuple (Pattern "1") 1
  , Tuple (Pattern "one") 1
  , Tuple (Pattern "2") 2
  , Tuple (Pattern "two") 2
  , Tuple (Pattern "3") 3
  , Tuple (Pattern "three") 3
  , Tuple (Pattern "4") 4
  , Tuple (Pattern "four") 4
  , Tuple (Pattern "5") 5
  , Tuple (Pattern "five") 5
  , Tuple (Pattern "6") 6
  , Tuple (Pattern "six") 6
  , Tuple (Pattern "7") 7
  , Tuple (Pattern "seven") 7
  , Tuple (Pattern "8") 8
  , Tuple (Pattern "eight") 8
  , Tuple (Pattern "9") 9
  , Tuple (Pattern "nine") 9
  ]

type StrMatch = Pattern -> String -> Maybe Int
type IntMatch = Int -> Int -> Boolean

findBy :: StrMatch -> IntMatch -> String -> Maybe Int
findBy fn chk input = do
  patterns <- pure $ Map.keys values # fromFoldable
  indexes <- pure $ (\p -> fn p input) <$> patterns
  patternsWithIndex <- pure $ zip patterns indexes
  flip Map.lookup values =<< fst <$> foldl checkLower Nothing patternsWithIndex
  where
  checkLower (Just (Tuple _ Nothing)) _ = Nothing -- invalid
  checkLower Nothing (Tuple _ Nothing) = Nothing
  checkLower Nothing v@(Tuple _ (Just _)) = Just v
  checkLower a@(Just (Tuple _ (Just _))) (Tuple _ Nothing) = a
  checkLower a@(Just (Tuple _ (Just lIdx))) v@(Tuple _ (Just rIdx))
    | rIdx `chk` lIdx = Just v
    | otherwise = a

parseLine2 :: String -> Maybe CalibrationValue
parseLine2 input = do
  first <- findBy String.indexOf (<) input
  last <- findBy String.lastIndexOf (>) input
  pure $ (Tuple first last)

parseInput2 :: String -> Maybe (Array CalibrationValue)
parseInput2 = linesFrom parseLine2

solve2 :: String -> Maybe Int
solve2 input = do
  values' <- parseInput2 input
  pure $ foldl (\a (Tuple l r) -> a + (l * 10 + r)) 0 values'

main :: Effect Unit
main = do
  logShow $ solve1 realInput
  logShow $ solve2 realInput
