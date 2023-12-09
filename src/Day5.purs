module Day5 where

import Prelude

import Data.Array (uncons, (!!))
import Data.Array.NonEmpty (index)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (foldM, foldl, minimum)
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
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Day5Input as Input
import Effect (Effect)
import Effect.Console (logShow)

type Length = BigInt
type Dest = BigInt
type Range = Tuple Dest Length

type Seed = BigInt
type Soil = BigInt
type Fertilizer = BigInt
type Water = BigInt
type Light = BigInt
type Temperature = BigInt
type Humidity = BigInt
type Location = BigInt

type Almanac =
  { seeds :: Set Seed
  , seedToSoil :: Map Seed (Tuple Soil Length)
  , soilToFertilizer :: Map Soil (Tuple Fertilizer Length)
  , fertilizerToWater :: Map Fertilizer (Tuple Water Length)
  , waterToLight :: Map Water (Tuple Light Length)
  , lightToTemp :: Map Water (Tuple Temperature Length)
  , tempToHumidity :: Map Temperature (Tuple Humidity Length)
  , humidityToLocation :: Map Humidity (Tuple Location Length)
  , seedToLocation :: Map Seed Location
  }

getMapping :: BigInt -> Map BigInt (Tuple Dest Length) -> BigInt
getMapping input map = case Map.lookupLE input map of
  Just { key, value: (Tuple dest len) }
    | (key + len) > input -> input + (dest - key)
    | otherwise -> input
  Nothing -> input

getLocation :: Seed -> Almanac -> Location
getLocation seed a =
  let
    soil = getMapping seed a.seedToSoil
    fert = getMapping soil a.soilToFertilizer
    water = getMapping fert a.fertilizerToWater
    light = getMapping water a.waterToLight
    temp = getMapping light a.lightToTemp
    humidity = getMapping temp a.tempToHumidity
  in
    getMapping humidity a.humidityToLocation

completeAlmanac :: Almanac -> Almanac
completeAlmanac a = a { seedToLocation = newLoc }
  where
  newLoc = foldl (\locs seed -> Map.insert seed (getLocation seed a) locs) Map.empty a.seeds

addRange :: BigInt -> BigInt -> BigInt -> Map BigInt (Tuple Dest Length) -> Map BigInt (Tuple Dest Length)
addRange source dest len map = Map.insert source (Tuple dest len) map

emptyAlmanac :: Almanac
emptyAlmanac =
  { seeds: Set.empty
  , seedToSoil: Map.empty
  , soilToFertilizer: Map.empty
  , fertilizerToWater: Map.empty
  , waterToLight: Map.empty
  , lightToTemp: Map.empty
  , tempToHumidity: Map.empty
  , humidityToLocation: Map.empty
  , seedToLocation: Map.empty
  }

type ParseState =
  { almanac :: Almanac
  , currentMap :: String
  }

startMap :: ParseState -> String -> ParseState
startMap s name = s { currentMap = name }

updateMap :: ParseState -> BigInt -> BigInt -> BigInt -> Maybe ParseState
updateMap s@{ almanac: a, currentMap } source dest len
  | currentMap == "soil" = Just s { almanac = a { seedToSoil = addRange source dest len a.seedToSoil } }
  | currentMap == "fertilizer" = Just s { almanac = a { soilToFertilizer = addRange source dest len a.soilToFertilizer } }
  | currentMap == "water" = Just s { almanac = a { fertilizerToWater = addRange source dest len a.fertilizerToWater } }
  | currentMap == "light" = Just s { almanac = a { waterToLight = addRange source dest len a.waterToLight } }
  | currentMap == "temperature" = Just s { almanac = a { lightToTemp = addRange source dest len a.lightToTemp } }
  | currentMap == "humidity" = Just s { almanac = a { tempToHumidity = addRange source dest len a.tempToHumidity } }
  | currentMap == "location" = Just s { almanac = a { humidityToLocation = addRange source dest len a.humidityToLocation } }
  | otherwise = Nothing

parseMapping :: ParseState -> String -> Maybe ParseState
parseMapping s line = do
  parts <- pure $ String.split (Pattern " ") line
  source <- parts !! 1 >>= BigInt.fromString
  dest <- parts !! 0 >>= BigInt.fromString
  len <- parts !! 2 >>= BigInt.fromString
  updateMap s source dest len

parseSeeds1 :: ParseState -> String -> Maybe ParseState
parseSeeds1 s line = case String.split (Pattern ": ") line of
  [ "seeds", seeds ] -> case traverse BigInt.fromString $ String.split (Pattern " ") seeds of
    Just seeds' -> Just s { almanac = s.almanac { seeds = Set.fromFoldable seeds' } }
    _ -> Nothing
  _ -> Nothing

headerRegex :: Regex
headerRegex = unsafeRegex ".+-to-(.+) map:" RegexFlags.noFlags

parseHeader :: ParseState -> String -> Maybe ParseState
parseHeader s line = case match headerRegex line of
  Just a -> a `index` 1 >>= map \section -> s { currentMap = section }
  _ -> Nothing

parseInput1 :: String -> Maybe Almanac
parseInput1 input = do
  state <- pure $ { almanac: emptyAlmanac, currentMap: "" }
  lines <- pure $ String.split (Pattern "\n") input
  { head: seeds, tail } <- uncons lines
  withSeeds <- parseSeeds1 state seeds
  _.almanac <$> foldM parseLine withSeeds tail
  where
  parseLine s line = case line of
    "" -> Just s
    _ -> case parseHeader s line of
      Just s' -> Just s'
      Nothing -> parseMapping s line

solve1 :: String -> Maybe BigInt
solve1 input = do
  almanac <- parseInput1 input <#> completeAlmanac
  minimum $ Map.values almanac.seedToLocation

main :: Effect Unit
main = do
  logShow $ solve1 Input.real
