module Geometry where

import Prelude

import Data.Array (concat, filter, intercalate, length, modifyAt, replicate, updateAt, (!!), (..))
import Data.Array as Array
import Data.Array.NonEmpty ((!!)) as NE
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (fromString, pow, toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.Number (sqrt)
import Data.Ord (abs)
import Data.String (split, Pattern(..))
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

-- | Distance class represents and entity that can hava a distance from another entity
class Distance a where
  dist :: a -> a -> Number

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------
newtype Point = Point
  { x :: Int
  , y :: Int
  }

instance eqPoint :: Eq Point where
  eq (Point p1) (Point p2) = p1.x == p2.x && p1.y == p2.y

instance ordPoint :: Ord Point where
  compare (Point p1) (Point p2) = if p1.x == p2.x then compare p1.y p2.y else compare p1.x p2.x

instance showPoint :: Show Point where
  show (Point { x, y }) = "(" <> show x <> "," <> show y <> ")"

instance distPoint :: Distance Point where
  dist (Point p1) (Point p2) = sqrt $ toNumber ((pow (p1.x - p2.x) 2) + (pow (p1.y - p2.y) 2))

instance Semiring Point where
  add (Point p1) (Point p2) = Point { x: p1.x + p2.x, y: p1.y + p2.y }
  mul (Point p1) (Point p2) = Point { x: p1.x * p2.x, y: p1.y * p2.y }
  zero = Point { x: 0, y: 0 }
  one = Point { x: 1, y: 1 }

instance Ring Point where
  sub (Point p1) (Point p2) = Point { x: p1.x - p2.x, y: p1.y - p2.y }

pointRegex :: Regex
pointRegex = unsafeRegex "\\(?(-?\\d+),(-?\\d+)\\)?" noFlags

pointFromString :: String -> Maybe Point
pointFromString s = do
  m <- match pointRegex s
  x <- fromString =<< (join $ m NE.!! 1)
  y <- fromString =<< (join $ m NE.!! 2)
  pure $ Point { x, y }

makePoints :: Array Int -> Array Int -> Array Point
makePoints xs ys = do
  x <- xs
  y <- ys
  [ Point { x, y } ]

--------------------------------------------------------------------------------
-- Point3D
--------------------------------------------------------------------------------
newtype Point3d = Point3d
  { x :: Int
  , y :: Int
  , z :: Int
  }

instance eqPoint3d :: Eq Point3d where
  eq (Point3d p1) (Point3d p2) = p1.x == p2.x && p1.y == p2.y && p1.z == p2.z

instance ordPoint3d :: Ord Point3d where
  compare (Point3d p1) (Point3d p2) =
    if p1.x == p2.x && p1.y == p2.y then
      compare p1.z p2.z
    else if p1.x == p2.x then
      compare p1.y p2.y
    else
      compare p1.x p2.x

instance showPoint3d :: Show Point3d where
  show (Point3d { x, y, z }) = "(" <> show x <> "," <> show y <> "," <> show z <> ")"

instance distPoint3d :: Distance Point3d where
  dist (Point3d p1) (Point3d p2) = sqrt $ toNumber ((pow (p1.x - p2.x) 2) + (pow (p1.y - p2.y) 2) + (pow (p1.z - p2.z) 2))

point3dRegex :: Regex
point3dRegex = unsafeRegex "\\(?(-?\\d+),(-?\\d+),(-?\\d+)\\)?" noFlags

point3dFromString :: String -> Maybe Point3d
point3dFromString s = do
  m <- match point3dRegex s
  x <- fromString =<< (join $ m NE.!! 1)
  y <- fromString =<< (join $ m NE.!! 2)
  z <- fromString =<< (join $ m NE.!! 3)
  pure $ Point3d { x, y, z }

--------------------------------------------------------------------------------
-- Line
--------------------------------------------------------------------------------
newtype Line = Line
  { start :: Point
  , stop :: Point
  }

instance showLine :: Show Line where
  show (Line { start, stop }) = show start <> " -> " <> show stop

--------------------------------------------------------------------------------
-- Dimensions
--------------------------------------------------------------------------------
newtype Dimensions = Dimensions
  { width :: Int
  , height :: Int
  }

instance showDimensions :: Show Dimensions where
  show (Dimensions { width, height }) = show width <> "x" <> show height

--------------------------------------------------------------------------------
-- Rect
--------------------------------------------------------------------------------
newtype Rect = Rect
  { origin :: Point
  , size :: Dimensions
  }

instance showRect :: Show Rect where
  show (Rect { origin, size }) = "( " <> show origin <> " " <> show size <> " )"

-- | Creates a rectangle from two opposing points, normalizing the origin so that
-- | it is min x and y and so that the dimensions are positive
fromPoints :: Point -> Point -> Rect
fromPoints (Point { x: x1, y: y1 }) (Point { x: x2, y: y2 }) =
  let
    origin = Point { x: min x1 x2, y: min y1 y2 }

    size = Dimensions { width: abs $ x2 - x1 + 1, height: abs $ y2 - y1 + 1 }
  in
    Rect { origin, size }

minX :: Rect -> Int
minX (Rect { origin: Point { x } }) = x

maxX :: Rect -> Int
maxX (Rect { origin: Point { x }, size: Dimensions { width } }) = x + width - 1

minY :: Rect -> Int
minY (Rect { origin: Point { y } }) = y

maxY :: Rect -> Int
maxY (Rect { origin: Point { y }, size: Dimensions { height } }) = y + height - 1

contains :: Point -> Rect -> Boolean
contains (Point { x, y }) r = x >= minX r && x <= maxX r && y >= minY r && y <= maxY r

--------------------------------------------------------------------------------
-- Grid
--------------------------------------------------------------------------------
newtype Grid a = Grid (Array (Array a))

instance showGridStrings :: Show (Grid String) where
  show (Grid rows) =
    let
      showRow = foldl (<>) "\n"
    in
      map showRow rows # foldl (<>) ""
else instance showGrid :: Show a => Show (Grid a) where
  show (Grid rows) =
    let
      showRow = map show >>> intercalate "" >>> (<>) "\n"
    in
      map showRow rows # foldl (<>) ""

data NeighborType
  = Adjacent
  | Diagonal

neighbors :: Point -> NeighborType -> Array Point
neighbors (Point { x, y }) Adjacent =
  [ Point { x: x - 1, y }
  , Point { x: x + 1, y }
  , Point { x, y: y + 1 }
  , Point { x, y: y - 1 }
  ]

neighbors p@(Point { x, y }) Diagonal =
  concat
    [ neighbors p Adjacent
    , [ Point { x: x - 1, y: y - 1 }
      , Point { x: x + 1, y: y - 1 }
      , Point { x: x - 1, y: y + 1 }
      , Point { x: x + 1, y: y + 1 }
      ]
    ]

inGrid :: forall a. Grid a -> Point -> Boolean
inGrid grid (Point { x, y }) = case gridDimensions grid of
  Just (Dimensions d) -> x >= 0 && y >= 0 && x <= d.width - 1 && y <= d.height - 1
  Nothing -> false

validNeighbors :: forall a. Point -> NeighborType -> Grid a -> Array Point
validNeighbors point ntype g = neighbors point ntype # filter (inGrid g)

gridPoints :: forall a. Grid a -> Array Point
gridPoints g = case gridDimensions g of
  Just (Dimensions d) -> 0 .. (d.height - 1) >>= \y -> 0 .. (d.width - 1) <#> \x -> Point { x, y }
  Nothing -> []

gridDimensions :: forall a. Grid a -> Maybe Dimensions
gridDimensions (Grid rows) = do
  height <- pure $ length rows
  width <- length <$> rows !! 0
  pure $ Dimensions { width, height }

gridValueAt :: forall a. Point -> Grid a -> Maybe a
gridValueAt (Point { x, y }) (Grid ys) = ys !! y >>= \xs -> xs !! x

toGrid :: forall a. Array (Array a) -> Maybe (Grid a)
toGrid a = Just (Grid a)

parseLine :: String -> Maybe (Array Int)
parseLine = split (Pattern "") >>> traverse fromString

gridFromIntStrings :: String -> Maybe (Grid Int)
gridFromIntStrings = split (Pattern "\n") >>> traverse parseLine >=> toGrid

modifyGridAt :: forall a. Point -> (a -> a) -> Grid a -> Maybe (Grid a)
modifyGridAt (Point { x, y }) fn (Grid rows) = do
  cols <- rows !! y
  newRow <- modifyAt x fn cols
  updateAt y newRow rows >>= toGrid

insertGridAt :: forall a. Point -> a -> Grid a -> Maybe (Grid a)
insertGridAt p i = modifyGridAt p (\_ -> i)

findInGrid :: forall a. Eq a => Grid a -> a -> Maybe Point
findInGrid (Grid g) a = foldlWithIndex findA Nothing g
  where
  findA y acc v =
    if isJust acc then acc
    else case Array.findIndex (eq a) v of
      Just x -> Just (Point { x, y })
      Nothing -> Nothing

findAllInGrid :: forall a. Eq a => Grid a -> a -> Maybe (Array Point)
findAllInGrid g a = gridPoints g # Array.filterA (flip gridValueAt g >>> map (eq a))

-- | does not handle negative x/y coords, assumes 0,0 origin
gridFromPoints :: forall a. Array Point -> Tuple a a -> Maybe (Grid a)
gridFromPoints points (Tuple emptyValue filledValue) = do
  (Point { x: x0, y: y0 }) <- points !! 0
  { maxX, maxY } <- pure $ foldl updateMax { maxX: x0, maxY: y0 } points
  grid <- pure $ Grid $ (\n -> replicate n emptyValue) <$> replicate (maxY + 1) (maxX + 1)
  foldl (\g p -> g >>= insertGridAt p filledValue) (Just grid) points
  where
  updateMax a (Point { x, y }) =
    { maxX: max x a.maxX
    , maxY: max y a.maxY
    }

data Direction
  = Up
  | Right
  | Down
  | Left

-- | Gets a set of points extending from a start point in a direction
-- | Note: Does not include the start point
pointRange :: forall a. Grid a -> Point -> Direction -> Maybe (Array Point)
pointRange grid (Point { x, y }) direction = do
  (Dimensions { height, width }) <- gridDimensions grid
  let
    xs = case direction of
      Right -> Array.range (min (x + 1) (width - 1)) (width - 1)
      Left -> Array.range (max (x - 1) 0) 0
      _ -> [ x ]
    ys = case direction of
      Down -> Array.range (min (y + 1) (height - 1)) (height - 1)
      Up -> Array.range (max (y - 1) 0) (0)
      _ -> [ y ]
  Just $ makePoints xs ys

-- | Gets an array of values for points in a given direction from a start point
-- | Note: does not include the start point
getLine :: forall a. Grid a -> Point -> Direction -> Maybe (Array a)
getLine grid p direction = do
  points <- pointRange grid p direction
  traverse (\point -> gridValueAt point grid) points
