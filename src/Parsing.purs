module Parsing where

import Prelude

import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

type Parser x = String -> Maybe x

-- | Simple line parser which breaks an input string up by newline and runs
-- | each line through a parser which may return a value. Returns an array of
-- | parsed values if all lines are parsed successfully or Nothing if one more
-- | lines fail
linesFrom :: forall x. Parser x -> String -> Maybe (Array x)
linesFrom parser = split (Pattern "\n") >>> traverse parser

-- | Simple string parser that parses a string of format "xxxx yyyy"
-- | Takes two Parser functions and returns a Tuple of both results if
-- | successful
tupleParser :: forall x y. Parser x -> Parser y -> String -> Maybe (Tuple x y)
tupleParser = customTupleParser (Pattern " ")

-- | Simple string parser that parses a string of format "xxxx yyyy"
-- | Takes one Parser function and another function which returns a Parser
-- | based on the value x, then returns a Tuple of both results if successful
-- |
-- | This is useful if the value y is dependent on the value x
tupleParser' :: forall x y. Parser x -> (x -> Parser y) -> String -> Maybe (Tuple x y)
tupleParser' = customTupleParser' (Pattern " ")

-- | String parser that parses a string of format "xxxxSSSyyyy"
-- | Takes two Parser functions and returns a Tuple of both results if
-- | successful
-- |
-- | Additionally takes in a Pattern which provides tha format of the separator SSS
customTupleParser :: forall x y. Pattern -> Parser x -> Parser y -> String -> Maybe (Tuple x y)
customTupleParser pattern parseLeft parseRight = customTupleParser' pattern parseLeft (\_ -> parseRight)

-- | String parser that parses a string of format "xxxxSSSyyyy"
-- | Takes one Parser function and another function which returns a Parser
-- | based on the value x, then returns a Tuple of both results if successful
-- |
-- | Additionally takes in a Pattern which provides tha format of the separator SSS
-- |
-- | This is useful if the value y is dependent on the value x
customTupleParser' :: forall x y. Pattern -> Parser x -> (x -> Parser y) -> String -> Maybe (Tuple x y)
customTupleParser' pattern parseLeft parseRight line = do
  let parts = split pattern line
  left <- parts !! 0 >>= parseLeft
  right <- parts !! 1 >>= parseRight left
  Just $ Tuple left right
