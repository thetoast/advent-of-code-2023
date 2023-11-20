module Test.PriorityQueue where

import Prelude

import Data.Maybe (isNothing)
import Effect (Effect)
import PriorityQueue (PriorityQueue, empty, popMin)
import Test.Assert (assert')

--delete
--empty
--member
--peekMax
--peekMin
--popMax
--popMin
--popMaxWithPriority
--popMinWithPriority
--priority
--push

testEmpty :: Effect Unit
testEmpty = do
  q <- pure $ (empty :: PriorityQueue Int Int)
  m <- pure $ popMin q
  assert' "popMin on empty should be Nothing" $ isNothing m


main :: Effect Unit
main = do
  testEmpty
