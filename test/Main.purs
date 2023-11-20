module Test.Main where

import Prelude

import Effect (Effect)

import Test.PriorityQueue (main) as PQ

main :: Effect Unit
main = do
  PQ.main
