module Graph where

import Prelude

import Data.Map (Map, insertWith, lookup)
import Data.Map (empty, insert, lookup) as M
import Data.Maybe (Maybe, fromMaybe)
import Data.Set (Set, insert, singleton, toUnfoldable)
import Data.Set (empty) as S
import Data.Tuple (Tuple(..))

type Node :: forall k. k -> k
type Node a = a

type Graph a v =
  { nodes :: Set (Node a)
  , edges :: Map (Node a) (Set (Node a))
  , values :: Map (Node a) v
  }

type Edge a = Tuple (Node a) (Node a)

type Path a = Array (Node a)

setValue :: forall a v. Ord a => Graph a v -> Node a -> v -> Graph a v
setValue g@{ values } node value = g { values = M.insert node value values }

getValue :: forall a v. Ord a => Graph a v -> Node a -> Maybe v
getValue { values } node = M.lookup node values

addEdge :: forall a v. Ord a => Graph a v -> Edge a -> Graph a v
addEdge { nodes, edges, values } (Tuple a b) =
  { nodes: insert a nodes # insert b
  , edges: insertWith (<>) a (singleton b) edges # insertWith (<>) b (singleton a)
  , values
  }

nodeEdges :: forall a v. Ord a => Node a -> Graph a v -> Array (Node a)
nodeEdges node { edges } = lookup node edges <#> toUnfoldable # fromMaybe []

allNodes :: forall a v. Ord a => Graph a v -> Set (Node a)
allNodes = _.nodes

emptyGraph :: forall a v. Graph a v
emptyGraph = { nodes: S.empty, edges: M.empty, values: M.empty }
