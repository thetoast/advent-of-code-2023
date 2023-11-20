module PriorityQueue
  ( PriorityQueue
  , delete
  , empty
  , member
  , peekMax
  , peekMin
  , popMax
  , popMin
  , popMaxWithPriority
  , popMinWithPriority
  , priority
  , push
  ) where

import Prelude
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust)
import Data.Set (Set)
import Data.Set as S
import Partial.Unsafe (unsafePartial)

data PQItem p i
  = PQI
    { p :: p
    , i :: i
    }

instance ordPQI :: (Ord p, Ord i) => Ord (PQItem p i) where
  compare (PQI { p: p1, i: i1 }) (PQI { p: p2, i: i2 }) = if p1 == p2 then compare i1 i2 else compare p1 p2

instance eqPQI :: (Eq p, Eq i) => Eq (PQItem p i) where
  eq (PQI { p: p1, i: i1 }) (PQI { p: p2, i: i2 }) = p1 == p2 && i1 == i2

instance showPQI :: (Show p, Show i) => Show (PQItem p i) where
  show (PQI { p, i }) = "{(p=" <> show p <> ") " <> show i <> "}"

data PriorityQueue p i
  = PQ
    { set :: Set (PQItem p i)
    , map :: Map i (PQItem p i)
    }

instance showPQ :: (Show p, Show i) => Show (PriorityQueue p i) where
  show (PQ { set }) = show set

empty :: forall p i. PriorityQueue p i
empty = PQ { set: S.empty, map: M.empty }

insert :: forall p i. Ord p => Ord i => p -> i -> PriorityQueue p i -> PriorityQueue p i
insert p i (PQ { set, map }) =
  let
    item = PQI { p, i }
  in
    PQ { set: S.insert item set, map: M.insert i item map }

update :: forall p i. Ord p => Ord i => p -> i -> PriorityQueue p i -> Maybe (PriorityQueue p i)
update p i (PQ { set, map }) = do
  exist <- M.lookup i map
  item <- pure $ PQI { p, i }
  pure $ PQ { set: S.delete exist set # S.insert item, map: M.insert i item map }

push :: forall p i. Ord p => Ord i => p -> i -> PriorityQueue p i -> PriorityQueue p i
push p i q@(PQ { map }) = case M.lookup i map of
  Just (PQI { p: p2 }) -> if p /= p2 then unsafePartial $ fromJust $ update p i q else q
  Nothing -> insert p i q

peekWith :: forall p i. Ord p => Ord i => (Set (PQItem p i) -> Maybe (PQItem p i)) -> PriorityQueue p i -> Maybe i
peekWith op (PQ { set }) = do
  PQI { i } <- op set
  pure i

peekMax :: forall p i. Ord p => Ord i => PriorityQueue p i -> Maybe i
peekMax = peekWith S.findMax

peekMin :: forall p i. Ord p => Ord i => PriorityQueue p i -> Maybe i
peekMin = peekWith S.findMin

popWith ::
  forall p i.
  Ord p =>
  Ord i =>
  (Set (PQItem p i) -> Maybe (PQItem p i)) ->
  PriorityQueue p i -> Maybe { item :: i, queue :: PriorityQueue p i }
popWith op (PQ { set, map }) = do
  item@(PQI { i }) <- op set
  newMap <- pure $ M.delete i map
  newSet <- pure $ S.delete item set
  pure { item: i, queue: PQ { map: newMap, set: newSet } }

popMax :: forall p i. Ord p => Ord i => PriorityQueue p i -> Maybe { item :: i, queue :: PriorityQueue p i }
popMax = popWith S.findMax

popMin :: forall p i. Ord p => Ord i => PriorityQueue p i -> Maybe { item :: i, queue :: PriorityQueue p i }
popMin = popWith S.findMin

popWithPriority ::
  forall p i.
  Ord p =>
  Ord i =>
  (Set (PQItem p i) -> Maybe (PQItem p i)) ->
  PriorityQueue p i -> Maybe { item :: i, priority :: p, queue :: PriorityQueue p i }
popWithPriority op (PQ { set, map }) = do
  item@(PQI { i, p }) <- op set
  newMap <- pure $ M.delete i map
  newSet <- pure $ S.delete item set
  pure { item: i, priority: p, queue: PQ { map: newMap, set: newSet } }

popMaxWithPriority ::
  forall p i.
  Ord p =>
  Ord i =>
  PriorityQueue p i -> Maybe { item :: i, priority :: p, queue :: PriorityQueue p i }
popMaxWithPriority = popWithPriority S.findMax

popMinWithPriority ::
  forall p i.
  Ord p =>
  Ord i =>
  PriorityQueue p i -> Maybe { item :: i, priority :: p, queue :: PriorityQueue p i }
popMinWithPriority = popWithPriority S.findMin

member :: forall p i. Ord p => Ord i => i -> PriorityQueue p i -> Boolean
member i (PQ { map }) = M.member i map

priority :: forall p i. Ord p => Ord i => i -> PriorityQueue p i -> Maybe p
priority i (PQ { map }) = do
  (PQI { p }) <- M.lookup i map
  pure p

delete :: forall p i. Ord p => Ord i => i -> PriorityQueue p i -> PriorityQueue p i
delete i pq@(PQ { set, map }) = case M.lookup i map of
  Just item -> PQ { set: S.delete item set, map: M.delete i map }
  Nothing -> pq
