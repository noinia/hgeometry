--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.IntervalTree
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.IntervalTree
  ( IntervalTree
  , createTree
  , fromIntervals
  , insert, delete
  , stab, search
  , listIntervals
  ) where

import           Control.DeepSeq
import           Control.Lens
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Ord (Down(..))
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           HGeometry.Interval
import           HGeometry.Tree.Binary.Static

--------------------------------------------------------------------------------

-- | Information stored in a node of the Interval Tree
data NodeData i r = NodeData { _splitPoint     :: !r
                             , _intervalsLeft  :: !(Map.Map r        [i])
                             , _intervalsRight :: !(Map.Map (Down r) [i])
                             } deriving stock (Show,Eq,Ord,Generic)

instance (NFData i, NFData r) => NFData (NodeData i r)

-- | IntervalTree type, storing intervals of type interval
newtype IntervalTree interval r =
  IntervalTree { _unIntervalTree :: BinaryTree (NodeData interval r) }
  deriving stock (Show,Eq,Generic)

instance (NFData i, NFData r) => NFData (IntervalTree i r)

-- | Given an ordered list of points, create an interval tree
--
-- \(O(n)\)
createTree :: (Foldable f, Ord r) => f r -> IntervalTree interval r
createTree = IntervalTree
           . fmap (\m -> NodeData m mempty mempty)
           . asBalancedBinTree

-- | Build an interval tree
--
-- \(O(n \log n)\)
fromIntervals    :: (Foldable f, Ord r, ClosedInterval_ interval r)
                 => f interval -> IntervalTree interval r
fromIntervals is = foldr insert (createTree pts) is
  where
    pts = foldr (\i s -> Set.insert (i^.start) $ Set.insert (i^.end) s) Set.empty is

-- | Lists the intervals. We don't guarantee anything about the order
--
-- running time: \(O(n)\).
listIntervals :: IntervalTree interval r -> [interval]
listIntervals = toList' . _unIntervalTree
  where
    toList' Nil              = []
    toList' (Internal l v r) =
      concat [concat $ v^..to _intervalsLeft.traverse, toList' l, toList' r]

--------------------------------------------------------------------------------

-- | Find all intervals that stab x
--
-- \(O(\log n + k)\), where k is the output size
search :: Ord r => r -> IntervalTree interval r -> [interval]
search = stab

-- | Find all intervals that stab x
--
-- \(O(\log n + k)\), where k is the output size
stab                    :: Ord r => r -> IntervalTree interval r -> [interval]
stab q (IntervalTree t) = stab' t
  where
    stab' Nil = []
    stab' (Internal l (NodeData m ll rr) r)
      | q <= m    = let is = f (<= q). Map.toAscList $ ll
                    in is <> stab' l
      | otherwise = let is = f (<= (Down q)) . Map.toAscList $ rr
                    in is <> stab' r
    f p = concatMap snd . List.takeWhile (p . fst)

--------------------------------------------------------------------------------

-- | Insert the interval into the tree.
--
-- pre: the interval intersects some midpoint in the tree
--
-- \(O(\log n)\), where n is the number of endpoints the tree is built on
insert                    :: (Ord r, ClosedInterval_ interval r)
                          => interval -> IntervalTree interval r -> IntervalTree interval r
insert i (IntervalTree t) = IntervalTree $ insert' t
  where
    insert' Nil                  = Nil
    insert' (Internal l nd@(_splitPoint -> m) r)
      | m `stabsInterval` i = Internal l (insertInNode nd) r
      | (i^.end) <= m       = Internal (insert' l) nd r
      | otherwise           = Internal l nd (insert' r)

    insertInNode (NodeData m l r) = NodeData m (Map.insertWith (<>) (i^.start)      [i] l)
                                               (Map.insertWith (<>) (Down $ i^.end) [i] r)


-- | Delete an interval from the Tree
--
-- \(O(\log n)\) (under some general position assumption)
delete                    :: (Ord r, ClosedInterval_ interval r, Eq interval)
                          => interval -> IntervalTree interval r -> IntervalTree interval r
delete i (IntervalTree t) = IntervalTree $ delete' t
  where
    delete' Nil                  = Nil
    delete' (Internal l nd@(_splitPoint -> m) r)
      | m `stabsInterval` i = Internal l (deleteFromNode nd) r
      | (i^.end) <= m       = Internal (delete' l) nd r
      | otherwise           = Internal l nd (delete' r)

    deleteFromNode (NodeData m l r) = NodeData m (Map.update f (i^.start)      l)
                                                 (Map.update f (Down $ i^.end) r)

    f is = let is' = List.delete i is in if null is' then Nothing else Just is'

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- test'' = fromIntervals test
-- test  = [Interval (Open (97 :+ ())) (Closed (228 :+ ())) ,Interval (Open (18 :+ ())) (Open (79 :+ ())),Interval (Closed (126 :+ ())) (Open (167 :+ ())),Interval (Closed (105 :+ ())) (Closed (158 :+ ())),Interval (Closed (126 :+ ())) (Closed (211 :+ ())),Interval (Closed (111 :+ ())) (Open (194 :+ ())),Interval (Closed (120 :+ ())) (Open (302 :+ ())),Interval (Closed (92 :+ ())) (Closed (140 :+ ()))]

-- test = fromIntervals [ closedInterval 0 10
--                      , closedInterval 5 15
--                      , closedInterval 1 4
--                      , closedInterval 3 9
--                      ]

-- closedInterval a b = ClosedInterval (ext a) (ext b)
