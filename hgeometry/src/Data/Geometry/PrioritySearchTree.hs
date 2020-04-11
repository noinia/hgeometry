--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.PrioritySearchTree
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implements a linear size data structure for three-sided range
-- queries in \(\mathbb{R}^2\). See
--
-- McCreight, Edward (May 1985). "Priority search trees".
-- SIAM Journal on Scientific Computing. 14 (2): 257-276.
--
-- for more details.
--
--------------------------------------------------------------------------------
module Data.Geometry.PrioritySearchTree( PrioritySearchTree(..)
                                       , createTree
                                       , queryRange
                                       ) where

import           Algorithms.DivideAndConquer (mergeSortedListsBy)
import           Control.Lens
import           Data.BinaryTree
import           Data.Ext
import           Data.Geometry.Point
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Measured.Class
import           Data.Measured.Size
import           Data.Ord (comparing, Down(..))
import           Data.Range
import qualified Data.Set as Set
import           Data.Util
--------------------------------------------------------------------------------

-- | Internal nodes store the max x-value from the left subtree and
-- the point that has globally the max-y coordinate.
data NodeData p r = NodeData { _splitPoint :: !r
                             , _maxVal     :: !(Maybe (Point 2 r :+ p))
                             } deriving (Show,Eq)

instance Bifunctor NodeData where
  bimap f g (NodeData x m) = NodeData (g x) ((bimap (fmap g) f) <$> m)

maxVal :: Lens' (NodeData p r) (Maybe (Point 2 r :+ p))
maxVal = lens _maxVal (\(NodeData x _) m -> NodeData x m)


type LeafData p r = SP r [Point 2 r :+ p]

--------------------------------------------------------------------------------

-- | A priority search tree storing points in \(\mathbb{R}^2) that
-- have an additional payload of type p.
newtype PrioritySearchTree p r =
    PrioritySearchTree { _unPrioritySearchTree :: BinLeafTree (NodeData p r) (LeafData p r) }
  deriving (Show,Eq)

instance Bifunctor PrioritySearchTree where
  -- ^ note that this is not necessarily safe, as mapping over r can
  -- invalidate the invariants. Users are responsible for making sure
  -- this does not happen.
  bimap f g (PrioritySearchTree t) = PrioritySearchTree . bimap (bimap f g) h $ t
    where
      h = bimap g (map $ bimap (fmap g) f)

--------------------------------------------------------------------------------

-- | Creates a Priority Search Tree for 3-sided range queries of the
-- form \([x_l,x_r] \times [y,\infty)\).
--
-- the base tree will be static.
--
-- pre: all points have unique x-coordinates
--
-- running time: \(O(n\log n)\)
createTree     :: Ord r => NonEmpty (Point 2 r :+ p) -> PrioritySearchTree p r
createTree pts = PrioritySearchTree $ foldr insert t pts
  where
    t = view _1
      . foldUp (\(SP l k) _ (SP r m) -> SP (Node l (NodeData k Nothing) r) m)
               (\(Elem x) -> SP (Leaf (SP x [])) x)
      . asBalancedBinLeafTree . NonEmpty.fromList
      . Set.toAscList . Set.fromList -- remove duplicates + sort
      . map (^.core.xCoord) . NonEmpty.toList $ pts

-- | Inserts a point into the priority search tree
--
-- running time: \(O(\log n)\)
insert                          :: Ord r
                                => Point 2 r :+ p
                                -> BinLeafTree (NodeData p r) (LeafData p r)
                                -> BinLeafTree (NodeData p r) (LeafData p r)
insert p = \case
    Leaf (SP x ps)                                -> Leaf $ SP x (p:ps)
      -- TODO: In case we have multiple points with the same x-coord, these points
      -- are not really in decreasing y-order.
    Node l d r | py > d^?maxVal._Just.core.yCoord ->
                   node' l (d&maxVal .~ Just p) r (d^.maxVal)
                   -- push the existing point down
               | otherwise                 ->
                   node' l d                             r (Just p)
  where
    py = Just $ p^.core.yCoord

    node' l d@(NodeData k _) r = \case
      Nothing                      -> Node l d r -- no new insertion necessary anymore
      Just q | q^.core.xCoord <= k -> Node (insert q l) d r
             | otherwise           -> Node l d (insert q r)



-- | Given a three sided range \([x_l,x_r],y\) report all points in
-- the range \([x_l,x_r] \times [y,\infty)\). The points are reported
-- in decreasing order of \(y\)-coordinate.
--
-- running time: \(O(\log n + k)\), where \(k\) is the number of reported points.
queryRange   :: Ord r
             => (Range r,r) -> PrioritySearchTree p r -> [Point 2 r :+ p]
queryRange q = queryRange' q . _unPrioritySearchTree

-- | Implementation fo the query function.
queryRange'           :: Ord r
                      => (Range r,r) -> BinLeafTree (NodeData p r) (LeafData p r)
                      -> [Point 2 r :+ p]
queryRange' q@(qr, y) = \case
    Leaf (SP x pts) | x `inRange` qr                     ->
                        takeWhile (\p -> p^.core.yCoord >= y) pts
                    | otherwise                          -> []
    Node _ (NodeData _ Nothing)  _                       -> []
      -- nothing stored here, or lower
    Node l (NodeData x (Just p)) r | p^.core.yCoord >= y -> mrep p <> merge (goL x l) (goR x r)
                                   | otherwise           -> []
                                     -- all stuff below here has lower
                                     -- y-coord, so outside the range.
  where
    mrep p | (p^.core.xCoord) `inRange` qr = [p]
           | otherwise                     = []

    goL x t' | qr^.lower <= Closed x = queryRange' q t'
             | otherwise             = []

    goR x t' | Open x < qr^.upper    = queryRange' q t'
             | otherwise             = []

    merge = mergeSortedListsBy $ comparing (Down . (^.core.yCoord))
