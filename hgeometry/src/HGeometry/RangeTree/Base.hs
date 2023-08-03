{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.RangeTree.Base
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Range Tree implementation
--
--------------------------------------------------------------------------------
module HGeometry.RangeTree.Base
  ( RangeTree
  , buildRangeTree
  , fromAscList
  , fromGroupedAscList

  , rangeQuery
  , query
  ) where


import           Control.Lens
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Type.Ord
import qualified Data.Vector as Vector
import           HGeometry.Foldable.Sort
import           HGeometry.Interval
import           HGeometry.Measured
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.SegmentTree.CanonicalSubSet
import           HGeometry.Tree.Binary.Static
import           HGeometry.Vector.NonEmpty.Util ()

--------------------------------------------------------------------------------

-- | Type Representing a generic (1D) range-tree.
newtype RangeTree f point = RangeTree (SubTree f point (NumType point))


deriving instance (Show (f point), Show (NumType point)) => Show (RangeTree f point)
deriving instance (Eq (f point), Eq (NumType point))     => Eq   (RangeTree f point)

-- | The actual tree type
type SubTree f point r = BinLeafTree (NodeData f point r)
                                     (LeafData f point r)


-- | Data stored at a leaf
data LeafData f point r = LeafData { _thePoint      :: !r
                                   , _leafCanonical :: !(f point)
                                   } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

-- | Access the point stored in this leaf.
thePoint :: Lens (LeafData f point r) (LeafData f point s) r s
thePoint = lens _thePoint (\ld p -> ld { _thePoint = p})

instance HasCanonicalSubSet (LeafData f point r) (LeafData g point r) point f g where
  canonicalSubSet = lens _leafCanonical (\(LeafData p _) cs -> LeafData p cs)

-- | The data stored at an internal node.
data NodeData f point r = NodeData { _split           :: !r
                                   , _canonicalSubSet :: !(f point)
                                   } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

-- | Access the split value of a NodeData
split :: Lens (NodeData f point r) (NodeData f point s) r s
split = lens _split (\nd x -> nd { _split = x })

instance HasCanonicalSubSet (NodeData f point r) (NodeData g point r) point f g where
  canonicalSubSet = lens _canonicalSubSet (\(NodeData x _) cs -> NodeData x cs)

instance HasCanonicalSubSet (SubTree f point r) (SubTree f point r) point f f where
  -- | Sets the node data of the root
  canonicalSubSet = lens get'' (flip set'')
    where
      get'' = \case
        Leaf ld     -> ld^.canonicalSubSet
        Node _ nd _ -> nd^.canonicalSubSet
      set'' cs = \case
        Leaf ld     -> Leaf $ ld&canonicalSubSet .~ cs
        Node l nd r -> Node l (nd&canonicalSubSet .~ cs) r


-- | Construct a rangetree on n points.
--
-- \(O(n\log n)\)
buildRangeTree :: ( Foldable1 g, Point_ point d r, Ord r, 1 <= d
                  , Semigroup (f point), Measured f point
                  ) => g point -> RangeTree f point
buildRangeTree = fromGroupedAscList
               . NonEmpty.groupWith1 (^.xCoord)
               . NonEmpty.fromList . Vector.toList
               . sortOnCheap (^.xCoord)

-- | Given the points in sorted order, builds the range tree.
--
-- The running time and space depends on the monoid used. In particlar, we use (<>) at
-- every internal node.
fromAscList :: ( Foldable1 g, Functor g, Point_ point d r, 1 <= d
               , Semigroup (f point), Measured f point
               ) => g point -> RangeTree f point
fromAscList = fromGroupedAscList . fmap (:| [])

-- | Given the points in groups per x-coordinate, in increasing order of x-coordinate
fromGroupedAscList :: ( Foldable1 g, Foldable1 h, Point_ point d r, 1 <= d
                      , Semigroup (f point), Measured f point
                      ) => g (h point) -> RangeTree f point
fromGroupedAscList = RangeTree . fst . foldUp node' leaf' . asBalancedBinLeafTree
  where
    leaf' pts            = let p = NonEmpty.head $ toNonEmpty pts
                               x = p^.xCoord
                               m = foldMap1 measure pts
                           in (Leaf $ LeafData x m, x)
    node' (l,m) _ (r,m') = ( Node l (NodeData m $ l^.canonicalSubSet <> r^.canonicalSubSet) r
                           , m'
                           )

-- | Report the canonical subsets of the nodes that together represent the query interval.
--
-- \(O(\log n)\)
rangeQuery                  :: (Interval_ interval r, Point_ point d r, 1 <= d, Ord r)
                            => interval -> RangeTree f point -> [f point]
rangeQuery q (RangeTree t0) = findSplitNode t0
  where
    --
    findSplitNode t = case t of
      Leaf ld     -> goReport ld
      Node l nd r -> case (nd^.split) `compareInterval` q of
                       LT -> findSplitNode r
                       EQ -> reportRightSubtrees l <> reportLeftSubtrees r
                       GT -> findSplitNode l

    -- | Report a leaf (if needed)
    goReport ld
        | (ld^.thePoint) `stabsInterval` q = [ld^.canonicalSubSet]
        | otherwise                        = []

    -- | walk the left-path reporting the right subtrees
    reportRightSubtrees = \case
      Leaf ld     -> goReport ld
      Node l nd r -> case (nd^.split) `compareInterval` q of
                       LT -> reportRightSubtrees r
                       EQ -> reportSubTree r : reportRightSubtrees l
                       GT -> error "RangeTree.rangeQuery, reportRightSubtrees absurd"

    -- | walk the right-path reporting left subtrees
    reportLeftSubtrees = \case
      Leaf ld     -> goReport ld
      Node l nd r -> case (nd^.split) `compareInterval` q of
                       LT -> error "RangeTree.rangeQuery, reportLeftSubtrees absurd"
                       EQ -> reportSubTree l : reportLeftSubtrees r
                       GT -> reportLeftSubtrees l

    -- | report the canonical subset of the node
    reportSubTree = \case
      Leaf ld     -> ld^.canonicalSubSet
      Node _ nd _ -> nd^.canonicalSubSet


-- | Query the range tree
--
-- \(O(\log n + k)\), where \(k\) is somehow depends on the output size (and the monoid
-- used).
query   :: (Interval_ interval r, Point_ point d r, 1 <= d, Ord r, Monoid (f point))
        => interval -> RangeTree f point -> f point
query q = mconcat . rangeQuery q
