{-# LANGUAGE UndecidableInstances #-}
module HGeometry.SegmentTree
  ( SegmentTree
  , buildSegmentTree
  , buildSkeleton

  , stab
  , query

  , insert

  , MeasureF(..)
  , Report(..)
  , Count(..)
  ) where

import           Control.Lens
import           Data.Foldable1
import           HGeometry.Foldable.Sort
import           HGeometry.Interval
import           HGeometry.Interval.EndPoint
import           HGeometry.Vector.NonEmpty.Util ()
-- import HGeometry.Measured.Class
import           HGeometry.Measured.Size
import           HGeometry.Properties
import           HGeometry.Intersection
import           HGeometry.Tree.Binary.Static
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import           Data.Monoid (Sum(..))

--------------------------------------------------------------------------------

-- | An atomic interval just stores a canonical subset, nothing else.
data AtomicInterval f interval r =
  AtomicInterval { _left   :: !r
                 -- ^ left endpoint is open
                 , _right  :: !r
                 -- ^ right endpoint is closed
                 , _subset :: !(f interval)
                 } deriving stock (Show,Eq)

type instance NumType (AtomicInterval f interval r) = r

instance HasStart (AtomicInterval f interval r) r where
  start = lens _left (\ai x -> ai { _left = x})

instance HasEnd (AtomicInterval f interval r) r where
  end = lens _right (\ai x -> ai { _right = x})

instance HasStartPoint (AtomicInterval f interval r) (EndPoint Open r)  where
  startPoint = lens (OpenE . _left) (\ai (OpenE x) -> ai { _left = x})

instance HasEndPoint (AtomicInterval f interval r) (EndPoint Closed r) where
  endPoint = lens (ClosedE . _right) (\ai (ClosedE x) -> ai { _right = x})

-- instance Monoid (f interval) => IntervalLike_ (AtomicInterval f interval r) r where
--   mkInterval (OpenE l) (ClosedE r) = AtomicInterval l r mempty
-- instance Monoid (f interval) => Interval_ (AtomicInterval f interval r) r



class HasCanonicalSubSet s t interval f g | s -> f
                                          , t -> g where
  -- | Lens to access the canonical subset of a node or leaf
  canonicalSubSet :: Lens s t (f interval) (g interval)

instance HasCanonicalSubSet (AtomicInterval f interval r)
                            (AtomicInterval g interval r) interval f g where
  canonicalSubSet = lens _subset (\ai x -> ai { _subset = x })

-- | The data that we store at each node
data NodeData f interval =
  NodeData { _split           :: !(NumType interval)
           , _nodeInterval    :: !(Interval AnEndPoint (NumType interval))
           , _canonicalSubSet :: !(f interval)
           }

deriving stock instance ( Show (NumType interval), Show (f interval)
                        ) => Show (NodeData f interval)

deriving stock instance ( Eq (NumType interval), Eq (f interval)
                        ) => Eq (NodeData f interval)

-- | Lens to access the split point of a node
split :: Lens' (NodeData f interval) (NumType interval)
split = lens _split (\nd x -> nd { _split = x })

-- | Lens to access the node interval
nodeInterval :: Lens' (NodeData f interval) (Interval AnEndPoint (NumType interval))
nodeInterval = lens _nodeInterval (\nd x -> nd { _nodeInterval = x })


instance HasCanonicalSubSet (NodeData f interval) (NodeData g interval) interval f g where
  canonicalSubSet = lens _canonicalSubSet (\nd x -> nd { _canonicalSubSet = x })



-- | A Segment tree
newtype SegmentTree f interval =
  SegmentTree (BinLeafTree (NodeData f interval)
                           (AtomicInterval f interval (NumType interval))
              )

deriving stock instance ( Show (NumType interval), Show (f interval)
                        ) => Show (SegmentTree f interval)

deriving stock instance ( Eq (NumType interval), Eq (f interval)
                        ) => Eq (SegmentTree f interval)

interval :: Getter (BinLeafTree (NodeData f interval)
                                (AtomicInterval f interval (NumType interval))
                   ) (Interval AnEndPoint (NumType interval))
interval = to $ \case
  Leaf atomic -> Interval (AnOpenE $ atomic^.start) (AnClosedE $ atomic^.end)
  Node _ nd _ -> nd^.nodeInterval

--------------------------------------------------------------------------------

-- | Given a set of intervals, build a segment tree
--
-- \(O(n\log n)\)
buildSegmentTree      :: forall f interval r g.
                         ( ClosedInterval_ interval r, Ord r
                         , Monoid (f interval), MeasureF f interval, Foldable1 g
                         )
                      => g interval -> SegmentTree f interval
buildSegmentTree ints = foldr insert t ints
  where
    t = buildSkeleton . NonEmptyVector . sort
      . foldMap1 (\i -> (i^.start) :| [i^.end]) $ ints

-- | Given te endpoints in ascending order, build an empty segment-tree
--
-- \(O(n)\)
buildSkeleton :: forall f interval r g.
                 ( ClosedInterval_ interval r, Ord r
                 , Monoid (f interval), Foldable1 g
                 )
              => g r -> SegmentTree f interval
buildSkeleton = SegmentTree
              . foldUp node' leaf'
              . asBalancedBinLeafTree . toAtomicIntervals
  where
    leaf' (Elem atomic) = Leaf atomic
    node' l _ r         = let int = Interval (l^.interval.startPoint) (r^.interval.endPoint)
                          in Node l (NodeData (l^.interval.end) int mempty) r


-- | Given a set of endpoints, in ascendin order, construct the atomic intervals defined
-- by them.
toAtomicIntervals :: ( Foldable1 g
                     , Monoid (f interval)
                     ) => g r -> NonEmpty (AtomicInterval f interval r)
toAtomicIntervals (toNonEmpty -> endPts@(x0 :| _)) =
    NonEmpty.zipWith mkInterval' (x0 NonEmpty.<| endPts) endPts
  where
    mkInterval' l r = AtomicInterval l r mempty

--------------------------------------------------------------------------------

-- | Insert a segment into the segment tree, whose endpoints are already in the tree
--
-- \(O(\log n)\)
insert                   :: ( ClosedInterval_ interval r, Ord r, MeasureF f interval
                            , Semigroup (f interval)
                            )
                         => interval -> SegmentTree f interval -> SegmentTree f interval
insert i (SegmentTree t) = SegmentTree $ go t
  where
    mi = measure i

    go (Leaf atomic) = Leaf $ atomic&canonicalSubSet %~ (mi <>)
    go (Node l nd r)
      | i `covers` (nd^.nodeInterval) = Node l (nd&canonicalSubSet %~ (mi <>)) r
      | otherwise                     = let l' = if intersectsLeft  l nd then go l else l
                                            r' = if intersectsRight l nd then go r else r
                                        in Node l' nd r'

    -- intersectsLeft  l nd = i^.end <= nd^.split
    -- intersectsRight r nd = i^.end >  nd^.split
    intersectsLeft  l _nd = i `intersects` (l^.interval)
    intersectsRight r _nd = i `intersects` (r^.interval)


-- | Test if the first interval covers the second interval.
covers       :: (ClosedInterval_ interval r, Ord r)
             => interval -> Interval AnEndPoint r -> Bool
i `covers` j = i^.start <= j^.start && j^.end <= i^.end


--------------------------------------------------------------------------------

{-

delete                   :: ( ClosedInterval_ interval r, Ord r, MeasureF f interval
                            , Semigroup (f interval)
                            )
                         => interval -> SegmentTree f interval -> SegmentTree f interval
delete i (SegmentTree t) = go t
  where
    go (Leaf atomic) = Leaf $ deleteAtomic i atomic
    go (Node l nd r)
      | i `covers` (nd^.nodeInterval) = Node l (nd&canonicalSubSet %~ delete' i) r
      | otherwise                     = let l' = if i^.start < nd^.split then go l else l
                                            r' = if i^.end   > nd^.split then go r else r
                                        in Node l' nd r'

delete' = undefined

deleteAtomic i (AtomicInterval xs) = AtomicInterval $ delete' i xs

-}

--------------------------------------------------------------------------------

-- | Query the segment tree, returns a list of canonical subsets; one for each relevant
-- node. Each element of the resulting list represents all intervals stored at that node
-- that are stabbed by q.
--
-- \(O(\log n)\)
stab                   :: ( Ord r
                          , ClosedInterval_ interval r
                          ) => r -> SegmentTree f interval -> [f interval]
stab q (SegmentTree t) = go0 t
  where
    go0 n@(Leaf _)                           = go n
    go0 n@(Node _ nd _)
      | q `stabsInterval` (nd^.nodeInterval) = go n
      | otherwise                            = []

    go (Leaf atomic) = [atomic^.canonicalSubSet]
    go (Node l nd r) = let ch = if q <= nd^.split then l else r
                       in nd^.canonicalSubSet : go ch

-- | Query the segment tree
--
-- \(O(\log n + k)\), where \(k\) is somehow depends on the output size (and the monoid
-- used).
query   :: (Ord r, ClosedInterval_ interval r, Monoid (f interval))
        => r -> SegmentTree f interval -> f interval
query q = mconcat . stab q

--------------------------------------------------------------------------------


class MeasureF f a where
  -- | Given a single a, measure it into someting of type 'f a'.
  measure :: a -> f a

-- | Type to represent reporting
newtype Report interval = Report [interval]
  deriving (Show,Eq,Ord,Semigroup,Monoid)

instance MeasureF Report interval where
  measure = Report . (:[])

-- | Counting queries
newtype Count interval = Count Int
  deriving (Show,Eq,Ord)
  deriving (Semigroup,Monoid) via Sum Int

instance MeasureF Count interval where
  measure = const $ Count 1
