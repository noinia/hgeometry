{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.SegmentTree
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Segment Tree implementation
--
--------------------------------------------------------------------------------
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

  -- , HasCanonicalSubSet(..)
  , ascEndPoints
  -- , elementaryIntervals
  -- , ElementaryInterval(..)
  ) where


import           Control.Lens
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Monoid (Sum(..))
import qualified Data.Vector as Vector
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import           HGeometry.Foldable.Sort
import           HGeometry.Intersection
import           HGeometry.Interval
import           HGeometry.Measured.Size
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Tree.Binary.Static
import           HGeometry.Vector.NonEmpty.Util ()

--------------------------------------------------------------------------------



-- | An Elementary interval is either a singleton closed interval, or an open interval.
data ElementaryInterval r set = EndPoint'    !r                 !set
                              | ElemInterval !(OpenInterval r)  !set
                              deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

type instance NumType (ElementaryInterval r set) = r


instance HasStart (ElementaryInterval r set) r where
  start = lens get'' (flip set'')
    where
      get'' = \case
        EndPoint'    s _ -> s
        ElemInterval i _ -> i^.start
      set'' s = \case
        EndPoint' _ cs    -> EndPoint' s cs
        ElemInterval i cs -> ElemInterval (i&start .~ s) cs

instance HasEnd (ElementaryInterval r set) r where
  end = lens get'' (flip set'')
    where
      get'' = \case
        EndPoint' s _    -> s
        ElemInterval i _ -> i^.end
      set'' s = \case
        EndPoint' _ cs    -> EndPoint' s cs
        ElemInterval i cs -> ElemInterval (i&end .~ s) cs

instance HasStartPoint (ElementaryInterval r set) (AnEndPoint r)  where
  startPoint = lens get'' (flip set'')
    where
      get'' = \case
        EndPoint'    s _ -> AnClosedE s
        ElemInterval i _ -> AnOpenE $ i^.start
      set'' = error "ElementaryInterva.startPoint set not implemented"
      -- set'' s = \case
      --   EndPoint' _ cs    -> EndPoint s cs
      --   ElemInterval i cs -> ElemInterval (i&start .~ s) cs

instance HasEndPoint (ElementaryInterval r set) (AnEndPoint r)  where
  endPoint = lens get'' (flip set'')
    where
      get'' = \case
        EndPoint'    s _ -> AnClosedE s
        ElemInterval i _ -> AnOpenE $ i^.end
      set'' = error "ElementaryInterva.endPoint set not implemented"


-- | Our Leaves and Nodes both store canonical subsets
class HasCanonicalSubSet s t interval f g | s -> f
                                          , t -> g where
  -- | Lens to access the canonical subset of a node or leaf
  canonicalSubSet :: Lens s t (f interval) (g interval)

instance HasCanonicalSubSet (ElementaryInterval r (f interval))
                            (ElementaryInterval r (g interval)) interval f g where
  canonicalSubSet = lens get'' (flip set'')
    where
      get'' = \case
        EndPoint' _ cs    -> cs
        ElemInterval _ cs -> cs
      set'' cs = \case
        EndPoint' x _    -> EndPoint' x cs
        ElemInterval i _ -> ElemInterval i cs


-- | The data that we store at each node; essentially the interval and a canonical subset.
data NodeData f interval =
  NodeData { _nodeInterval    :: !(Interval AnEndPoint (NumType interval))
           , _canonicalSubSet :: !(f interval)
           }

deriving stock instance ( Show (NumType interval), Show (f interval)
                        ) => Show (NodeData f interval)

deriving stock instance ( Eq (NumType interval), Eq (f interval)
                        ) => Eq (NodeData f interval)

-- | Lens to access the node interval
nodeInterval :: Lens' (NodeData f interval) (Interval AnEndPoint (NumType interval))
nodeInterval = lens _nodeInterval (\nd x -> nd { _nodeInterval = x })

instance HasCanonicalSubSet (NodeData f interval) (NodeData g interval) interval f g where
  canonicalSubSet = lens _canonicalSubSet (\nd x -> nd { _canonicalSubSet = x })



-- | A Segment tree
newtype SegmentTree f interval = SegmentTree (SubTree f interval)

type instance NumType   (SegmentTree f interval) = NumType interval
type instance Dimension (SegmentTree f interval) = 1


-- | The actual segment tree immplementation
type SubTree f interval = BinLeafTree (NodeData f interval)
                                      (ElementaryInterval (NumType interval) (f interval))

deriving stock instance ( Show (NumType interval), Show (f interval)
                        ) => Show (SegmentTree f interval)

deriving stock instance ( Eq (NumType interval), Eq (f interval)
                        ) => Eq (SegmentTree f interval)


-- | Get the interval of a subtree
interval :: Getter (SubTree f interval) (Interval AnEndPoint (NumType interval))
interval = to $ \case
  Leaf atomic -> Interval (atomic^.startPoint) (atomic^.endPoint)
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
    t = buildSkeleton . NonEmptyVector . Vector.uniq . sort
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
    leaf' (Elem elemInt) = Leaf elemInt
    node' l _ r          = let int = Interval (l^.interval.startPoint) (r^.interval.endPoint)
                           in Node l (NodeData int mempty) r


-- | Given a set of endpoints, in ascendin order, construct the atomic intervals defined
-- by them.
toAtomicIntervals :: ( Foldable1 g
                     , Monoid (f interval)
                     ) => g r -> NonEmpty (ElementaryInterval r (f interval))
toAtomicIntervals (toNonEmpty -> (x0 :| endPts)) =
    EndPoint' x0 mempty :| mconcat (zipWith mkInterval' (x0 : endPts) endPts)
  where
    mkInterval' l r = [ElemInterval (OpenInterval l r) mempty, EndPoint' r mempty]

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
    ic = ClosedInterval (i^.start) (i^.end)

    go (Leaf atomic) = Leaf $ atomic&canonicalSubSet %~ (mi <>)
    go (Node l nd r)
      | i `covers` (nd^.nodeInterval) = Node l (nd&canonicalSubSet %~ (mi <>)) r
      | otherwise                     = let l' = if intersectsLeft  l nd then go l else l
                                            r' = if intersectsRight r nd then go r else r
                                        in Node l' nd r'

    intersectsLeft  l _nd = ic `intersects` (l^.interval)
    intersectsRight r _nd = ic `intersects` (r^.interval)


-- | Test if the first interval covers the second interval.
covers       :: (ClosedInterval_ interval r, Interval_ interval' r, Ord r)
             => interval -> interval' -> Bool
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
    go (Node l nd r) = let ch = if Point1 q `intersects` (l^.interval) then l else r
                       in nd^.canonicalSubSet : go ch

-- | Query the segment tree
--
-- \(O(\log n + k)\), where \(k\) is somehow depends on the output size (and the monoid
-- used).
query   :: (Ord r, ClosedInterval_ interval r, Monoid (f interval))
        => r -> SegmentTree f interval -> f interval
query q = mconcat . stab q

--------------------------------------------------------------------------------

-- | Report te list of endpoints that the segment tree is built on in left to right order.
ascEndPoints :: SegmentTree f interval -> [NumType interval]
ascEndPoints = foldMap (\case
                           EndPoint' x _ -> [x]
                           _             -> []
                       ) . elementaryIntervals


-- | Report the atomic intervals in left-to-right order
elementaryIntervals                 :: SegmentTree f interval
                                    -> NonEmpty (ElementaryInterval(NumType interval) (f interval))
elementaryIntervals (SegmentTree t) = toNonEmpty t

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
