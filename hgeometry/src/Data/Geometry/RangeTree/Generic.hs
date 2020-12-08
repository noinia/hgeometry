module Data.Geometry.RangeTree.Generic where

import           Control.Lens
import           Data.BinaryTree
import           Data.Ext
import           Data.Geometry.Properties
import           Data.Geometry.RangeTree.Measure
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Range
import           Data.Measured.Class
import           Data.Measured.Size
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Util

--------------------------------------------------------------------------------

data NodeData v r = NodeData { _minVal     :: !(Min r)
                             , _maxVal     :: !(Max r)
                             , _assoc      :: !v
                             } deriving (Show,Eq,Functor)

instance (Semigroup v, Ord r) => Semigroup (NodeData v r) where
  NodeData mi ma v <> NodeData mi' ma' v' = NodeData (mi <> mi') (ma <> ma') (v <> v')


-- | A generic (1D) range tree. The 'r' parameter indicates the type
-- of the coordinates of the points. The 'q' represents any associated
-- data values with those points (stored in the leaves), and the 'v'
-- types represents the data stored at internal nodes.
newtype RangeTree v q r =
  RangeTree { _unRangeTree :: BinLeafTree (NodeData v r) (NodeData (v,q) r) }
    deriving (Show,Eq)



--------------------------------------------------------------------------------

-- | Creates a range tree
createTree   :: ( Ord r
                , Measured v p
                , Semigroup p
                )
             => NonEmpty (r :+ p)
             -> RangeTree v p r
createTree = createTree'
           . fmap (\pts -> let x =  pts^.to NonEmpty.head.core
                           in x :+ (sconcat . fmap (^.extra) $ pts))
           . NonEmpty.groupAllWith1 (^.core) -- sort and group on r value

-- | pre: input is sorted and grouped by x-coord
createTree'     :: (Ord r, Measured v p)
                => NonEmpty (r :+ p)
                -> RangeTree v p r
createTree' pts = RangeTree t
  where
    t = view _1
      . foldUp (\(SP l dl) _ (SP r dr) -> let d = dl <> dr in SP (Node l d r) d
               )
               (\(Elem (x :+ ld)) -> let v = measure ld
                                     in SP (Leaf $ NodeData (Min x) (Max x) (v,ld))
                                           (NodeData (Min x) (Max x) v)
               )
      . asBalancedBinLeafTree $ pts

--------------------------------------------------------------------------------
-- * Converting to a List

-- | Lists all points in increasing order
--
-- running time: \(O(n)\)
toAscList :: RangeTree v p r -> NonEmpty (r :+ p)
toAscList = fmap (\(NodeData (Min x) _ (_,d)) -> x :+ d) . toNonEmpty . _unRangeTree

--------------------------------------------------------------------------------
-- * Querying x

-- | Range search
--
-- running time: \(O(\log n)\)
search    :: (Ord r, Monoid v) => Range r -> RangeTree v p r -> v
search qr = mconcat . search' qr

-- | Range search, report the (associated data structures of the)
-- \(O(\log n)\) nodes that form the disjoint union of the range we
-- are querying with.
--
-- running time: \(O(\log n)\)
search'    :: Ord r
           => Range r -> RangeTree v p r -> [v]
search' qr = search'' qr . _unRangeTree

-- | The actual search
search''    :: Ord r
            => Range r
            -> BinLeafTree (NodeData v r) (NodeData (v,q) r)
            -> [v]
search'' qr t = case t of
    Leaf (NodeData _ _ (v,_)) | qr `covers` rangeOf t -> [v]
                              | otherwise             -> []
    Node l (NodeData _ _ v) r | qr `covers` rangeOf t -> [v]
                              | otherwise             -> msearch l <> msearch r
  where
    msearch t' | qr `intersects` rangeOf t' = search'' qr t'
               | otherwise                  = []


-- | Helper function to get the range of  a binary leaf tree
rangeOf              :: BinLeafTree (NodeData v r) (NodeData v' r) -> Range r
rangeOf (Leaf d)     = rangeOf' d
rangeOf (Node _ d _) = rangeOf' d

-- | Get the range of a node
rangeOf'                                :: NodeData v r -> Range r
rangeOf' (NodeData (Min mi) (Max ma) _) = ClosedRange mi ma


--------------------------------------------------------------------------------
-- * Updates

-- support inserting and deleting points, assuming that the x-coord already exists.


--------------------------------------------------------------------------------


createReportingTree :: Ord r => NonEmpty (r :+ [p]) -> RangeTree (Report p) (Report p) r
createReportingTree = createTree . fmap (&extra %~ Report)

report    :: (Ord r) => Range r -> RangeTree (Report p) q r -> [p]
report qr = reportList . search qr


----------------------------------------

newtype CountOf p = CountOf [p]
  deriving (Show,Eq,Ord,Functor,Foldable,Semigroup,Monoid)

instance Measured (Count p) (CountOf p) where
  measure (CountOf xs) = Count $ length xs

createCountingTree :: Ord r => NonEmpty (r :+ [p]) -> RangeTree (Count p) (CountOf p) r
createCountingTree = createTree . fmap (&extra %~ CountOf)

-- | Perform a counting query
--
count    :: Ord r => Range r -> RangeTree (Count p) q r -> Int
count qr = getCount . search qr
