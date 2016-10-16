{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.SegmentTree( NodeData(..), splitPoint, range, assoc
                                , LeafData(..), atomicRange, leafAssoc

                                , SegmentTree(..), unSegmentTree
                                , Assoc(..)

                                , createTree, fromIntervals
                                , insert, delete

                                , search, stab

                                , I(..), fromIntervals'

                                , Count(..)
                                )

       where

import Data.Semigroup
import Control.Lens
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import Data.Ext
import Data.BinaryTree
import Data.Range
import Data.Geometry.Interval
import Data.Geometry.Properties
import Data.Geometry.IntervalTree(IntervalLike(..))

import Debug.Trace

--------------------------------------------------------------------------------

-- | Internal nodes store a split point, the range, and an associated data structure
data NodeData v r = NodeData { _splitPoint :: r
                             , _range      :: Range r
                             , _assoc      :: v
                             } deriving (Show,Eq,Functor)
makeLenses ''NodeData

-- | Leaf nodes store an atomic range, and an associated data structure
data LeafData v r = LeafData  { _atomicRange :: Range r
                              , _leafAssoc   :: v
                              } deriving (Show,Eq,Functor)
makeLenses ''LeafData

--------------------------------------------------------------------------------

-- | Segemnet tree on a Fixed set of endpoints
newtype SegmentTree v r =
  SegmentTree { _unSegmentTree :: BinLeafTree (NodeData v r) (LeafData v r) }
    deriving (Show,Eq)
makeLenses ''SegmentTree


-- | Given a sorted list of endpoints, construct a segment tree
--
-- $O(n)$ time
createTree       :: NonEmpty r -> v -> SegmentTree v r
createTree pts v = SegmentTree . foldUpData f g . fmap _unElem
                 . asBalancedBinLeafTree $ ranges
  where
    f l _ r = let m  = l^.range.upper.unEndPoint
                  ll = l^.range.lower
                  rr = r^.range.upper
              in NodeData m (Range ll rr) v
    g (LeafData r v') = NodeData (r^.upper.unEndPoint) r v'

    ranges = NonEmpty.fromList $ zipWith (\a b -> LeafData (ClosedRange a b) v)
                (NonEmpty.toList pts) (NonEmpty.tail pts)

-- | Build a SegmentTree
--
-- $O(n \log n)$
fromIntervals      :: (Ord r, Eq p, Assoc v i, IntervalLike i, Monoid v, NumType i ~ r)
                   => (Interval p r -> i)
                   -> NonEmpty (Interval p r) -> SegmentTree v r
fromIntervals f is = foldr (insert . f) (createTree pts mempty) is
  where
    endPoints i@(toRange -> Range' a b) = [a,b]
    pts = NonEmpty.sort . NonEmpty.fromList . concatMap endPoints $ is


--------------------------------------------------------------------------------
-- * Searching

-- | Search for all intervals intersecting x
--
-- $O(\log n + k)$ where k is the output size
search   :: (Ord r, Monoid v) => r -> SegmentTree v r -> v
search x = mconcat . stab x

-- | Returns the associated values of the nodes on the search path to x
--
-- $O(\log n)$
stab                   :: Ord r => r -> SegmentTree v r -> [v]
stab x (SegmentTree t) = stabRoot t
  where
    stabRoot (Leaf (LeafData rr v))
      | x `inRange` rr = [v]
      | otherwise      = []
    stabRoot (Node l (NodeData m rr v) r) = case (x `inRange` rr, x <= m) of
      (False,_)   -> []
      (True,True) -> v : stab' l
      _           -> v : stab' r

    stab' (Leaf (LeafData rr v))      | x `inRange` rr = [v]
                                      | otherwise      = []
    stab' (Node l (NodeData m _ v) r) | x == m         = v : stab' l ++ stab' r
                                      | x < m          = v : stab' l
                                      | otherwise      = v : stab' r


--------------------------------------------------------------------------------
-- * Inserting intervals

-- | Class for associcated data structures
class Measured v i => Assoc v i where
  insertAssoc :: i -> v -> v
  deleteAssoc :: i -> v -> v


-- | Pre: the interval should have one of the endpoints on which the tree is built.
insert                   :: (Assoc v i, NumType i ~ r, Ord r, IntervalLike i)
                         => i -> SegmentTree v r -> SegmentTree v r
insert i (SegmentTree t) = SegmentTree $ insert' False t
  where
    ri@(Range' a b) = toRange i

    newND coversParent coversMe nd
      | not coversParent && coversMe = nd&assoc %~ insertAssoc i
      | otherwise                    = nd

    newLD coversParent ld@(LeafData rr v)
      | not coversParent && (ri `covers` rr) = LeafData rr (insertAssoc i v)
      | otherwise                            = ld


    -- pre: b > rr^.upper
    insertL coversParent (Leaf r) = Leaf $ newLD coversParent r
    insertL coversParent (Node l nd@(NodeData m rr _) r)
      -- | traceShow ("L:" :: String,coversParent,nd) False = undefined
      | a <= m    = let cp  = ri `covers` rr
                    in Node (insertL cp l) (newND coversParent cp nd) (insert'' cp r)
      | otherwise = Node l nd (insertL False r)
                    -- in this case the interval does not cover me

    -- insert in the current node
    insert'' coversParent (Leaf r) = Leaf $ newLD coversParent r
    insert'' coversParent (Node l nd r) =
      Node l (newND coversParent (ri `covers` (nd^.range)) nd) r

    -- pre: a < rr^.lower
    insertR coversParent (Leaf r) = Leaf $ newLD coversParent r
    insertR coversParent (Node l nd@(NodeData m rr _) r)
      -- | traceShow ("R:" :: String,nd) False = undefined
      | m <= b    = let cp  = ri `covers` rr
                    in Node (insert'' cp l) (newND coversParent cp nd) (insertR cp r)
      | otherwise = Node (insertR False l) nd r


    insert' coversParent (Leaf r) = Leaf $ newLD coversParent r
    insert' coversParent (Node l nd@(NodeData m rr _) r)
      -- | traceShow ("ND:" :: String,nd) False = undefined
      | b < m     = Node (insert' False l) nd r
      | m < a     = Node l nd (insertR False r)
      | otherwise = let cp  = ri `covers` rr
                        l'  = if a <= m then insertL cp l else l
                        r'  = if m <= b then insertR cp r else r
                    in Node l' (newND coversParent cp nd) r'

-- | Delete an interval from the tree
--
-- pre: The segment is in the tree!
delete :: (Assoc v i, NumType i ~ r, Ord r, IntervalLike i)
          => i -> SegmentTree v r -> SegmentTree v r
delete i (SegmentTree t) = SegmentTree $ delete' t
  where
    ri@(Range' a b) = toRange i

    delete' (Leaf ld)     = Leaf $ ld&leafAssoc %~ deleteAssoc i
    delete' (Node l nd@(NodeData m rr _) r)
      | b < m = Node (delete' l) nd r
      | m < a = Node l nd (delete' r)
      | otherwise = Node (deleteL l) (nd&assoc %~ deleteAssoc i) (deleteR r)

    delete'' (Leaf ld)     = Leaf $ ld&leafAssoc %~ deleteAssoc i
    delete'' (Node l nd r) = Node l (nd&assoc %~ deleteAssoc i) r

    deleteL (Leaf ld)     = Leaf $ ld&leafAssoc %~ deleteAssoc i
    deleteL (Node l nd@(_splitPoint -> m) r)
      | a <= m    = Node (deleteL l) (nd&assoc %~ deleteAssoc i) (delete'' r)
      | otherwise = Node l nd (deleteL r)

    deleteR (Leaf ld)     = Leaf $ ld&leafAssoc %~ deleteAssoc i
    deleteR (Node l nd@(_splitPoint -> m) r)
      | m <= b    = Node (delete'' l) (nd&assoc %~ deleteAssoc i) (deleteR r)
      | otherwise = Node (deleteR l) nd r


--------------------------------------------------------------------------------
-- * Listing the intervals stabbed

-- | Interval
newtype I a = I { _unI :: a} deriving (Show,Read,Eq,Ord)

type instance NumType (I a) = NumType a

instance Measured [I a] (I a) where
  measure = (:[])

instance Eq a => Assoc [I a] (I a) where
  insertAssoc = (:)
  deleteAssoc = List.delete

-- instance Measured [Interval p r] (Interval p r) where
--   measure = (:[])

-- instance (Eq p, Eq r) => Assoc [Interval p r] (Interval p r) where
--   insertAssoc = (:)
--   deleteAssoc = List.delete


instance IntervalLike a => IntervalLike (I a) where
  toRange = toRange . _unI


fromIntervals' :: (Eq p, Ord r)
               => NonEmpty (Interval p r) -> SegmentTree [I (Interval p r)] r
fromIntervals' = fromIntervals I


--------------------------------------------------------------------------------
-- * Counting the number of segments intersected

newtype Count = Count { getCount :: Int} deriving (Show,Eq,Ord,Num,Integral,Enum,Real)

newtype C a = C { _unC :: a} deriving (Show,Read,Eq,Ord)

instance Semigroup Count where
  a <> b = Count $ getCount a + getCount b

instance Measured Count (C i) where
  measure _ = 1

instance Assoc Count (C i) where
  insertAssoc _ v = v + 1
  deleteAssoc _ v = v - 1


--------------------------------------------------------------------------------
-- * Testing stuff

test = fromIntervals' . NonEmpty.fromList $ [ closedInterval 0 10
                                            , closedInterval 5 15
                                            , closedInterval 1 4
                                            , closedInterval 3 9
                                            ]

closedInterval a b = ClosedInterval (ext a) (ext b)

showT :: (Show r, Show v) => SegmentTree v r -> String
showT = drawTree . _unSegmentTree


test' :: (Show r, Num r, Ord r, Enum r) => SegmentTree [I (Interval () r)] r
test' = insert (I $ closedInterval 6 14) $ createTree (NonEmpty.fromList [2,4..20]) []
