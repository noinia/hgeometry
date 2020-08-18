{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.SegmentTree.Generic
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Implementation of SegmentTrees
--
--------------------------------------------------------------------------------
module Data.Geometry.SegmentTree.Generic( NodeData(..), splitPoint, range, assoc
                                        , LeafData(..), atomicRange, leafAssoc

                                        , SegmentTree(..), unSegmentTree
                                        , Assoc(..)

                                        , createTree, fromIntervals
                                        , insert, delete

                                        , search, stab

                                        , I(..), fromIntervals'

                                        , Count(..)
                                        ) where


import           Control.DeepSeq
import           Control.Lens
import           Data.BinaryTree
import           Data.Geometry.Interval
import           Data.Geometry.IntervalTree (IntervalLike(..))
import           Data.Geometry.Properties
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Measured.Class
import           Data.Measured.Size
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Internal nodes store a split point, the range, and an associated data structure
data NodeData v r = NodeData { _splitPoint :: !(EndPoint r)
                             , _range      :: !(Range r)
                             , _assoc      :: !v
                             } deriving (Show,Eq,Functor,Generic)
makeLenses ''NodeData
instance (NFData v, NFData r) => NFData (NodeData v r)

-- | We store atomic ranges a bit more efficiently.
data AtomicRange r = Singleton !r | AtomicRange deriving (Show,Eq,Functor,Generic)
instance NFData r => NFData (AtomicRange r)

-- | Leaf nodes store an atomic range, and an associated data structure.
data LeafData v r = LeafData  { _atomicRange :: !(AtomicRange r)
                              , _leafAssoc   :: !v
                              } deriving (Show,Eq,Functor,Generic)
makeLenses ''LeafData
instance (NFData v, NFData r) => NFData (LeafData v r)

--------------------------------------------------------------------------------

-- | Segment tree on a Fixed set of endpoints
newtype SegmentTree v r =
  SegmentTree { _unSegmentTree :: BinLeafTree (NodeData v r) (LeafData v r) }
    deriving (Show,Eq,Generic,NFData)
makeLenses ''SegmentTree


-- rangeOf :: BinLeafTree (NodeData v r) (LeafData v r) -> Range (UnBounded r)
-- rangeOf (Node _ x _) = Val <$> x^.range
-- rangeOf (Leaf x)     = case x^.atomicRange of
--                         Singleton r -> ClsoedRange (Val r)     (Val r)
--                         AtomicRange -> OpenRange   MinInfinity MaxInfinity


data BuildLeaf a = LeafSingleton !a | LeafRange !a !a deriving (Show,Eq)

-- | Given a sorted list of endpoints, without duplicates, construct a segment tree
--
--
-- \(O(n)\) time
createTree                      :: NonEmpty r -> v -> SegmentTree v r
-- createTree (r NonEmpty.:| []) v = SegmentTree . Leaf $ LeafData (Singleton r) v
createTree pts                v = SegmentTree . fmap h . foldUpData f g . fmap _unElem
                                . asBalancedBinLeafTree $ ranges
  where
    h (LeafSingleton r) = LeafData (Singleton r) v
    h (LeafRange   _ _) = LeafData AtomicRange   v

    f l _ r = let m  = l^.range.upper
                  ll = l^.range.lower
                  rr = r^.range.upper
              in NodeData m (Range ll rr) v
    -- | Singletons map to closed singleton ranges, Ranges map to open ranges
    g (LeafSingleton r) = NodeData (Closed r) (ClosedRange r r) v
    g (LeafRange   s r) = NodeData (Open r)   (OpenRange   s r) v


    ranges  = interleave (fmap LeafSingleton pts) ranges'
    ranges' = zipWith LeafRange (NonEmpty.toList pts) (NonEmpty.tail pts)




-- | Interleaves the two lists
--
-- >>> interleave (NonEmpty.fromList ["0","1","2"]) ["01","12"]
-- "0" :| ["01","1","12","2"]
interleave                       :: NonEmpty a -> [a] -> NonEmpty a
interleave (x NonEmpty.:| xs) ys = x NonEmpty.:| concat (zipWith (\a b -> [a,b]) ys xs)


-- | Build a SegmentTree
--
-- \(O(n \log n)\)
fromIntervals      :: (Ord r, Eq p, Assoc v i, IntervalLike i, Monoid v, NumType i ~ r)
                   => (Interval p r -> i)
                   -> NonEmpty (Interval p r) -> SegmentTree v r
fromIntervals f is = foldr (insert . f) (createTree pts mempty) is
  where
    endPoints (asRange -> Range' a b) = [a,b]
    endPoints _ = error "Unreachable, but cannot prove it in Haskell"
    pts = nub' . NonEmpty.sort . NonEmpty.fromList . concatMap endPoints $ is
    nub' = fmap NonEmpty.head . NonEmpty.group1

-- -- | lists all intervals
-- toList :: SegmentTree v r -> [i]
-- toList = undefined

--------------------------------------------------------------------------------
-- * Searching

-- | Search for all intervals intersecting x
--
-- \(O(\log n + k)\) where \(k\) is the output size
search   :: (Ord r, Monoid v) => r -> SegmentTree v r -> v
search x = mconcat . stab x



inAtomicRange                   :: Eq r => r -> AtomicRange r -> Bool
x `inAtomicRange` (Singleton r) = x == r
_ `inAtomicRange` AtomicRange   = True


-- | Returns the associated values of the nodes on the search path to x
--
-- \(O(\log n)\)
stab                   :: Ord r => r -> SegmentTree v r -> [v]
stab x (SegmentTree t) = stabRoot t
  where
    stabRoot (Leaf (LeafData rr v))
      | x `inAtomicRange` rr = [v]
      | otherwise            = []
    stabRoot (Node l (NodeData m rr v) r) = case (x `inRange` rr, Closed x <= m) of
      (False,_)   -> []
      (True,True) -> v : stab' l
      _           -> v : stab' r

    stab' (Leaf (LeafData rr v))      | x `inAtomicRange` rr = [v]
                                      | otherwise            = []
    stab' (Node l (NodeData m _ v) r) | Closed x <= m        = v : stab' l
                                      | otherwise            = v : stab' r


--------------------------------------------------------------------------------
-- * Inserting intervals

-- | Class for associcated data structures
class Measured v i => Assoc v i where
  insertAssoc :: i -> v -> v
  deleteAssoc :: i -> v -> v

-- | Gets the range associated with this node
getRange  :: BinLeafTree (NodeData v r) (LeafData t r) -> Maybe (Range r)
getRange (Leaf (LeafData (Singleton r) _)) = Just $ Range (Closed r) (Closed r)
getRange (Leaf _)                          = Nothing
getRange (Node _ nd _)                     = Just $ nd^.range

coversAtomic                      :: Ord r
                                  => Range r -> Range r -> AtomicRange r -> Bool
coversAtomic ri _   (Singleton r) = r `inRange` ri
coversAtomic ri inR AtomicRange   = ri `covers` inR

-- | Pre: the interval should have one of the endpoints on which the tree is built.
insert                   :: (Assoc v i, NumType i ~ r, Ord r, IntervalLike i)
                         => i -> SegmentTree v r -> SegmentTree v r
insert i (SegmentTree t) = SegmentTree $ insertRoot t
  where
    ri@(Range a b) = asRange i
    insertRoot t' = maybe t' (flip insert' t') $ getRange t'

    insert' inR         lf@(Leaf nd@(LeafData rr _))
      | coversAtomic ri inR rr = Leaf $ nd&leafAssoc %~ insertAssoc i
      | otherwise              = lf
    insert' (Range c d) (Node l nd@(NodeData m rr _) r)
      | ri `covers` rr       = Node l (nd&assoc %~ insertAssoc i) r
      | otherwise            = Node l' nd r'
      where
           -- check if the range intersects the range of the left subtree
        l'  = if a <= m then insert' (Range c          m) l else l
        r'  = if m < b  then insert' (Range (toOpen m) d) r else r

    toOpen = Open . view unEndPoint

-- | Delete an interval from the tree
--
-- pre: The segment is in the tree!
delete :: (Assoc v i, NumType i ~ r, Ord r, IntervalLike i)
          => i -> SegmentTree v r -> SegmentTree v r
delete i (SegmentTree t) = SegmentTree $ delete' t
  where
    (Range _ b) = asRange i

    delete' (Leaf ld) = Leaf $ ld&leafAssoc %~ deleteAssoc i
    delete' (Node l nd@(_splitPoint -> m) r)
      | b <= m    = Node (delete' l) (nd&assoc %~ deleteAssoc i) r
      | otherwise = Node l           (nd&assoc %~ deleteAssoc i) (delete' r)

    -- delete'' (Leaf ld)     = Leaf $ ld&leafAssoc %~ deleteAssoc i
    -- delete'' (Node l nd r) = Node l (nd&assoc %~ deleteAssoc i) r

    -- deleteL (Leaf ld)     = Leaf $ ld&leafAssoc %~ deleteAssoc i
    -- deleteL (Node l nd@(_splitPoint -> m) r)
    --   | a <= m    = Node (deleteL l) (nd&assoc %~ deleteAssoc i) (delete'' r)
    --   | otherwise = Node l nd (deleteL r)

    -- deleteR (Leaf ld)     = Leaf $ ld&leafAssoc %~ deleteAssoc i
    -- deleteR (Node l nd@(_splitPoint -> m) r)
    --   | m <= b    = Node (delete'' l) (nd&assoc %~ deleteAssoc i) (deleteR r)
    --   | otherwise = Node (deleteR l) nd r


--------------------------------------------------------------------------------
-- * Listing the intervals stabbed

-- | Interval
newtype I a = I { _unI :: a} deriving (Show,Read,Eq,Ord,Generic,NFData)

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
  asRange = asRange . _unI


fromIntervals' :: (Eq p, Ord r)
               => NonEmpty (Interval p r) -> SegmentTree [I (Interval p r)] r
fromIntervals' = fromIntervals I


--------------------------------------------------------------------------------
-- * Counting the number of segments intersected

newtype Count = Count { getCount :: Word }
              deriving (Show,Eq,Ord,Num,Integral,Enum,Real,Generic,NFData)

newtype C a = C { _unC :: a} deriving (Show,Read,Eq,Ord,Generic,NFData)

instance Semigroup Count where
  a <> b = Count $ getCount a + getCount b
instance Monoid Count where
  mempty = 0
  mappend = (<>)

instance Measured Count (C i) where
  measure _ = 1

instance Assoc Count (C i) where
  insertAssoc _ v = v + 1
  deleteAssoc _ v = v - 1


--------------------------------------------------------------------------------
-- * Testing stuff

-- test'' = fromIntervals' . NonEmpty.fromList $ test
-- test = [Interval (Closed (238 :+ ())) (Open (309 :+ ())), Interval (Closed (175 :+ ())) (Closed (269 :+ ())),Interval (Closed (255 :+ ())) (Open (867 :+ ())),Interval (Open (236 :+ ())) (Closed (863 :+ ())),Interval (Open (150 :+ ())) (Closed (161 :+ ())),Interval (Closed (35 :+ ())) (Closed (77 :+ ()))]


-- -- q =        [78]

-- -- test = fromIntervals' . NonEmpty.fromList $ [ closedInterval 0 10
-- --                                             , closedInterval 5 15
-- --                                             , closedInterval 1 4
-- --                                             , closedInterval 3 9
-- --                                             ]
-- tst = fromIntervals' . NonEmpty.fromList $ [ closedInterval 1 6
--                                            , closedInterval 2 6
--                                            -- , Interval (Closed $ ext 0) (Open $ ext 1)
--                                            ]



-- closedInterval a b = ClosedInterval (ext a) (ext b)

-- showT :: (Show r, Show v) => SegmentTree v r -> String
-- showT = drawTree . _unSegmentTree


-- test' :: (Show r, Num r, Ord r, Enum r) => SegmentTree [I (Interval () r)] r
-- test' = insert (I $ closedInterval 6 14) $ createTree (NonEmpty.fromList [2,4..20]) []
