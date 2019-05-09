module Data.Geometry.Algorithms.RedBlueLineSegmentIntersection where

import qualified Data.Foldable as F
import           Data.Monoid

-- using ``A Simple Trapezoid Sweep Algorithm for Reporting Red/Blue Segment Intersections (1994)''
-- by Timothy M. Chan
-- http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.44.4227

--------------------------------------------------------------------------------

type Size = Int

-- | Size balanced trees, similar to Data.Set, but with the function on which
-- we order stored explicitly.
--
--
-- Balancing weight-balanced trees
-- YOICHI HIRAI
-- The University of Tokyo, JSPS Research Fellow (e-mail: yh@lyon.is.s.u-tokyo.ac.jp)
-- KAZUHIKO YAMAMOTO
data SBTree a = Tip (a -> a -> Ordering)
              | Bin (a -> a -> Ordering) !Size (SBTree a) a (SBTree a)

instance F.Foldable SBTree where
  foldMap _ (Tip _)         = mempty
  foldMap f (Bin _ _ l x r) = F.foldMap f l <> f x <> F.foldMap f r


----------------------------------------

-- | Get the compare function
compareBy                   :: SBTree a -> (a -> a -> Ordering)
compareBy (Tip cmp)         = cmp
compareBy (Bin cmp _ _ _ _) = cmp


setCompareBy                     :: (a -> a -> Ordering) -> SBTree a -> SBTree a
setCompareBy cmp (Tip _)         = Tip cmp
setCompareBy cmp (Bin _ s l x r) = Bin cmp s l x r

size                 :: SBTree a -> Size
size (Bin _ s _ _ _) = s
size _               = 0


----------------------------------------

delta, gamma :: Int
delta = 3
gamma = 2


isBalanced :: SBTree a -> SBTree b -> Bool
isBalanced l r = let sl = size l
                     sr = size r
                 in sl + sr <= 1 || delta * sl >= sr

isSingle     :: SBTree a -> SBTree b -> Bool
isSingle l r = size l < gamma * size r

----------------------------------------
-- | Smart constructors

empty     :: (a -> a -> Ordering) -> SBTree a
empty cmp = Tip cmp

bin       :: SBTree a -> a -> SBTree a -> SBTree a
bin l x r = Bin (compareBy l) (1 + size l + size r) l x r

----------------------------------------
-- | Core operations
