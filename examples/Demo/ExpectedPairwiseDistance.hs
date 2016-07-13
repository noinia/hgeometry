module Demo.ExpectedPairwiseDistance where

import           Algorithms.Geometry.WellSeparatedPairDecomposition.Types
import           Algorithms.Geometry.WellSeparatedPairDecomposition.WSPD
import           Control.Lens
import           Data.BinaryTree
import           Data.Ext
import           Data.Geometry
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup



-- | Evaluates the formula: $\sum_{p,q \in pts} \|pq\|*Prb[sample has size k and contains p and q]$ which solves to $\frac{{n-2 \choose k-2}}{{n \choose k}} \sum_{p,q} \|pq\|$
expectedPairwiseDistance       :: (Floating r, Arity d) => Int -> [Point d r :+ p] -> r
expectedPairwiseDistance k pts = makeExpected k pts pairwiseDist


-- approxExpectedPairwiseDistance ::


makeExpected         :: (Fractional r, Foldable t) => Int -> t a -> (t a -> r) -> r
makeExpected k pts f = prb * f pts
  where
    n   = length pts
    prb = ((n - 2) `choose` (k - 2)) / (n `choose` k)






choose       :: (Integral a, Num b) => a -> a -> b
n `choose` k = fromIntegral $ fac n' `div` (fac (n'-k') * fac k')
  where
    n' :: Integer
    n' = fromIntegral n
    k' :: Integer
    k' = fromIntegral k

    fac z = product [1..z]


-- approxPairwiseDistance         :: (Floating r, Arity d) => r -> [Point d r :+ p] -> r
-- approxPairwiseDistance _   []  = 0
-- approxPairwiseDistance eps pts =
--     sum [ euclideanDist (p^.core) (q^.core) | p <- pts, q <- pts]
--   where
--     n   = length pts

newtype F a = F a
instance Semigroup (F a) where
  l <> _ = l


withSizes :: SplitTree d p r a -> SplitTree d p r (Sized (F (Point d r :+ p)))
withSizes = foldUp f Leaf
  where
    f l (NodeData j b _) r = let nd = (measure l)^.nodeData <> (measure r)^.nodeData
                             in Node l (NodeData j b nd) r



-- | Sum of the pairwise distances
pairwiseDist     :: (Floating r, Arity d) => [Point d r :+ p] -> r
pairwiseDist pts = sum [ euclideanDist (p^.core) (q^.core) | p <- pts, q <- pts]
