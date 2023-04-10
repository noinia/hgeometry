{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.ClosestPair.DivideAndConquer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Classical \(O(n\log n)\) time divide and conquer algorithm to compute the
-- closest pair among a set of \(n\) points in \(\mathbb{R}^2\).
--
--------------------------------------------------------------------------------
module HGeometry.ClosestPair.DivideAndConquer
  ( closestPair
  , CP
  , CCP(..)
  , mergePairs
  ) where

import           Control.Lens
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Data.Semigroup.Foldable
import qualified Data.Vector.NonEmpty as NonEmptyVector
import           HGeometry.Algorithms.DivideAndConquer
import           HGeometry.Combinatorial.Util
import           HGeometry.Foldable.Sort
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Unbounded
import           HGeometry.Vector
import           HGeometry.Vector.NonEmpty.Util ()

--------------------------------------------------------------------------------

-- | Classical divide and conquer algorithm to compute the closest pair among
-- \(n\) points.
--
-- pre: input contains at least two points
--
-- running time: \(O(n \log n)\)
closestPair :: ( Ord r, Num r, Point_ point 2 r, Foldable1 f)
            => f point -> Vector 2 point
closestPair = f
            . divideAndConquer1 mkCCP
            . NonEmptyVector.unsafeFromVector
            . sortOnCheap (view asPoint)
  where
    mkCCP p = CCP (p :| []) Top
    f = \case
          CCP _ (ValT (SP (Two p q) _)) -> Vector2 p q
          CCP _ Top                     -> error "closestPair: absurd."


-- | the closest pair and its (squared) distance
type CP point = Top (SP (Two point) (NumType point))

-- | Type used in the closest pair computation. The fields represent the points
-- ordered on increasing y-order and the closest pair (if we know it)
data CCP point = CCP (NonEmpty point) !(CP point)
               -- deriving (Show,Eq)

instance (Num r, Ord r, Point_ point 2 r) => Semigroup (CCP point) where
  (CCP ptsl cpl) <> (CCP ptsr cpr) = CCP (mergeSortedBy cmp ptsl ptsr)
                                         (mergePairs (minBy getDist cpl cpr) ptsl ptsr)
    where
      -- compare on y first then on x
      cmp     :: point -> point -> Ordering
      cmp p q = comparing (^.yCoord) p q <> comparing (^.xCoord) p q

-- | Function that does the actual merging work
mergePairs            :: forall point r. (Ord r, Num r, Point_ point 2 r)
                      => CP point -- ^ current closest pair and its dist
                      -> NonEmpty point -- ^ pts on the left
                      -> NonEmpty point -- ^ pts on the right
                      -> CP point
mergePairs cp' ls' rs' = go cp' (NonEmpty.toList ls') (NonEmpty.toList rs')
  where
    -- scan through the points on the right in increasing order.
    go              :: CP point -> [point] -> [point]
                    -> CP point
    go cp _  []     = cp
    go cp ls (r:rs) = let ls'' = trim (getDist cp) ls r
                          cp'' = run cp r ls'' -- try to find a new closer pair with r.
                      in go cp'' ls'' rs   -- and then process the remaining points

-- | ditch the points on the left that are too low anyway
trim               :: (Ord r, Num r, Point_ point 2 r)
                   => Top r -> [point] -> point
                   -> [point]
trim (ValT d) ls r = List.dropWhile (\l -> sqVertDist l r > d) ls
trim _        ls _ = ls

-- | the squared vertical distance (in case r lies above l) or 0 otherwise
sqVertDist    :: (Ord r, Num r, Point_ point 2 r) => point -> point -> r
sqVertDist l r = let d = 0 `max` (r^.yCoord - l^.yCoord) in d*d

-- | try and find a new closest pair with r. If we get to points that are too
-- far above r we stop (since none of those points will be closer to r anyway)
run          :: (Ord r, Num r, Point_ point 2 r)
             => CP point -> point -> [point]
             -> CP point
run cp'' r ls =
      runWhile cp'' ls
               (\cp l -> ValT (sqVertDist r l) < getDist cp) -- r and l inverted
                                                             -- by design
               (\cp l -> minBy getDist cp (ValT $ SP (Two l r) (squaredEuclideanDist l r)))


-- | Given some function that decides when to keep things while maintaining some state.
runWhile           :: s -> [a] -> (s -> a -> Bool) -> (s -> a -> s) -> s
runWhile s' ys p f = go s' ys
  where
    go s []                 = s
    go s (x:xs) | p s x     = go (f s x) xs  -- continue with new state
                | otherwise = s -- stop, return the current state

-- | returns the minimum element according to some function.
minBy                   :: Ord b => (a -> b) -> a -> a -> a
minBy f a b | f a < f b = a
            | otherwise = b

-- | Get the distance of a (candidate) closest pair
getDist :: CP point -> Top (NumType point)
getDist = fmap (view _2)





-- test4 = [Point2 (479109173120836 % 8353334321025) (5100576283797 % 96072829279) :+ ()
--         ,Point2 (58405408826671 % 1010204299645) (416491493323834 % 7859181827347) :+ ()
--         ,Point2 (497723773632392 % 8797511756605) (484251118551575 % 9452820868018) :+ ()
--         ,Point2 (71823625388220 % 1256943286753) (211467894699900 % 3952412568913) :+ ()
--         ]


-- myTree = asBalancedBinLeafTree . LSeq.toNonEmpty . LSeq.promise . LSeq.unstableSortBy (comparing (^.core)). LSeq.fromList $ test4
-- myTree2 = let mkCCP (Elem p) = CCP (p :| []) Top in mkCCP <$> myTree



-- ans2p = Point2 (479109173120836 % 8353334321025) (5100576283797 % 96072829279)
-- ans2q = Point2 (71823625388220 % 1256943286753) (211467894699900 % 3952412568913)



-- temp =Two (test4 !! 1) (test4 !! 0)
-- tempX = ValT (SP temp $ squaredEuclideanDist (temp^._1.core) (temp^._2.core))
