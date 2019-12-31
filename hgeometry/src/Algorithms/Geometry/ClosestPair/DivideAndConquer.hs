{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.ClosestPair.DivideAndConquer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Classical \(O(n\log n)\) time divide and conquer algorithm to compute the
-- closest pair among a set of \(n\) points in \(\mathbb{R}^2\).
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.ClosestPair.DivideAndConquer( closestPair
                                                       , CP
                                                       , CCP(..)
                                                       , mergePairs
                                                       )
where

import           Algorithms.DivideAndConquer
import           Control.Lens
import           Data.Ext
import           Data.Geometry.Point
import           Data.LSeq (LSeq)
import qualified Data.LSeq as LSeq
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Data.Semigroup.Foldable (toNonEmpty)
import           Data.UnBounded
import           Data.Util

--------------------------------------------------------------------------------

-- | Classical divide and conquer algorithm to compute the closest pair among
-- \(n\) points.
--
-- running time: \(O(n)\)
closestPair :: (Ord r, Num r) => LSeq 2 (Point 2 r :+ p) -> Two (Point 2 r :+ p)
closestPair = f . divideAndConquer1 mkCCP . toNonEmpty
            . LSeq.unstableSortBy (comparing (^.core))
  where
    mkCCP p = CCP (p :| []) Top
    f = \case
          CCP _ (ValT (SP cp _)) -> cp
          CCP _ Top              -> error "closestPair: absurd."


-- | the closest pair and its (squared) distance
type CP q r = Top (SP (Two q) r)

-- | Type used in the closest pair computation. The fields represent the points
-- ordered on increasing y-order and the closest pair (if we know it)
data CCP p r = CCP (NonEmpty (Point 2 r :+ p))
                   !(CP (Point 2 r :+ p) r)
            deriving (Show,Eq)

instance (Num r, Ord r) => Semigroup (CCP p r) where
  (CCP ptsl cpl) <> (CCP ptsr cpr) = CCP (mergeSortedBy cmp ptsl ptsr)
                                         (mergePairs (minBy getDist cpl cpr) ptsl ptsr)
    where
      -- compare on y first then on x
      cmp     :: Point 2 r :+ p -> Point 2 r :+ p -> Ordering
      cmp p q = comparing (^.core.yCoord) p q <> comparing (^.core.xCoord) p q



-- | Function that does the actual merging work
mergePairs            :: forall p r. (Ord r, Num r)
                      => CP (Point 2 r :+ p) r -- ^ current closest pair and its dist
                      -> NonEmpty (Point 2 r :+ p) -- ^ pts on the left
                      -> NonEmpty (Point 2 r :+ p) -- ^ pts on the right
                      -> CP (Point 2 r :+ p) r
mergePairs cp' ls' rs' = go cp' (NonEmpty.toList ls') (NonEmpty.toList rs')
  where
    -- scan through the points on the right in increasing order.
    go              :: CP (Point 2 r :+ p) r -> [Point 2 r :+ p] -> [Point 2 r :+ p]
                    -> CP (Point 2 r :+ p) r
    go cp _  []     = cp
    go cp ls (r:rs) = let ls'' = trim (getDist cp) ls r
                          cp'' = run cp r ls'' -- try to find a new closer pair with r.
                      in go cp'' ls'' rs   -- and then process the remaining points

-- | ditch the points on the left that are too low anyway
trim               :: (Ord r, Num r)
                   => Top r -> [Point 2 r :+ q] -> Point 2 r :+ a
                   -> [Point 2 r :+ q]
trim (ValT d) ls r = List.dropWhile (\l -> sqVertDist l r > d) ls
trim _        ls _ = ls

-- | the squared vertical distance (in case r lies above l) or 0 otherwise
sqVertDist    :: (Ord r, Num r) => Point 2 r :+ p -> Point 2 r :+ q -> r
sqVertDist l r = let d = 0 `max` (r^.core.yCoord - l^.core.yCoord) in d*d

-- | try and find a new closest pair with r. If we get to points that are too
-- far above r we stop (since none of those points will be closer to r anyway)
run          :: (Ord r, Num r)
             => CP (Point 2 r :+ p) r -> Point 2 r :+ p -> [Point 2 r :+ p]
             -> CP (Point 2 r :+ p) r
run cp'' r ls =
      runWhile cp'' ls
               (\cp l -> (ValT $ sqVertDist r l) < getDist cp) -- r and l inverted
                                                               -- by design
               (\cp l -> minBy getDist cp (ValT $ SP (Two l r) (dist l r)))
  where
    dist (p :+ _) (q :+ _) = squaredEuclideanDist p q


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
getDist :: CP a r -> Top r
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
