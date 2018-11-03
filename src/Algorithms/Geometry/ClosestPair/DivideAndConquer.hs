{-# LANGUAGE ScopedTypeVariables #-}
module Algorithms.Geometry.ClosestPair.DivideAndConquer where

import           Control.Lens
import           Data.BinaryTree
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Point
import           Data.LSeq (LSeq)
import qualified Data.LSeq as LSeq
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Data.Semigroup.Foldable(foldMap1)
import           Data.UnBounded
import           Data.Util


--------------------------------------------------------------------------------

type CP q r = Top (SP (Two q) r) -- ^ the closest pair and its (squared) distance

data CCP p r = CCP (NonEmpty (Point 2 r :+ p))   -- ^ pts ordered on increasing y-order
                   (CP (Point 2 r :+ p) r) -- ^ the closest pair (if we know it yet)
            deriving (Show,Eq)

instance (Num r, Ord r) => Semigroup (CCP p r) where
  (CCP ptsl cpl) <> (CCP ptsr cpr) = CCP (mergeSortedBy cmp ptsl ptsr)
                                         (mergePairs (minBy getDist cpl cpr) ptsl ptsr)
    where
      -- compare on y first then on x
      cmp     :: Point 2 r :+ p -> Point 2 r :+ p -> Ordering
      cmp p q = comparing (^.core.yCoord) p q <> comparing (^.core.xCoord) p q

-- | Classical divide and conquer algorithm to compute the closest pair among
-- \(n\) points.
--
-- running time: \(O(n)\)
closestPair :: ( Ord r, Num r) => LSeq 2 (Point 2 r :+ p) -> Two (Point 2 r :+ p)
closestPair = f . foldMap1 mkCCP . asBalancedBinLeafTree . LSeq.toNonEmpty
            . LSeq.unstableSortBy (comparing (^.core))
  where
    mkCCP (Elem p) = CCP (p :| []) Top
    f = \case
          CCP _ (ValT (SP cp _)) -> cp
          CCP _ Top              -> error "closestPair: absurd."


-- | Given an ordering and two nonempty sequences ordered according to that
-- ordering, merge them
mergeSortedBy           :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a -> NonEmpty a
mergeSortedBy cmp ls rs = NonEmpty.fromList $ go (F.toList ls) (F.toList rs)
  where
    go []         ys     = ys
    go xs         []     = xs
    go xs@(x:xs') ys@(y:ys') = case x `cmp` y of
                                 LT -> x : go xs' ys
                                 EQ -> x : go xs' ys
                                 GT -> y : go xs  ys'


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

    -- ditch the points on the left that are too low anyway
    trim               :: Top r -> [Point 2 r :+ q] -> Point 2 r :+ a
                       -> [Point 2 r :+ q]
    trim (ValT d) ls r = List.dropWhile (\l -> l^.core.yCoord < r^.core.yCoord - d) ls
    trim _        ls _ = ls

    -- try and find a new closest pair with r. If we get to points that are too far above
    -- r we stop (since none of those points will be closer to r anyway)
    run          :: CP (Point 2 r :+ p) r -> Point 2 r :+ p -> [Point 2 r :+ p]
                 -> CP (Point 2 r :+ p) r
    run cp'' r ls =
      runWhile cp'' ls
               (\cp l -> ValT (l^.core.yCoord - r^.core.yCoord) < (getDist cp))
               (\cp l -> minBy getDist cp (ValT $ SP (Two l r) (dist l r)))

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

getDist :: CP a r -> Top r
getDist = fmap (view _2)
