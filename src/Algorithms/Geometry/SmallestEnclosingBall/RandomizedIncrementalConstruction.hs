{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Algorithms.Geometry.SmallestEnclosingBall.RandomizedIncrementalConstruction where

import           Algorithms.Geometry.SmallestEnclosingBall.Types

import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry
import           Data.Geometry.Ball
import qualified Data.List as L
import           Data.List.NonEmpty
import           Data.Maybe(fromMaybe)
import           Data.Monoid

import           System.Random
import           System.Random.Shuffle(shuffle)



-- | O(n) expected time algorithm to compute the smallest enclosing disk of a
-- set of points. we need at least two points.
-- implemented using randomized incremental construction
smallestEnclosingDisk           :: (Ord r, Fractional r, RandomGen g)
                                => g
                                -> Point 2 r :+ p -> Point 2 r :+ p -> [Point 2 r :+ p]
                                -> DiskResult p r
smallestEnclosingDisk g p q pts = let (p':q':pts') = shuffle g (p:q:pts)
                                  in smallestEnclosingDisk' p' q' pts'


-- | Smallest enclosing disk.
smallestEnclosingDisk'     :: (Ord r, Fractional r)
                           => Point 2 r :+ p -> Point 2 r :+ p -> [Point 2 r :+ p]
                           -> DiskResult p r
smallestEnclosingDisk' a b = foldr addPoint (initial a b) . L.tails
  where
    -- The epty case occurs only initially
    addPoint []      br   = br
    addPoint (p:pts) br@(DiskResult d _)
      | (p^.core) `inClosedBall` d = br
      | otherwise                  = smallestEnclosingDiskWithPoint p (a :| (b : pts))


-- | Smallest enclosing disk, given that p should be on it.
smallestEnclosingDiskWithPoint              :: (Ord r, Fractional r)
                                            => Point 2 r :+ p -> NonEmpty (Point 2 r :+ p)
                                            -> DiskResult p r
smallestEnclosingDiskWithPoint p (a :| pts) = foldr addPoint (initial p a) $ L.tails pts
  where
    addPoint []      br   = br
    addPoint (q:pts) br@(DiskResult d _)
      | (q^.core) `inClosedBall` d = br
      | otherwise                  = smallestEnclosingDiskWithPoints p q (a:pts)



-- | Smallest enclosing disk, given that p and q should be on it
smallestEnclosingDiskWithPoints     :: (Ord r, Fractional r)
                                    => Point 2 r :+ p -> Point 2 r :+ p -> [Point 2 r :+ p]
                                    -> DiskResult p r
smallestEnclosingDiskWithPoints p q = foldr addPoint (initial p q)
  where
    addPoint r br@(DiskResult d _)
      | (r^.core) `inClosedBall` d = br
      | otherwise                  = DiskResult (circle' r) (Three p q r)

    circle' r = fromMaybe degen $ disk (p^.core) (q^.core) (r^.core)
    degen = error "smallestEnclosingDisk: Unhandled degeneracy"
    -- TODO: handle degenerate case


-- | Constructs the initial 'DiskResult' from two points
initial     :: Fractional r => Point 2 r :+ p -> Point 2 r :+ p -> DiskResult p r
initial p q = DiskResult (fromDiameter (p^.core) (q^.core)) (Two p q)
