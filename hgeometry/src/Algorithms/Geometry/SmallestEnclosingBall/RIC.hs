{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE TemplateHaskell  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.SmallestEnclosingBall.RIC
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- An randomized algorithm to compute the smallest enclosing disk of a set of
-- \(n\) points in \(\mathbb{R}^2\). The expected running time is \(O(n)\).
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.SmallestEnclosingBall.RIC(
    smallestEnclosingDisk'
  , smallestEnclosingDisk
  , smallestEnclosingDiskWithPoint
  , smallestEnclosingDiskWithPoints
  ) where

import           Algorithms.Geometry.SmallestEnclosingBall.Types
import           Control.Lens
import           Control.Monad.Random.Class
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry
import           Data.Geometry.Ball
import qualified Data.List as List
import           Data.List.NonEmpty(NonEmpty(..))
import           Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import           Data.Ord (comparing)
import           System.Random.Shuffle (shuffle)

import Debug.Trace

--------------------------------------------------------------------------------

-- | Compute the smallest enclosing disk of a set of points,
-- implemented using randomized incremental construction.
--
-- pre: the input has at least two points.
--
-- running time: expected \(O(n)\) time, where \(n\) is the number of input points.
smallestEnclosingDisk           :: (Ord r, Fractional r, MonadRandom m
                                               -- , Show r, Show p
                                   )
                                => [Point 2 r :+ p]
                                -> m (DiskResult p r)

smallestEnclosingDisk pts@(_:_:_) = ((\(p:q:pts') -> smallestEnclosingDisk' p q pts')
                                    . F.toList) <$> shuffle pts
smallestEnclosingDisk _           = error "smallestEnclosingDisk: Too few points"

-- | Smallest enclosing disk.
smallestEnclosingDisk'     :: (Ord r, Fractional r
                                               -- , Show r, Show p

                              )
                           => Point 2 r :+ p -> Point 2 r :+ p -> [Point 2 r :+ p]
                           -> DiskResult p r
smallestEnclosingDisk' a b = foldr addPoint (initial a b) . List.tails
  where
    -- The empty case occurs only initially
    addPoint []      br            = br
    addPoint (p:pts) br@(DiskResult d _)
      | (p^.core) `inClosedBall` d = br
      | otherwise                  = fromJust' $ smallestEnclosingDiskWithPoint p (a :| (b : pts))
    fromJust' = fromMaybe (error "smallestEncosingDisk' : fromJust, absurd")

-- | Smallest enclosing disk, given that p should be on it.
smallestEnclosingDiskWithPoint              :: (Ord r, Fractional r
                                               -- , Show r, Show p
                                               )
                                            => Point 2 r :+ p -> NonEmpty (Point 2 r :+ p)
                                            -> Maybe (DiskResult p r)
smallestEnclosingDiskWithPoint p (a :| pts) = foldr addPoint (Just $ initial p a) $ List.tails pts
  where
    addPoint []       br   = br
    addPoint (q:pts') br@(Just (DiskResult d _))
      | (q^.core) `inClosedBall` d = br
      | otherwise                  = smallestEnclosingDiskWithPoints p q (a:pts')
    addPoint _        br           = br


-- | Smallest enclosing disk, given that p and q should be on it
--
-- running time: \(O(n)\)
smallestEnclosingDiskWithPoints        :: (Ord r, Fractional r
                                          -- , Show r, Show p
                                          )
                                       => Point 2 r :+ p -> Point 2 r :+ p -> [Point 2 r :+ p]
                                       -> Maybe (DiskResult p r)
smallestEnclosingDiskWithPoints p q ps = minimumOn (^.enclosingDisk.squaredRadius)
                                       $ catMaybes [mkEnclosingDisk dl, mkEnclosingDisk dr, mdc]
  where
    centers = mapMaybe disk' ps
    -- generate a disk with p q and r
    disk' r = (r:+) <$> disk (p^.core) (q^.core) (r^.core)

    -- partition the points in to those on the left and those on the
    -- right.  Note that centers still contains only those points (and
    -- disks) for which the three points are not colinear. So the
    -- points are either on the left or on the right.
    (leftCenters,rightCenters) = List.partition (\(r :+ _) -> ccw' p q r == CCW) centers
    -- note that we consider 'leftmost' with respect to going from p
    -- to q. This does not really have a global meaning.

    -- we need to find the leftmost and rightmost center on the
    -- bisector. In case there are left-centers, this means that among
    -- the left centers we want to find the point that is furthest way
    -- from p (or q). If there are no left-centers, we with to find
    -- the closest one among the right-centers.
    leftDist z = let c = z^.extra.center
                     s = if ccw' p q c == CCW then 1 else -1
                 in s * squaredEuclideanDist (p^.core) (c^.core)

    dl = maximumOn leftDist leftCenters  -- disk that has the "leftmost" center
    dr = minimumOn leftDist rightCenters -- disk that has the "rightmost" center

    -- diameteral disk
    dd = fromDiameter (p^.core) (q^.core)
    mdc | isEnclosingDisk dd ps = Just $ DiskResult dd (Two p q)
        | otherwise             = Nothing

    -- test if d is an enclosing disk.
    mkEnclosingDisk  md = md >>= mkEnclosingDisk'
    mkEnclosingDisk' (r :+ d) | isEnclosingDisk d ps = Just (DiskResult d (Three p q r))
                              | otherwise            = Nothing


isEnclosingDisk   :: (Foldable t, Ord r, Num r)
                  => Disk p r -> t (Point 2 r :+ extra) -> Bool
isEnclosingDisk d = all (\s -> (s^.core) `inClosedBall` d)

-- | Constructs the initial 'DiskResult' from two points
initial     :: Fractional r => Point 2 r :+ p -> Point 2 r :+ p -> DiskResult p r
initial p q = DiskResult (fromDiameter (p^.core) (q^.core)) (Two p q)

maximumOn   :: Ord b => (a -> b) -> [a] -> Maybe a
maximumOn f = \case
    [] -> Nothing
    xs -> Just $ List.maximumBy (comparing f) xs

minimumOn   :: Ord b => (a -> b) -> [a] -> Maybe a
minimumOn f = \case
    [] -> Nothing
    xs -> Just $ List.minimumBy (comparing f) xs


--------------------------------------------------------------------------------

test :: Maybe (DiskResult () Rational)
test = smallestEnclosingDiskWithPoints p q myPts
  where
    p = ext $ Point2 0 (-6)
    q = ext $ Point2 0 6


myPts = map ext [Point2 5 1, Point2 3 3, Point2 (-2) 2, Point2 (-4) 5]

disk'' r = (r:+) <$> disk (p^.core) (q^.core) (r^.core)
  where
    p = ext $ Point2 0 (-6)
    q = ext $ Point2 0 6
