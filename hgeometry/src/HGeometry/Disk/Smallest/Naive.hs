--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Disk.Smallest.Naive
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Naive implementation to compute the smallest enclosing disk of a set of
-- points in \(\mathbb{R}^2\)
--
--------------------------------------------------------------------------------
module HGeometry.Disk.Smallest.Naive
  ( smallestEnclosingDisk
  , smallestEnclosingDiskWith
  , enclosesAll
  ) where

import           Control.Lens
import           Data.Foldable (toList)
import           Data.Foldable1
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe)
import           Data.Semigroup
import           HGeometry.Ball
import           HGeometry.Combinatorial.Util
import           HGeometry.Disk
import           HGeometry.Intersection
import           HGeometry.Point

--------------------------------------------------------------------------------

-- | Horrible \( O(n^4) \) implementation that simply tries all disks, checks if they
-- enclose all points, and takes the largest one. Basically, this is only useful
-- to check correctness of the other algorithm(s)
--
-- pre: the set contains at least 2 (distinct) points
smallestEnclosingDisk     :: (Point_ point 2 r, Ord r, Fractional r, Foldable set)
                          => set point
                          -> DiskByPoints point
smallestEnclosingDisk pts = minDisk
  where
    Min (Arg _ minDisk) = foldMap1 (\disk -> Min (Arg (disk^.squaredRadius) disk))
                        $ NonEmpty.fromList $ disks2 <> disks3
                        -- by the precondition, there are at least two distinct points
                        -- so there is at least one element in disks2

    disks2 = mapMaybe (\(Two p q) -> enclosesAll pts $ DiametralDisk (DiametralPoints p q))
           $ uniquePairs pts
    disks3 = mapMaybe (\(Three a b c) -> (DiskByPoints <$> diskFromPoints a b c)
                                         >>= enclosesAll pts)
           $ uniqueTriplets pts

-- | Test if the given disk encloses all points.
enclosesAll                                         :: ( Foldable set, Point_ point 2 r
                                                       , HasInBall disk
                                                       , HasIntersectionWith (Point 2 r) disk
                                                       ) => set point -> disk -> Maybe disk
enclosesAll pts disk
  | all (\p -> (p^.asPoint) `intersects` disk) pts = Just disk
  | otherwise                                      = Nothing

    -- I think we should be able to avoid the fractional constraint, and get a 'Num' constraint
    -- instead, by comparing against the squaredDiameter.

-- | Given a point p and a non-empty set of n points, computes the smallest disk that has
-- p on the boundary and encloses all points.
--
-- pre: such a disk exists
--
-- running time: \(O(n^2)\)
smallestEnclosingDiskWith       :: (Point_ point 2 r, Fractional r, Ord r, Foldable1 nonEmpty)
                                => point -> nonEmpty point -> DiskByPoints point
smallestEnclosingDiskWith p pts = minDisk
  where
    Min (Arg _ minDisk) = foldMap1 (\disk -> Min (Arg (disk^.squaredRadius) disk))
                        $ NonEmpty.fromList $ disks2 <> disks3
                        -- by the precondition, such a disk must exist.

    disks2 = mapMaybe (\q -> enclosesAll pts $ DiametralDisk (DiametralPoints p q)) (toList pts)
    disks3 = mapMaybe (\(Two a b) -> (DiskByPoints <$> diskFromPoints p a b) >>= enclosesAll pts)
           $ uniquePairs pts

    -- TODO: As with the Naive solution can we write this without using the fractioanl
    -- constraint?




-- pts@(_:_:_) = smallestEnclosingDisk' pts $
--                                       pairs pts ++ triplets pts
-- smallestEnclosingDisk _           = error "smallestEnclosingDisk: Too few points"

-- pairs     :: Fractional r => [Point 2 r :+ p] -> [DiskResult p r]
-- pairs pts = [ DiskResult (fromDiameter (a^.core) (b^.core)) (Two a b)
--             | Util.Two a b <- uniquePairs pts]

-- triplets     :: (Ord r, Fractional r) => [Point 2 r :+ p] -> [DiskResult p r]
-- triplets pts = [DiskResult (disk' a b c) (Three a b c)
--                | Util.Three a b c <- uniqueTriplets pts]

-- {- HLINT ignore disk' -}
-- disk'       :: (Ord r, Fractional r)
--             => Point 2 r :+ p -> Point 2 r :+ p -> Point 2 r :+ p -> Disk () r
-- disk' a b c = fromMaybe degen $ disk (a^.core) (b^.core) (c^.core)
--   where
--     -- if the points are colinear, select the disk by the diametral pair
--     degen = (smallestEnclosingDisk' [a,b,c] $ pairs [a,b,c])^.enclosingDisk


-- -- | Given a list of canidate enclosing disks, report the smallest one.
-- smallestEnclosingDisk'     :: (Ord r, Num r)
--                            => [Point 2 r :+ p] -> [DiskResult p r] -> DiskResult p r
-- smallestEnclosingDisk' pts = minimumBy (compare `on` (^.enclosingDisk.squaredRadius))
--                            . filter (`enclosesAll` pts)

-- -- | check if a disk encloses all points
-- enclosesAll   :: (Num r, Ord r) =>  -> [Point 2 r :+ q] -> Bool
-- enclosesAll d = all (\(p :+ _) -> p `inClosedBall` (d^.enclosingDisk))
