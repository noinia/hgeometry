--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.SmallestEnclosingBall.Naive
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Naive implementation to compute the smallest enclosing disk of a set of
-- points in \(\mathbb{R}^2\)
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.SmallestEnclosingBall.Naive( smallestEnclosingDisk
                                                      , enclosesAll
                                                      ) where

-- just for the types
import Control.Lens
import Data.Ext
import Algorithms.Geometry.SmallestEnclosingBall.Types
import Data.Geometry.Ball
import Data.Geometry.Point
import Data.List (minimumBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Util(uniquePairs, uniqueTriplets)
import qualified Data.Util as Util
--------------------------------------------------------------------------------

-- | Horrible O(n^4) implementation that simply tries all disks, checks if they
-- enclose all points, and takes the largest one. Basically, this is only useful
-- to check correctness of the other algorithm(s)
smallestEnclosingDisk          :: (Ord r, Fractional r)
                               => [Point 2 r :+ p]
                               -> DiskResult p r
smallestEnclosingDisk pts@(_:_:_) = smallestEnclosingDisk' pts $
                                      pairs pts ++ triplets pts
smallestEnclosingDisk _           = error "smallestEnclosingDisk: Too few points"

pairs     :: Fractional r => [Point 2 r :+ p] -> [DiskResult p r]
pairs pts = [ DiskResult (fromDiameter (a^.core) (b^.core)) (Two a b)
            | Util.Two a b <- uniquePairs pts]

triplets     :: (Ord r, Fractional r) => [Point 2 r :+ p] -> [DiskResult p r]
triplets pts = [DiskResult (disk' a b c) (Three a b c)
               | Util.Three a b c <- uniqueTriplets pts]

disk'       :: (Ord r, Fractional r)
            => Point 2 r :+ p -> Point 2 r :+ p -> Point 2 r :+ p -> Disk () r
disk' a b c = fromMaybe degen $ disk (a^.core) (b^.core) (c^.core)
  where
    -- if the points are colinear, select the disk by the diametral pair
    degen = (smallestEnclosingDisk' [a,b,c] $ pairs [a,b,c])^.enclosingDisk


-- | Given a list of canidate enclosing disks, report the smallest one.
smallestEnclosingDisk'     :: (Ord r, Num r)
                           => [Point 2 r :+ p] -> [DiskResult p r] -> DiskResult p r
smallestEnclosingDisk' pts = minimumBy (compare `on` (^.enclosingDisk.squaredRadius))
                           . filter (flip enclosesAll pts)

-- | check if a disk encloses all points
enclosesAll   :: (Num r, Ord r) => DiskResult p r -> [Point 2 r :+ q] -> Bool
enclosesAll d = all (\(p :+ _) -> p `inClosedBall` (d^.enclosingDisk))
