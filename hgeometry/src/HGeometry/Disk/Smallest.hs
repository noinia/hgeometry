--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Disk.Smallest
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Expected linear time implemenation for smallest enclsoing disk (by phrasing it as an
-- LP-type problem and solving it using clarksons algorithm.).
--
--------------------------------------------------------------------------------
module HGeometry.Disk.Smallest
  ( smallestEnclosingDisk
  , minDisk
  ) where


import           Control.Lens
import           Data.Foldable (toList)
import           Data.Foldable1
import qualified Data.Vector as V
import           HGeometry.Ball
import           HGeometry.Disk
import qualified HGeometry.Disk.Smallest.Naive as Naive
import           HGeometry.Intersection
import           HGeometry.LPType
import           HGeometry.Point
import           Prelude hiding (filter)
import           System.Random
import           VectorBuilder.Builder (foldable)
import           VectorBuilder.Vector (build)
import           Witherable

--------------------------------------------------------------------------------
-- * Smallest Enclosing disk

-- | Given a set of \(n \geq 2\) points, computes the smallest enclosing disk.
--
-- pre: the set contains at least 2 (distinct) points
--
-- running time: expected \(O(n)\)
smallestEnclosingDisk     :: ( Point_ point 2 r, Ord r, Fractional r, Foldable1 set
                             , HasIntersectionWith point (DiskByPoints point), Eq point
                             , SplitGen gen
                             )
                          => gen
                          -> set point
                          -> DiskByPoints point
smallestEnclosingDisk gen = snd . clarkson gen minDisk . build @V.Vector . foldable


-- | minimize the y-coordinate. (Lexicographically)
-- set contains at least two points
minDisk :: ( Point_ point 2 r, Foldable set, Ord r, Fractional r
           , HasIntersectionWith point (DiskByPoints point), Eq point
           ) => LPType r DiskByPoints set point
minDisk = LPType {
    costFunction           = view squaredRadius
  , combinatorialDimension = 2
  , extendBasis            = extendDisk
  , initialBasis           = initialDisk
  }

-- | pre:
--  - needs at least 2 distinct points!
initialDisk     :: (Foldable set, Point_ point 2 r, Eq point)
                => set point -> DiskByPoints point
initialDisk pts = case toList pts of
                    p:ps -> case filter (/= p) ps of
                              q:_ -> DiametralDisk $ DiametralPoints p q
                              _   -> error "initialDisk: Too few distinct points!"
                    _     -> error "initialDisk: precondition failed; no points!"

-- | Tries to extend the disk into the smallest enclosing disk.
extendDisk               :: ( Point_ point 2 r, Ord r, Fractional r
                            , HasIntersectionWith point (DiskByPoints point)
                            )
                         => point -> DiskByPoints point -> Maybe (DiskByPoints point)
extendDisk q disk
  | q `intersects` disk = Nothing
  | otherwise           = Just $ Naive.smallestEnclosingDiskWith q disk
    -- since q lies outside the disk, the disk is guaranteed to exist. Hence, the
    -- precondition is satisfied. Furthermore. The existing disk is defined by only O(1)
    -- points; so this runs in O(1) time.
