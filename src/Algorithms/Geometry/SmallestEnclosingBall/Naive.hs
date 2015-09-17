module Algorithms.Geometry.SmallestEnclosingBall.Naive where

-- just for the types
import Control.Lens
import Data.Ext
import Algorithms.Geometry.SmallestEnclosingBall.Types
import Data.Geometry.Ball
import Data.Geometry.Point
import Data.List(maximumBy)
import Data.Function(on)
import Data.Maybe(fromMaybe)

--------------------------------------------------------------------------------

-- | Horrible O(n^4) implementation that simply tries all disks, checks if they
-- enclose all points, and takes the largest one. Basically, this is only useful
-- to check correctness of the other algorithm(s)
smallestEnclosingDisk          :: (Ord r, Fractional r)
                               => Point 2 r :+ p -> Point 2 r :+ p -> [Point 2 r :+ p]
                               -> DiskResult p r
smallestEnclosingDisk p q rest = maximumBy (compare `on` (^.enclosingDisk.squaredRadius))
                               . filter (flip enclosesAll pts) $ pairs ++ triplets
  where
    pts      = p:q:rest
    pairs    = [DiskResult (fromDiameter (a^.core) (b^.core))   (Two a b)
               | a <- pts, b <- pts]
    triplets = [DiskResult (disk' (a^.core) (b^.core) (c^.core)) (Three a b c)
               | a <- pts, b <- pts, c <- pts]

    disk' a b c = fromMaybe degen $ disk a b c
    degen       = error "smallestEnclosingDisk: Unhandled degeneracy"


-- | check if a disk encloses all points
enclosesAll   :: (Num r, Ord r) => DiskResult p r -> [Point 2 r :+ q] -> Bool
enclosesAll d = all (\(p :+ _) -> p `inClosedBall` (d^.enclosingDisk))
