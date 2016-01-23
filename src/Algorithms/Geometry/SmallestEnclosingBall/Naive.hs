module Algorithms.Geometry.SmallestEnclosingBall.Naive where

-- just for the types
import Control.Lens
import Data.Ext
import Algorithms.Geometry.SmallestEnclosingBall.Types
import Data.Geometry.Ball
import Data.Geometry.Point
import Data.List(minimumBy)
import Data.Function(on)
import Data.Maybe(fromMaybe)

--------------------------------------------------------------------------------

-- | Horrible O(n^4) implementation that simply tries all disks, checks if they
-- enclose all points, and takes the largest one. Basically, this is only useful
-- to check correctness of the other algorithm(s)
smallestEnclosingDisk          :: (Ord r, Fractional r, Show r)
                               => [Point 2 r :+ p]
                               -> DiskResult p r
smallestEnclosingDisk pts@(_:_:_) = minimumBy
                                      (compare `on` (^.enclosingDisk.squaredRadius))
                                  . filter (flip enclosesAll pts) $ pairs ++ triplets
  where
    pairs    = [DiskResult (fromDiameter (a^.core) (b^.core))   (Two a b)
               | a <- pts, b <- pts, a^.core /= b^.core]
    triplets = [DiskResult (disk' (a^.core) (b^.core) (c^.core)) (Three a b c)
               | (a,b,c) <- diffTriplets pts]

                -- a <- pts, b <- pts, c <- pts ]


    disk' a b c = fromMaybe (degen (a,b,c)) $ disk a b c
    degen x     = error $ "smallestEnclosingDisk: Unhandled degeneracy" ++ show x
smallestEnclosingDisk _           = error "smallestEnclosingDisk: Too few points"


diffTriplets    :: Eq a => [a :+ b] -> [(a :+ b,a :+ b, a :+ b)]
diffTriplets xs = [ (a,b,c)
            | (a,b) <- ys, c <- xs, c^.core /= a^.core, c^.core /= b^.core
            ]
  where
    ys = [(a,b) | a <- xs, b <- xs, a^.core /= b^.core]


-- | check if a disk encloses all points
enclosesAll   :: (Num r, Ord r) => DiskResult p r -> [Point 2 r :+ q] -> Bool
enclosesAll d = all (\(p :+ _) -> p `inClosedBall` (d^.enclosingDisk))
