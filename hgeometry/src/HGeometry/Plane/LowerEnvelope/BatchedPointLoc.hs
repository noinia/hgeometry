module HGeometry.LowerEnvelope.BatchedPointLoc
  ( -- batchedPointLocation
  ) where

import           Control.Lens
import qualified Data.Vector as Boxed
import           HGeometry.Interval
import           HGeometry.Line
import           HGeometry.Line.LineEQ
-- import           HGeometry.LowerEnvelope.Type
import           HGeometry.Point

--------------------------------------------------------------------------------
{-

-- | Given a set of n query points and a set of r planes, computes
-- an output array A of the planes, and for each query point q an interval [i,j] so that
-- the planes above q are stored in A[i..j]
--
--
-- running time: O()
batchedPointLocation           :: f (Point 3 r) -> g (Plane r) -> ( Boxed.Vector (Plane r)
                                                                  , f (Point 3 r, ClosedInterval Int)
                                                                  )
batchedPointLocation qs planes = undefined
  where
    allVertices = undefined



-- | Given a set of n query points and a set of r lines, computes
-- an output array A of the lines, and for each query point q an interval [i,j] so that
-- the planes above q are stored in A[i..j]
--
--
-- running time: O((r^2 + n) log (r+n) + k) where k = O(nr) is the output size
batchedPointLocation2 :: f (Point 2 r) -> g (LineEQ r) -> ( Boxed.Vector (LineEQ r)
                                                        , f (Point 2 r, ClosedInterval Int)
                                                        )
batchedPointLocation2 = undefined
-- use some sweepline here. so maybe don't report them as a vector
-}
