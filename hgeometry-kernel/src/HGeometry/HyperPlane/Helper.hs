module HGeometry.HyperPlane.Helper
  ( intersectsSeg

  ) where

import Control.Lens
import Data.Type.Ord
import GHC.TypeLits
import HGeometry.HyperPlane.Class
import HGeometry.Interval.EndPoint
import HGeometry.LineSegment.Class
import HGeometry.Point.Class

--------------------------------------------------------------------------------

-- | Test if a segemnt intersects a hyperplane.
--
-- helper for implementing the HasIntersection typeclass
--
intersectsSeg       :: ( LineSegment_ segment point
                       , HyperPlane_ hyperPlane d r
                       , Point_ point d r
                       , Ord r, Num r
                       , KnownNat d, d < d+1
                       ) =>  hyperPlane -> segment -> Bool
intersectsSeg l seg = case ( onSideTest (seg^.start) l, onSideTest (seg^.end) l ) of
                        (LT,LT) -> False
                        (GT,GT) -> False
                        (LT,GT) -> True
                        (GT,LT) -> True
                        (EQ,EQ) -> True -- using that the endpoints are not the same
                        (EQ,_)  -> seg^.startPoint.to endPointType == Closed
                        (_,EQ)  -> seg^.endPoint.to endPointType   == Closed
  -- main idea: if the signs are different then the segment intersects the plane,
  -- if the signs are  the same they won't intersect. If one of the points lies
  -- on the plane it depends on the type of the endpoints. I.e. only if the endpoint
  -- on the plane is closed it intersects.
