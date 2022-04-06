module Algorithms.Geometry.ConvexHull.Minimalist.Point where


import           Control.Lens (view)
import           Data.Geometry.Point (xCoord)
import qualified Data.Geometry.Point as Point
import           Data.Geometry.Properties
import           Data.Ord (comparing)

--------------------------------------------------------------------------------

class ( Ord (NumType point), Fractional (NumType point)
      , Point.ToAPoint point 3 (NumType point)
      ) => Point point where
  toPt3 :: point -> Point.Point 3 (NumType point)
  toPt3 = view Point.toPoint

toPt2                                :: Point point
                                     => Time point -> point -> Point.Point 2 (NumType point)
toPt2 t (toPt3 -> Point.Point3 x y z) = Point.Point2 x (z - t*y)


instance (Ord r, Fractional r) => Point (Point.Point 3 r)

compareX :: Point point => point -> point -> Ordering
compareX = comparing (view xCoord . toPt3)

--------------------------------------------------------------------------------

type Time point = NumType point
