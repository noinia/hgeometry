module Algorithms.Geometry.ConvexHull.Minimalist.Point where


import           Algorithms.BinarySearch
import           Algorithms.DivideAndConquer
import           Control.Lens((^.), view)
import           Data.Geometry.Point (xCoord, yCoord, zCoord)
import qualified Data.Geometry.Point as Point
import           Data.Geometry.Polygon.Convex (lowerTangent')
import           Data.Geometry.Properties
import           Data.List.Util
import           Data.Ord (comparing, Down(..))
-- import           Data.Geometry.Triangle
-- import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import           Data.Sequence(Seq(..), ViewL(..), ViewR(..))
import qualified Data.OrdSeq as OrdSeq
import qualified Data.Set as Set
import           Data.Maybe
import           Data.Kind

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
