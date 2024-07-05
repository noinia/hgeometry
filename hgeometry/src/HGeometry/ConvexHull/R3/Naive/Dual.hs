module HGeometry.ConvexHull.R3.Naive.Dual
  ( UpperHull
  , upperHull
  ) where

import Control.Lens
import Data.Foldable (toList)
import Data.Foldable1
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isNothing)
import HGeometry.Combinatorial.Util
import HGeometry.Duality
import HGeometry.Ext
import HGeometry.HalfSpace
import HGeometry.HyperPlane
import HGeometry.HyperPlane.NonVertical
import HGeometry.Intersection (intersects)
import HGeometry.Plane.LowerEnvelope (LowerEnvelope)
import HGeometry.Plane.LowerEnvelope.Naive
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Triangle
import HGeometry.Vector

--------------------------------------------------------------------------------



type UpperHull point = LowerEnvelope (NonVerticalHyperPlane 3 (NumType point) :+ point)


upperHull :: ( Point_ point 3 r
             , Ord r, Fractional r
             , Foldable1 f, Functor f

             , Show point, Show r, Ord point
             ) => f point -> UpperHull point
upperHull = lowerEnvelope . fmap (\p -> dualHyperPlane p :+ p)


-- newtype DualHyperPlane point = DualHyperPlane point
--   deriving (Show,Read,Eq,Ord)

-- instance (Point_ point d r, 1 <= d, Num r) => HyperPlane_ (DualHyperPlane point) d r where


-- instance (Point_ point d r, 1 <= d, Num r) => NonVerticalHyperPlane_ (DualHyperPlane point) d r where
