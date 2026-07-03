module CatmulRomSpline
  ( CatmulRomSplineF(CatmulRomSpline, CatmulRomSegment), CatmulRomSpline
  , CatmulRomSegment
  , toCubicBezier
  ) where

import Control.DeepSeq (NFData)
import Control.Lens
-- import qualified Data.Foldable as F
import Data.Functor.Classes
-- import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup.Foldable
import Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import GHC.Generics (Generic)
--import HGeometry.Box
import HGeometry.Point
import HGeometry.Properties
-- import HGeometry.Transformation
import HGeometry.Vector
import HGeometry.Matrix
import HGeometry.BezierSpline
import HGeometry.Vector.NonEmpty.Util ()
import Data.Kind (Type)
import Data.Coerce
import Data.Distributive
import Ipe
import Ipe.Draw

--------------------------------------------------------------------------------

-- | A CatmulRom spline.
type CatmulRomSplineF            :: (Type -> Type) -> Type -> Type
newtype CatmulRomSplineF f point = CatmulRomSpline (f point)
  deriving stock (Generic,Show)
  deriving newtype (NFData,Functor,Foldable,Foldable1,Eq,Ord,Eq1,Ord1)

-- | By default we store simple poylline as non-empty vectors.
type CatmulRomSpline = CatmulRomSplineF NonEmptyVector


type instance Dimension (CatmulRomSplineF f point) = 2
type instance NumType   (CatmulRomSplineF f point) = NumType point

-- | A single catmul rom spline segment
type CatmulRomSegment = CatmulRomSplineF (Vector 4)

-- | The CatmulRomSegment a p q b represents the segment between p and q
pattern CatmulRomSegment         :: point -> point -> point -> point -> CatmulRomSegment point
pattern CatmulRomSegment a b c d = CatmulRomSpline (Vector4 a b c d)
{-# COMPLETE CatmulRomSegment #-}

--------------------------------------------------------------------------------

-- | Given a function that acts on a vector of n individual coordinates, and a
-- vector of n points; applies the function coordinate wise to the points.
-- i.e. for each coordinate i; we collect the i^th coordinates of the points and apply
-- the function there. We then recombine the resulting vector of output coordinates
-- into new points.
coordinateWise   :: forall point d r n m s. ( Point_ point d r
                    , Functor (Vector m)
                    , Functor (Vector d), Functor (Vector n)
                    , Distributive (Vector m), Distributive (Vector d)
                    )
                 => (Vector n r -> Vector m s) -> Vector n point -> Vector m (Point d s)
coordinateWise f = fmap coerce
                 . distribute . fmap f . distribute . fmap (view vector)
  -- TODO: I don't see why we cannot just coerce the entire thing from
  -- Vector m (Vector m s) to Vector m (Point d s) rather than using
  -- fmap coerce


-- | Convert a CatmulRom spline segment in a Cubic Bezier spline segment
toCubicBezier :: forall point r. (Point_ point 2 r, Fractional r)
              => CatmulRomSegment point
              -> CubicBezier (Point 2 r)
toCubicBezier (CatmulRomSpline controlPoints) =
    BezierSpline $ coordinateWise (m !*) controlPoints
  where
    m :: Matrix 4 4 r
    m = (1/6) *!! matrixFromRows (Vector4 (Vector4 0    6 0 0)
                                          (Vector4 (-1) 6 1 0)
                                          (Vector4 0    1 6 (-1))
                                          (Vector4 0    0 6 0)
                                 )
  -- see https://en.wikipedia.org/wiki/Catmull%E2%80%93Rom_spline#Converting_to_B%C3%A9zier_curve




--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

instance (Point_ point 2 r, Fractional r
         ) => IsDrawable (Ipe r) (CatmulRomSegment point) where
  type AttrOf (Ipe r) (CatmulRomSegment point) = PathAttributes r
  draw ats = draw @(Ipe r) ats . toCubicBezier


{-
-- | Given a value t in the range [0,1], evaluate the catmul rom spline
evalAt :: (Point_ point 2 r, Fractional r) => r -> CatmulRomSegment point -> Point 2 r
evalAt t (CatmulRomSpline controlPoints) = Point2 (f xPoints) (f yPoints)
  where
    f w = let Vector1 x = (1/2 *^ v) *! m !* w in x
    v = let s = t*t in Vector4 (s*t) s t 1
    m = matrixFromRows $ Vector4 (Vector4 (-1) 3    (-3) 1)
                                 (Vector4 2    (-5) 4    (-1))
                                 (Vector4 (-1) 0    1    0)
                                 (Vector4 0    2    0    0)
    xPoints = (^.xCoord) <$> controlPoints
    yPoints = (^.yCoord) <$> controlPoints
-}
