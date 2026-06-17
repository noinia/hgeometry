module Main
  (main) where


import Control.DeepSeq (NFData)
import Control.Lens
-- import qualified Data.Foldable as F
import Data.Functor.Classes
-- import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup.Foldable
import Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import GHC.Generics (Generic)
import HGeometry.Box
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Transformation
import HGeometry.Vector
import HGeometry.Matrix
import HGeometry.BezierSpline
import HGeometry.Vector.NonEmpty.Util ()
import Hiraffe.Graph
import Data.Kind (Type)
-- import GHC.TypeLits

import Data.Distributive
-- import Ipe
import HGeometry.Number.Real.Rational

--------------------------------------------------------------------------------

type R = RealNumber 5


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

coordinateWise   :: (Vector n r -> Vector m s) -> Vector n point -> Vector m (Point d s)
coordinateWise f vPts = fmap f . distribute . fmap (view vector) $ vPts
  where
    vmVecs =
      -- distribute == transpose'





-- transpose' :: Vector n (Vector m r) -> Vector m (Vector n r)
-- transpose' = distribute





-- | Convert a CatmulRom spline segment in a Cubic Bezier spline segment
toCubicBezier :: forall point r. (Point_ point 2 r, Fractional r)
              => CatmulRomSegment point
              -> CubicBezier (Point 2 r)
toCubicBezier (CatmulRomSpline controlPoints) = BezierSpline $ m !* controlPoints
  where
    m :: Matrix 4 4 r
    m = (1/6) *!! matrixFromRows (Vector4 (Vector4 0    6 0 0)
                                          (Vector4 (-1) 6 1 0)
                                          (Vector4 0    1 6 (-1))
                                          (Vector4 0    0 6 0)
                                 )
  -- see https://en.wikipedia.org/wiki/Catmull%E2%80%93Rom_spline#Converting_to_B%C3%A9zier_curve

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


spline :: CatmulRomSegment (Point 2 R)
spline = CatmulRomSegment (Point2 (-1) 1) (Point2 0 0) (Point2 10 0) (Point2 11 1)

-- main = printAsIpeSelection [toCubicBezier spline]
main = print $ toCubicBezier spline
