{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.BezierSpline
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Basic Bezier splines.
--
--------------------------------------------------------------------------------
module HGeometry.BezierSpline
  ( BezierSplineF(..)
  , BezierSpline
  , QuadraticBezier
  , CubicBezier
  -- , module HGeometry.BezierSpline.Class
  ) where


import Control.DeepSeq (NFData)
import Control.Lens
-- import qualified Data.Foldable as F
import Data.Functor.Classes
-- import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup.Foldable
import Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import GHC.Generics
import HGeometry.Box
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Transformation
import HGeometry.Vector
import HGeometry.Vector.NonEmpty.Util ()
import Hiraffe.Graph
import Data.Kind (Type)
-- import GHC.TypeLits

--------------------------------------------------------------------------------

-- | A Bezier spline.
type BezierSplineF              :: (Type -> Type) -> Type -> Type
newtype BezierSplineF f point = BezierSpline (f point)
  deriving (Generic)
  deriving newtype (NFData,Functor,Foldable,Foldable1,Eq,Ord,Eq1,Ord1)

-- | By default we store simple poylline as non-empty vectors.
type BezierSpline = BezierSplineF NonEmptyVector

-- | Quadratic Bezier splines
type QuadraticBezier = BezierSplineF (Vector 3)
-- | Cubic Bezier splines
type CubicBezier     = BezierSplineF (Vector 4)

type instance Dimension (BezierSplineF f point) = 2
type instance NumType   (BezierSplineF f point) = NumType point


-- | Access the container
_BezierSplineF :: Iso (BezierSplineF f point) (BezierSplineF f' point')
                      (f point)                 (f' point' )
_BezierSplineF = iso (\(BezierSpline vs) -> vs) BezierSpline

instance Traversable f => Traversable (BezierSplineF f) where
  traverse f (BezierSpline vs) = BezierSpline <$> traverse f vs
instance Traversable1 f => Traversable1 (BezierSplineF f) where
  traverse1 f (BezierSpline vs) = BezierSpline <$> traverse1 f vs

instance (TraversableWithIndex Int f
         , IxValue (f point) ~ point
         , Index   (f point) ~ Int
         , Ixed    (f point)
         ) => HasVertices (BezierSplineF f point) (BezierSplineF f point') where
  vertices = _BezierSplineF . itraversed

instance ( Traversable1 f
         , IxValue (f point) ~ point
         , Index   (f point) ~ Int
         , Ixed    (f point)
         ) => HasPoints (BezierSplineF f point) (BezierSplineF f point') point point' where
  allPoints = _BezierSplineF . traversed1

instance ( Traversable1 f
         , IxValue (f point) ~ point
         , Index   (f point) ~ Int
         , Ixed    (f point)
         , DefaultTransformByConstraints (BezierSplineF f point) 2 r
         , Point_ point 2 r
         ) => IsTransformable (BezierSplineF f point)

instance ( Traversable1 f
         , IxValue (f point) ~ point
         , Index   (f point) ~ Int
         , Ixed    (f point)
         , Point_ point 2 r
         , OptVector_ 2 r, OptMetric_ 2 r, Ord (VectorFamily' 2 r)
         ) => IsBoxable (BezierSplineF f point)

instance ( TraversableWithIndex Int f
         , Ixed (f point)
         , IxValue (f point) ~ point
         , Index (f point) ~ Int
         ) => HasVertices' (BezierSplineF f point) where
  type Vertex   (BezierSplineF f point) = point
  type VertexIx (BezierSplineF f point) = Int
  vertexAt i = _BezierSplineF . iix i
