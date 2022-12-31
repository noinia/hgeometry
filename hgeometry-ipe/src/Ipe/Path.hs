--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.Path
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines an Ipe Path.
--
--------------------------------------------------------------------------------
module Ipe.Path(
    PathF(Path), Path, pathSegments
  , PathSegment(..)

  , _PolyLineSegment
  , _PolygonPath
  , _CubicBezierSegment
  , _QuadraticBezierSegment
  , _EllipseSegment
  , _ArcSegment
  , _SplineSegment
  , _ClosedSplineSegment

  , Operation(..)
  , _MoveTo
  , _LineTo
  , _CurveTo
  , _QCurveTo
  , _Ellipse
  , _ArcTo
  , _Spline
  , _ClosedSpline
  , _ClosePath
  ) where

import Control.Lens hiding (rmap)
import Data.Bitraversable
import HGeometry.BezierSpline
import HGeometry.Point
-- import           HGeometry.Ellipse(Ellipse)
import HGeometry.PolyLine
import HGeometry.Polygon.Simple
import HGeometry.Properties
import HGeometry.Transformation
import HGeometry.Matrix
import Data.Sequence (Seq)
import Data.Traversable

--------------------------------------------------------------------------------
-- | Paths

type Ellipse r = ()

-- | Paths consist of Path Segments. PathSegments come in the following forms:
data PathSegment r = PolyLineSegment        (PolyLine (Point 2 r))
                   | PolygonPath            (SimplePolygon (Point 2 r))
                   | CubicBezierSegment     (CubicBezier (Point 2 r))
                   | QuadraticBezierSegment (QuadraticBezier (Point 2 r))
                   | EllipseSegment         (Ellipse r)
                     -- TODO
                   | ArcSegment
                   | SplineSegment          -- (Spline 2 r)
                   | ClosedSplineSegment    -- (ClosedSpline 2 r)
                   -- deriving (Show,Eq)
-- makePrisms ''PathSegment

_PolyLineSegment ::  Prism' (PathSegment r) (PolyLine (Point 2 r))
_PolyLineSegment
  = (prism (\ x1_adRP -> PolyLineSegment x1_adRP))
      (\ x_adRQ
         -> case x_adRQ of
              PolyLineSegment y1_adRR -> Right y1_adRR
              _ -> Left x_adRQ)
{-# INLINE _PolyLineSegment #-}
_PolygonPath ::  Prism' (PathSegment r) (SimplePolygon (Point 2 r))
_PolygonPath
  = (prism (\ x1_adRS -> PolygonPath x1_adRS))
      (\ x_adRT
         -> case x_adRT of
              PolygonPath y1_adRU -> Right y1_adRU
              _ -> Left x_adRT)
{-# INLINE _PolygonPath #-}
_CubicBezierSegment ::  Prism' (PathSegment r) (CubicBezier (Point 2 r))
_CubicBezierSegment
  = (prism (\ x1_adRV -> CubicBezierSegment x1_adRV))
      (\ x_adRW
         -> case x_adRW of
              CubicBezierSegment y1_adRX -> Right y1_adRX
              _ -> Left x_adRW)
{-# INLINE _CubicBezierSegment #-}
_QuadraticBezierSegment ::  Prism' (PathSegment r) (QuadraticBezier (Point 2 r))
_QuadraticBezierSegment
  = (prism (\ x1_adRY -> QuadraticBezierSegment x1_adRY))
      (\ x_adRZ
         -> case x_adRZ of
              QuadraticBezierSegment y1_adS0 -> Right y1_adS0
              _ -> Left x_adRZ)
{-# INLINE _QuadraticBezierSegment #-}
_EllipseSegment ::  Prism' (PathSegment r) (Ellipse r)
_EllipseSegment
  = (prism (\ x1_adS1 -> EllipseSegment x1_adS1))
      (\ x_adS2
         -> case x_adS2 of
              EllipseSegment y1_adS3 -> Right y1_adS3
              _ -> Left x_adS2)
{-# INLINE _EllipseSegment #-}
_ArcSegment :: Prism' (PathSegment r) ()
_ArcSegment
  = (prism (\ () -> ArcSegment))
      (\ x_adS4
         -> case x_adS4 of
              ArcSegment -> Right ()
              _ -> Left x_adS4)
{-# INLINE _ArcSegment #-}
_SplineSegment :: Prism' (PathSegment r) ()
_SplineSegment
  = (prism (\ () -> SplineSegment))
      (\ x_adS5
         -> case x_adS5 of
              SplineSegment -> Right ()
              _ -> Left x_adS5)
{-# INLINE _SplineSegment #-}
_ClosedSplineSegment ::  Prism' (PathSegment r) ()
_ClosedSplineSegment
  = (prism (\ () -> ClosedSplineSegment))
      (\ x_adS6
         -> case x_adS6 of
              ClosedSplineSegment -> Right ()
              _ -> Left x_adS6)
{-# INLINE _ClosedSplineSegment #-}


type instance NumType   (PathSegment r) = r
type instance Dimension (PathSegment r) = 2

-- instance Functor PathSegment where
--   fmap = fmapDefault
-- instance Foldable PathSegment where
--   foldMap = foldMapDefault
-- instance Traversable PathSegment where
--   traverse f = \case
--     PolyLineSegment p        -> PolyLineSegment <$> bitraverse pure f p
--     PolygonPath p            -> PolygonPath <$> bitraverse pure f p
--     CubicBezierSegment b     -> CubicBezierSegment <$> traverse f b
--     QuadraticBezierSegment b -> QuadraticBezierSegment <$> traverse f b
--     EllipseSegment e         -> pure $ EllipseSegment () -- EllipseSegment <$> traverse f e
--     ArcSegment               -> pure ArcSegment
--     SplineSegment            -> pure SplineSegment
--     ClosedSplineSegment      -> pure ClosedSplineSegment

-- instance Fractional r => IsTransformable (PathSegment r) where
--   transformBy t = \case
--     PolyLineSegment p        -> PolyLineSegment $ transformBy t p
--     PolygonPath p            -> PolygonPath $ transformBy t p
--     CubicBezierSegment b     -> CubicBezierSegment $ transformBy t b
--     QuadraticBezierSegment b -> QuadraticBezierSegment $ transformBy t b
--     -- TODO:
--     EllipseSegment e         -> EllipseSegment () -- $ transformBy t e
--     ArcSegment               -> ArcSegment
--     SplineSegment            -> SplineSegment
--     ClosedSplineSegment      -> ClosedSplineSegment


-- | A path is a non-empty sequence of PathSegments.
newtype PathF f r = Path { _pathSegments :: f (PathSegment r) }
                 -- deriving (Show,Eq,Functor,Foldable,Traversable)
-- makeLenses ''Path

type Path = PathF Seq

pathSegments :: Iso (PathF f r)         (PathF f' r')
                    (f (PathSegment r)) (f' (PathSegment r'))
pathSegments = iso (\(Path s) -> s) Path

type instance NumType   (PathF f r) = r
type instance Dimension (PathF f r) = 2

-- instance Fractional r => IsTransformable (PathF f r) where
--   transformBy t (Path s) = Path $ fmap (transformBy t) s


--------------------------------------------------------------------------------

-- | type that represents a path in ipe.
data Operation r = MoveTo (Point 2 r)
                 | LineTo (Point 2 r)
                 | Ellipse (Matrix 3 3 r)
                 | ArcTo (Matrix 3 3 r) (Point 2 r)
                 | Spline [Point 2 r]
                 | ClosedSpline [Point 2 r]
                 | ClosePath
                 -- these should be deprecated
                 | CurveTo (Point 2 r) (Point 2 r) (Point 2 r)
                 | QCurveTo (Point 2 r) (Point 2 r)
                 -- deriving (Eq, Show,Functor,Foldable,Traversable)
-- makePrisms ''Operation

_MoveTo ::  Prism' (Operation r) (Point 2 r)
_MoveTo
  = prism MoveTo
      (\ x_al8s
         -> case x_al8s of
              MoveTo y1_al8t -> Right y1_al8t
              _ -> Left x_al8s)
{-# INLINE _MoveTo #-}
_LineTo ::  Prism' (Operation r) (Point 2 r)
_LineTo
  = prism LineTo
      (\ x_al8v
         -> case x_al8v of
              LineTo y1_al8w -> Right y1_al8w
              _ -> Left x_al8v)
{-# INLINE _LineTo #-}
_Ellipse ::  Prism' (Operation r) (Matrix 3 3 r)
_Ellipse
  = prism Ellipse
      (\ x_al8y
         -> case x_al8y of
              Ellipse y1_al8z -> Right y1_al8z
              _ -> Left x_al8y)
{-# INLINE _Ellipse #-}
_ArcTo ::  Prism' (Operation r) (Matrix 3 3 r, Point 2 r)
_ArcTo
  = prism (\ (x1_al8A, x2_al8B) -> (ArcTo x1_al8A) x2_al8B)
      (\ x_al8C
         -> case x_al8C of
              ArcTo y1_al8D y2_al8E -> Right (y1_al8D, y2_al8E)
              _ -> Left x_al8C)
{-# INLINE _ArcTo #-}
_Spline ::  Prism' (Operation r) [Point 2 r]
_Spline
  = prism (\ x1_al8F -> Spline x1_al8F)
      (\ x_al8G
         -> case x_al8G of
              Spline y1_al8H -> Right y1_al8H
              _ -> Left x_al8G)
{-# INLINE _Spline #-}
_ClosedSpline ::  Prism' (Operation r) [Point 2 r]
_ClosedSpline
  = prism (\ x1_al8I -> ClosedSpline x1_al8I)
      (\ x_al8J
         -> case x_al8J of
              ClosedSpline y1_al8K -> Right y1_al8K
              _ -> Left x_al8J)
{-# INLINE _ClosedSpline #-}
_ClosePath :: Prism' (Operation r) ()
_ClosePath
  = prism (\ () -> ClosePath)
      (\ x_al8L
         -> case x_al8L of
              ClosePath -> Right ()
              _ -> Left x_al8L)
{-# INLINE _ClosePath #-}
_CurveTo ::  Prism' (Operation r) (Point 2 r, Point 2 r, Point 2 r)
_CurveTo
  = prism (\ (x1_al8M, x2_al8N, x3_al8O) -> ((CurveTo x1_al8M) x2_al8N) x3_al8O)
      (\ x_al8P
         -> case x_al8P of
              CurveTo y1_al8Q y2_al8R y3_al8S
                -> Right (y1_al8Q, y2_al8R, y3_al8S)
              _ -> Left x_al8P)
{-# INLINE _CurveTo #-}
_QCurveTo ::  Prism' (Operation r) (Point 2 r, Point 2 r)
_QCurveTo
  = prism (\ (x1_al8T, x2_al8U) -> (QCurveTo x1_al8T) x2_al8U)
      (\ x_al8V
         -> case x_al8V of
              QCurveTo y1_al8W y2_al8X -> Right (y1_al8W, y2_al8X)
              _ -> Left x_al8V)
{-# INLINE _QCurveTo #-}
