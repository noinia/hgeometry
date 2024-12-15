{-# LANGUAGE TemplateHaskell #-}
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
    Path(Path), pathSegments
  , PathSegment(..)

  , _PolyLineSegment
  , _PolygonPath
  , _CubicBezierSegment
  , _QuadraticBezierSegment
  , _EllipseSegment
  , _ArcSegment
  , _SplineSegment
  , _ClosedSplineSegment

  , Orientation(..)
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

import           Control.Lens hiding (rmap, elements)
import qualified Data.Sequence as Seq
import           Data.Traversable
import           GHC.Generics (Generic)
import           HGeometry.BezierSpline
import           HGeometry.Ellipse (Ellipse)
import           HGeometry.Matrix
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Polygon.Simple
import           HGeometry.Properties
import           HGeometry.Transformation

--------------------------------------------------------------------------------
-- | Paths

-- | Polygons in ipe may be given in CCW order, or in CW order. Since simple polygon
-- normalizes the order, we actually store the original orientation.
data Orientation = AsIs | Reversed
  deriving (Show,Eq,Ord)

-- | Paths consist of Path Segments. PathSegments come in the following forms:
data PathSegment r = PolyLineSegment        (PolyLine (Point 2 r))
                   | PolygonPath            {-# UNPACK #-}!Orientation
                                            (SimplePolygon (Point 2 r))
                   | CubicBezierSegment     (CubicBezier (Point 2 r))
                   | QuadraticBezierSegment (QuadraticBezier (Point 2 r))
                   | EllipseSegment         (Ellipse r)
                     -- TODO
                   | ArcSegment
                   | SplineSegment          -- (Spline 2 r)
                   | ClosedSplineSegment    -- (ClosedSpline 2 r)
                   deriving (Show,Eq)
makePrisms ''PathSegment

type instance NumType   (PathSegment r) = r
type instance Dimension (PathSegment r) = 2

instance Functor PathSegment where
  fmap = fmapDefault
instance Foldable PathSegment where
  foldMap = foldMapDefault
instance Traversable PathSegment where
  traverse f = \case
      PolyLineSegment p        -> PolyLineSegment
                                  <$> traverseOf (cloneTraversal $ vertices.coordinates) f p
      PolygonPath o p          -> PolygonPath o
                                  <$> traverseOf (cloneTraversal $ vertices.coordinates) f p
      CubicBezierSegment b     -> CubicBezierSegment
                                  <$> traverseOf (cloneTraversal $ vertices.coordinates) f b
      QuadraticBezierSegment b -> QuadraticBezierSegment
                                  <$> traverseOf (cloneTraversal $ vertices.coordinates) f b
      EllipseSegment e         -> EllipseSegment <$> traverse f e
      ArcSegment               -> pure ArcSegment
      SplineSegment            -> pure SplineSegment
      ClosedSplineSegment      -> pure ClosedSplineSegment


instance Fractional r => IsTransformable (PathSegment r) where
  transformBy t = \case
    PolyLineSegment p        -> PolyLineSegment $ transformBy t p
    PolygonPath o p          -> PolygonPath o (transformBy t p)
    CubicBezierSegment b     -> CubicBezierSegment $ transformBy t b
    QuadraticBezierSegment b -> QuadraticBezierSegment $ transformBy t b
    EllipseSegment e         -> EllipseSegment $ transformBy t e
    -- TODO:
    ArcSegment               -> ArcSegment
    SplineSegment            -> SplineSegment
    ClosedSplineSegment      -> ClosedSplineSegment


-- | A path is a non-empty sequence of PathSegments.
newtype Path r = Path { _pathSegments :: Seq.Seq (PathSegment r) }
                 deriving (Show,Eq,Functor,Foldable,Traversable,Generic)
                 deriving newtype (Semigroup)

-- | Lens/Iso to access the sequcne of segments of the path
pathSegments :: Iso (Path r) (Path r') (Seq.Seq (PathSegment r)) (Seq.Seq (PathSegment r'))
pathSegments = coerced

type instance NumType   (Path r) = r
type instance Dimension (Path r) = 2

instance Fractional r => IsTransformable (Path r) where
  transformBy t (Path s) = Path $ fmap (transformBy t) s

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
                 deriving (Eq,Show)
makePrisms ''Operation

instance Functor Operation where
  fmap = fmapDefault
instance Foldable Operation where
  foldMap = foldMapDefault
instance Traversable Operation where
  traverse f = let coordinates' = cloneTraversal coordinates
                   elements'    = cloneTraversal elements
               in \case
    MoveTo p         -> MoveTo <$> coordinates' f p
    LineTo p         -> LineTo <$> coordinates' f p
    Ellipse m        -> Ellipse <$> elements' f m
    ArcTo m p        -> ArcTo   <$> elements' f m <*> coordinates' f p
    Spline pts       -> Spline <$> traverse (coordinates' f) pts
    ClosedSpline pts -> ClosedSpline  <$> traverse (coordinates' f) pts
    ClosePath        -> pure ClosePath
    CurveTo p q r    -> CurveTo  <$> coordinates' f p <*> coordinates' f q <*> coordinates' f r
    QCurveTo p q     -> QCurveTo <$> coordinates' f p <*> coordinates' f q
