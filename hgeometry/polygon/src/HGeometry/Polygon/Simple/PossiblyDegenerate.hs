--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Simple.PossiblyDegenerate
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Covnex polygon that may be degenerate, i.e. may also be a line segment or point.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Simple.PossiblyDegenerate
  ( PossiblyDegenerateSimplePolygon(..)
  ) where

import           Control.Lens
import           Data.Bifunctor
import           Data.Foldable (toList, for_)
import           Data.Foldable1
import           Data.Functor.Contravariant (phantom)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isJust, mapMaybe, maybeToList)
import           Data.Traversable
import           Data.Vector.NonEmpty (NonEmptyVector)
import           HGeometry.Box
import qualified HGeometry.Box as Box
import           HGeometry.Cyclic
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Interval.EndPoint
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon.Simple.Type
import           HGeometry.Properties
import           HGeometry.Triangle
import qualified HGeometry.Triangle as Triangle
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | A simple polygon (or a "subtype" thereof) that may be degenerate; i.e. may be a single
-- vertex, or a point.
data PossiblyDegenerateSimplePolygon vertex polygon =
    DegenerateVertex vertex
  | DegenerateEdge (ClosedLineSegment vertex)
  | ActualPolygon polygon
  deriving (Show,Eq,Functor)

instance Bifunctor PossiblyDegenerateSimplePolygon where
  bimap f g = \case
    DegenerateVertex v -> DegenerateVertex (f v)
    DegenerateEdge e   -> DegenerateEdge (fmap f e)
    ActualPolygon poly -> ActualPolygon (g poly)

--------------------------------------------------------------------------------

-- | A HalfPlane and a simple polygon intersect in a bunch of components, each of
-- which is a possiblyDegenerate simple polygon.
type instance Intersection (HalfSpaceF line) (SimplePolygonF f point) =
  [HalfPlane_x_SimplePolygon_Component f (NumType point) point]
-- | A single Component of a HalfPlane x SimplePolygon intersection.
type HalfPlane_x_SimplePolygon_Component f r vertex =
  PossiblyDegenerateSimplePolygon vertex (SimplePolygonF f (OriginalOrExtra vertex (Point 2 r)))


-- | If we drag along extra information in the halfplane polygon intersection we lose it
type instance Intersection (HalfSpaceF line :+ extra) (SimplePolygonF f point :+ extra') =
  Intersection (HalfSpaceF line) (SimplePolygonF f point)
