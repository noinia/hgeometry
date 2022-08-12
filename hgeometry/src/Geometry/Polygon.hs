{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Polygon
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Polygon data type and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module Geometry.Polygon
  ( -- * Types
    PolygonType(..)
  , Polygon(..)
  , _SimplePolygon, _MultiPolygon
  , SimplePolygon, MultiPolygon, SomePolygon

    -- * Conversion
  , fromPoints
  , fromCircularVector

  , simpleFromPoints
  , simpleFromCircularVector

  , unsafeFromPoints
  , unsafeFromCircularVector
  , unsafeFromVector
  , toVector
  , toPoints

  , isSimple

    -- * Accessors

  , size
  , polygonVertices, listEdges

  , outerBoundary, outerBoundaryVector
  , unsafeOuterBoundaryVector
  , outerBoundaryEdges
  , outerVertex, outerBoundaryEdge

  , polygonHoles, polygonHoles'
  , holeList

    -- * Properties

  , area, signedArea
  , centroid

    -- * Queries
  , inPolygon, insidePolygon, onBoundary


  , isTriangle, isStarShaped

  , isCounterClockwise
  , toCounterClockWiseOrder, toCounterClockWiseOrder'
  , toClockwiseOrder, toClockwiseOrder'
  , reverseOuterBoundary

  , rotateLeft
  , rotateRight
  , maximumVertexBy
  , minimumVertexBy


   -- * Misc
  , pickPoint
  , findDiagonal

  , withIncidentEdges, numberVertices

  , extremesLinear, cmpExtreme

  , findRotateTo

  ) where

import           Geometry.Polygon.InPolygon
import           Algorithms.Geometry.LinearProgramming.LP2DRIC
import           Algorithms.Geometry.LinearProgramming.Types
import           Control.Lens hiding (Simple)
import           Control.Monad.Random.Class
import           Data.Ext
import qualified Data.Foldable as F
import           Geometry.Boundary
import           Geometry.HalfSpace (rightOf)
import           Geometry.Line
import           Geometry.LineSegment
import           Geometry.Point
import           Geometry.Polygon.Core
import           Geometry.Polygon.Extremes
import           Geometry.Properties
import           Data.Ord (comparing)
import qualified Data.Sequence as Seq

--------------------------------------------------------------------------------
-- * Polygons

-- | Test if a Simple polygon is star-shaped. Returns a point in the kernel
-- (i.e. from which the entire polygon is visible), if it exists.
--
--
-- \(O(n)\) expected time
isStarShaped    :: (MonadRandom m, Ord r, Fractional r)
                => SimplePolygon p r -> m (Maybe (Point 2 r))
isStarShaped (toClockwiseOrder -> pg) =
    solveBoundedLinearProgram $ LinearProgram c (F.toList hs)
  where
    c  = pg^.outerVertex 1.core.vector
    -- the first vertex is the intersection point of the two supporting lines
    -- bounding it, so the first two edges bound the shape in this sirection
    hs = fmap (rightOf . supportingLine) . outerBoundaryEdges $ pg


--------------------------------------------------------------------------------
-- * Instances

type instance IntersectionOf (Line 2 r) (Boundary (Polygon t p r)) =
  '[Seq.Seq (Either (Point 2 r) (LineSegment 2 () r))]

type instance IntersectionOf (Point 2 r) (Polygon t p r) = [NoIntersection, Point 2 r]

instance (Fractional r, Ord r) => Point 2 r `HasIntersectionWith` Polygon t p r where
  q `intersects` pg = q `inPolygon` pg /= Outside

instance (Fractional r, Ord r) => Point 2 r `IsIntersectableWith` Polygon t p r where
  nonEmptyIntersection = defaultNonEmptyIntersection
  q `intersect` pg | q `intersects` pg = coRec q
                   | otherwise         = coRec NoIntersection

-- instance IsIntersectableWith (Line 2 r) (Boundary (Polygon t p r)) where
--   nonEmptyIntersection _ _ (CoRec xs) = null xs
--   l `intersect` (Boundary (SimplePolygon vs)) =
--     undefined
  -- l `intersect` (Boundary (MultiPolygon vs hs)) = coRec .
  --    Seq.sortBy f . Seq.fromList
  --     . concatMap (unpack . (l `intersect`) . Boundary)
  --     $ SimplePolygon vs : hs
  --   where
  --     unpack (CoRec x) = x
  --     f = undefined

instance (Fractional r, Ord r) => HasSquaredEuclideanDistance (Boundary (Polygon t p r)) where
  pointClosestToWithDistance q = F.minimumBy (comparing snd)
                               . fmap (pointClosestToWithDistance q)
                               . listEdges . review _Boundary

-- instance (Fractional r, Ord r) => HasSquaredEuclideanDistance (Polygon t p r) where
--   pointClosestToWithDistance q pg
--     | fromGenericPoint @Point q `intersects` pg = (q, 0)
--     | otherwise                                 = pointClosestToWithDistance q (Boundary pg)
