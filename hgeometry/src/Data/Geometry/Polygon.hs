--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Polygon
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Polygon data type and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module Data.Geometry.Polygon( PolygonType(..)
                            , Polygon(..)
                            , _SimplePolygon, _MultiPolygon
                            , SimplePolygon, MultiPolygon, SomePolygon

                            , fromPoints

                            , polygonVertices, listEdges

                            , outerBoundary, outerBoundaryEdges
                            , outerVertex, outerBoundaryEdge

                            , polygonHoles, polygonHoles'
                            , holeList
                            , connectHoles

                            , inPolygon, insidePolygon, onBoundary

                            , area, signedArea

                            , centroid
                            , pickPoint

                            , isTriangle, isStarShaped

                            , isCounterClockwise
                            , toCounterClockWiseOrder, toCounterClockWiseOrder'
                            , toClockwiseOrder, toClockwiseOrder'
                            , reverseOuterBoundary

                            , findDiagonal

                            , withIncidentEdges, numberVertices

                            , asSimplePolygon
                            , extremesLinear, cmpExtreme
                            ) where

import           Algorithms.Geometry.LinearProgramming.LP2DRIC (solveBoundedLinearProgram)
import           Algorithms.Geometry.LinearProgramming.Types   (LinearProgram (LinearProgram))
import           Control.Lens                                  ((^.))
import           Control.Monad.Random.Class                    (MonadRandom)
import           Data.Bifunctor
import           Data.CircularSeq as C
import           Data.Ext
import qualified Data.Foldable                                 as F
import           Data.Function
import           Data.Geometry.HalfSpace                       (rightOf)
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point                           (Point, vector)
import           Data.Geometry.Polygon.Core
import           Data.Geometry.Polygon.Extremes
import           Data.Geometry.Vector                          (Vector,
                                                                pattern Vector2)
import           Data.Intersection                             (IsIntersectableWith (intersect))
import           Data.List                                     (sortBy)
import           Data.Vinyl                                    (Rec (RNil, (:&)))
import           Data.Vinyl.CoRec                              (Handler (H),
                                                                match)

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

-- Properties:
--   \poly -> selfIntersections poly == selfIntersections (connectHoles poly)
--   \poly -> area poly == area (connectHoles poly)
connectHoles :: (Ord r, Num r, Fractional r) =>
  MultiPolygon p r -> SimplePolygon () r
connectHoles = connectHolesLinear (Vector2 0 1)

-- | Connect holes with the outer hull by linear cuts.
--
-- \(O(kn)\)
connectHolesLinear :: (Ord r, Num r, Fractional r)
  => Vector 2 r -> MultiPolygon p r -> SimplePolygon () r
connectHolesLinear vec (MultiPolygon border holes) =
    connectHolesLinear' vec (trunc $ SimplePolygon border) sorted
  where
    trunc = first (const ())
    extremes = map (snd . extremesLinearSeq vec) (map trunc holes)
    sorted = sortBy (cmpExtreme vec `on` focus) extremes

connectHolesLinear' :: (Ord r, Num r, Fractional r)
  => Vector 2 r -> SimplePolygon () r -> [CSeq (Point 2 r :+ ())]
  -> SimplePolygon () r
connectHolesLinear' _ border []      = border
connectHolesLinear' vec border (hole:holes) =
    connectHolesLinear' vec newBorder holes
  where
    newBorder = SimplePolygon (C.fromList (openBorder ++ F.toList hole ++ [focus hole]))
    line = undefined
    (_pt, openBorder) =
      F.minimumBy (cmpExtreme vec `on` fst)
      [ (pt, outer)
      | (pt, outer) <- cutPolygon border line
      , cmpExtreme vec (focus hole) pt == LT ]

-- for each hole, find extreme point
-- pick hole with largest extreme
-- cut from that hole to outer poly
-- iterate until no more holes

cutPolygon :: (Fractional r, Ord r) =>
  SimplePolygon () r -> Line 2 r -> [(Point 2 r :+ (), [Point 2 r :+ ()])]
cutPolygon poly line = worker [] (listEdges poly)
  where
    fromSegment (LineSegment' firstPt _) = firstPt
    worker _acc [] = []
    worker acc (segment:segments) =
      let acc' = fromSegment segment : acc
          LineSegment' sStart sEnd = segment
      in match (segment `intersect` line) $
             (H $ const $ worker acc' segments)
          :& (H $ \pt ->
              let toPolyEnd = map fromSegment segments
                  fromPolyStart = reverse acc ++ [sStart]
              in (ext pt, ext pt : sEnd : toPolyEnd  ++ fromPolyStart) :
                  worker acc' segments)
          :& (H undefined)
          :& RNil
