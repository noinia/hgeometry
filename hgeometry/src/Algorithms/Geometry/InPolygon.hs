--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.InPolygon
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Testing if a point lies in a polygon
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.InPolygon
  ( inPolygon
  , insidePolygon
  , onBoundary
  ) where

import           Control.Lens

import           Data.Ext
import qualified Data.Foldable as F
import           Geometry.Boundary
import           Geometry.Line
import           Geometry.LineSegment
import           Geometry.Point
import           Geometry.Polygon.Core
import           Geometry.Properties

import qualified Data.List.Util as List
import           Data.Maybe (mapMaybe)
import           Data.Vinyl.CoRec (asA)
--------------------------------------------------------------------------------

{- $setup
>>> import Data.RealNumber.Rational
>>> import Data.Foldable
>>> import Control.Lens.Extras
>>> :{
-- import qualified Data.Vector.Circular as CV
let simplePoly :: SimplePolygon () (RealNumber 10)
    simplePoly = fromPoints . map ext $
      [ Point2 0 0
      , Point2 10 0
      , Point2 10 10
      , Point2 5 15
      , Point2 1 11
      ]
    simpleTriangle :: SimplePolygon () (RealNumber 10)
    simpleTriangle = fromPoints  . map ext $
      [ Point2 0 0, Point2 2 0, Point2 1 1]
    multiPoly :: MultiPolygon () (RealNumber 10)
    multiPoly = MultiPolygon
      (fromPoints . map ext $ [Point2 (-1) (-1), Point2 3 (-1), Point2 2 2])
      [simpleTriangle]
:} -}


-- | \( O(n) \) Test if q lies on the boundary of the polygon.
--
-- >>> Point2 1 1 `onBoundary` simplePoly
-- False
-- >>> Point2 0 0 `onBoundary` simplePoly
-- True
-- >>> Point2 10 0 `onBoundary` simplePoly
-- True
-- >>> Point2 5 13 `onBoundary` simplePoly
-- False
-- >>> Point2 5 10 `onBoundary` simplePoly
-- False
-- >>> Point2 10 5 `onBoundary` simplePoly
-- True
-- >>> Point2 20 5 `onBoundary` simplePoly
-- False
--
-- TODO: testcases multipolygon
onBoundary        :: (Num r, Ord r) => Point 2 r -> Polygon t p r -> Bool
q `onBoundary` pg = any (q `intersects`) es
  where
    out = pg^.outerBoundary
    es = concatMap (F.toList . outerBoundaryEdges) $ out : holeList pg

-- | Check if a point lies inside a polygon, on the boundary, or outside of the polygon.
-- Running time: O(n).
--
-- >>> Point2 1 1 `inPolygon` simplePoly
-- Inside
-- >>> Point2 0 0 `inPolygon` simplePoly
-- OnBoundary
-- >>> Point2 10 0 `inPolygon` simplePoly
-- OnBoundary
-- >>> Point2 5 13 `inPolygon` simplePoly
-- Inside
-- >>> Point2 5 10 `inPolygon` simplePoly
-- Inside
-- >>> Point2 10 5 `inPolygon` simplePoly
-- OnBoundary
-- >>> Point2 20 5 `inPolygon` simplePoly
-- Outside
--
-- TODO: Add some testcases with multiPolygons
-- TODO: Add some more onBoundary testcases
inPolygon             :: forall t p r. (Fractional r, Ord r)
                      => Point 2 r -> Polygon t p r -> PointLocationResult
q `inPolygon` pg
  | q `onBoundary` pg = OnBoundary
  | inHole            = Outside
  | otherwise         = q `inPolygon'` (pg^.outerBoundary)
  where
    inHole = any (q `insidePolygon`) $ holeList pg

-- | Returns true if the point lies in the polygon
-- pre: point lies inside or outside the polygon, not on its boundary.
inPolygon'        :: forall p r. (Fractional r, Ord r)
                  => Point 2 r -> SimplePolygon p r
                  -> PointLocationResult
q `inPolygon'` pg = if odd . length . mapMaybe intersectionPoint $ ups <> downs
                    then Inside else Outside
  where
    -- we don't care about horizontal edges
    (ups',_horizontals,downs') = partitionEdges . listEdges $ pg
    partitionEdges = List.partition3 $ \s -> (s^.end.core.yCoord) `compare` (s^.start.core.yCoord)

    -- upward edges include start, exclude end
    ups   = map (\(LineSegment' a b) -> LineSegment (Closed a) (Open b)) ups'
    -- downward edges exclude start, include end
    downs = map (\(LineSegment' a b) -> LineSegment (Open a) (Closed b)) downs'

    -- Given an edge, compute the intersection point (if a point) with
    -- the line through the query point, and test if it lies strictly
    -- right of q.
    --
    -- See http://geomalgorithms.com/a03-_inclusion.html for more information.
    intersectionPoint =  F.find (\p -> p^.xCoord > q^.xCoord) . asA @(Point 2 r) . (`intersect` l)
    l = horizontalLine @Line @r $ q^.yCoord


-- | Test if a point lies strictly inside the polgyon.
insidePolygon        :: (Fractional r, Ord r) => Point 2 r -> Polygon t p r -> Bool
q `insidePolygon` pg = q `inPolygon` pg == Inside


-- testQ = map (`inPolygon` testPoly) [ Point2 1 1    -- Inside
--                                    , Point2 0 0    -- OnBoundary
--                                    , Point2 5 14   -- Inside
--                                    , Point2 5 10   -- Inside
--                                    , Point2 10 5   -- OnBoundary
--                                    , Point2 20 5   -- Outside
--                                    ]

-- testPoly :: SimplePolygon () Rational
-- testPoly = fromPoints . map ext $ [ Point2 0 0
--                                                   , Point2 10 0
--                                                   , Point2 10 10
--                                                   , Point2 5 15
--                                                   , Point2 1 11
--                                                   ]
