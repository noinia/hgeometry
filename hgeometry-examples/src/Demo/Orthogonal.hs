module Demo.Orthogonal where

import Data.Ext
import Data.Geometry
import Data.Geometry.Polygon
import Data.Geometry.Ipe

import Data.Geometry.LineSegment
import Data.Geometry.SegmentTree
import qualified Data.Map as Map

-- Given a set of horizontal line segments, preprocess them for vertical
-- line-segment intersection
--
-- We compare four ways of doing this
--
-- 1) Segment Tree + 1D RangeTree (implemented as a Data.Map)
-- 2) Interval Tree + Priority Search Tree
-- 3) 3D-Range Tree
-- 4) 3D KD-Tree

-- 5) SegmenTree + 1D RangeTree (implemented by Data.Geometry.RangeTree)

--------------------------------------------------------------------------------

newtype RangeMap r = RangeMap (Map.Map r (LineSegment 2 () r))
                     deriving (Show,Eq)

newtype SegTree r = SegTree (SegmentTree (RangeMap r) r)
                    deriving (Show,Eq)

-- instance Measured
poly :: SimplePolygon () Rational
poly = (fromPoints . map ext) [Point2 61 1500, Point2 89 2200, Point2 82.5 2200, Point2 68 1950, Point2 52.5 1500]


tst = sqDistanceToArg (Point2 70 2100) . supportingLine <$> outerBoundaryEdges poly
