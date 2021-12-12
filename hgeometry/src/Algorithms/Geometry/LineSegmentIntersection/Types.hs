{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.LineSegmentIntersection.Types
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Algorithms.Geometry.LineSegmentIntersection.Types where

import           Control.DeepSeq
import           Control.Lens
import           Data.Ext
import           Data.Geometry.Interval
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import qualified Data.Map as Map
import           GHC.Generics

--------------------------------------------------------------------------------

-- | The line segments that contain a given point p may either have p
-- as the endpoint or have p in their interior.
--
-- if somehow the segment is degenerate, and p is both the start and
-- end it is reported only as the start point.
data Associated p r =
  Associated { _startPointOf :: [LineSegment 2 p r] -- ^ segments for which the intersection point is the start point (i.e. s^.start.core == p)
             , _endPointOf   :: [LineSegment 2 p r] -- ^ segments for which the intersection point is the end point (i.e. s^.end.core == p)
             , _interiorTo   :: [LineSegment 2 p r]
             } deriving (Show, Eq, Generic)
makeLenses ''Associated

-- | Reports whether this associated has any interior intersections
--
-- \(O(1)\)
isInteriorIntersection :: Associated p r -> Bool
isInteriorIntersection = not . null . _interiorTo

-- | test if the given segment has p as its endpoint, an construct the
-- appropriate associated representing that.
mkAssociated                :: (Eq r) => Point 2 r -> LineSegment 2 p r -> Associated p r
mkAssociated p s@(LineSegment a b)
  | p == a^.unEndPoint.core = Associated [s] [] []
  | p == b^.unEndPoint.core = Associated [] [s] []
  | otherwise               = mempty


instance Semigroup (Associated p r) where
  (Associated ss es is) <> (Associated ss' es' is') = Associated (ss <> ss') (es <> es') (is <> is')

instance Monoid (Associated p r) where
  mempty = Associated mempty mempty mempty

instance (NFData p, NFData r) => NFData (Associated p r)

-- | For each intersection point the segments intersecting there.
type Intersections p r = Map.Map (Point 2 r) (Associated p r)

-- | An intersection point together with all segments intersecting at this point.
data IntersectionPoint p r =
  IntersectionPoint { _intersectionPoint :: !(Point 2 r)
                    , _associatedSegs    :: !(Associated p r)
                    } deriving (Show,Eq)
makeLenses ''IntersectionPoint



-- newtype E a b = E (a -> b)
