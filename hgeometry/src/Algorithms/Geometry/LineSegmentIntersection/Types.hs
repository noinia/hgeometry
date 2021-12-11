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
import           Data.Geometry.Properties
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           GHC.Generics

--------------------------------------------------------------------------------

type Compare a = a -> a -> Ordering

-- | get the endpoints of a line segment
endPoints'   :: (HasEnd s, HasStart s) => s -> (StartCore s, EndCore s)
endPoints' s = (s^.start.core,s^.end.core)


type Set' l =
  Map.Map (Point (Dimension l) (NumType l), Point (Dimension l) (NumType l))
          (NonEmpty l)


-- | The line segments that contain a given point p may either have p
-- as the endpoint or have p in their interior.
data Associated p r = Associated { _endPointOf        :: Set' (LineSegment 2 p r)
                                 , _interiorTo        :: Set' (LineSegment 2 p r)
                                 } deriving (Show, Generic)
makeLenses ''Associated

instance (Eq p, Eq r) => Eq (Associated p r) where
  (Associated es is) == (Associated es' is') = f es es' && f is is'
    where
      f xs ys = and $ zipWith (\(p,pa) (q,qa) -> p == q && pa `sameElements` qa)
                        (Map.toAscList xs) (Map.toAscList ys)

      g = L.nub . NonEmpty.toList
      sameElements (g -> xs) (g -> ys) = L.null $ (xs L.\\ ys) ++ (ys L.\\ xs)


instance (NFData p, NFData r) => NFData (Associated p r)

-- | Constructs an Associated from two lists:
associated       :: Ord r
                 => [LineSegment 2 p r] -- ^ given the segments incident to an endpoint
                 -> [LineSegment 2 p r] -- ^ given the segment interior to the point in question
                 -> Associated p r
associated es is = Associated (f es) (f is)
  where
    f = foldr (\s -> Map.insertWith (<>) (endPoints' s) (s :| [])) mempty


-- endPointOf :: Associated p r -> [LineSegment 2 p r]
-- endPointOf = concatMap NonEmpty.toList . Map.elems . _endPointOf

-- interiorTo :: Associated p r -> [LineSegment 2 p r]
-- interiorTo = concatMap NonEmpty.toList . Map.elems . _interiorTo


instance Ord r => Semigroup (Associated p r) where
  (Associated es is) <> (Associated es' is') = Associated (es <> es') (is <> is')

instance Ord r => Monoid (Associated p r) where
  mempty = Associated mempty mempty
  mappend = (<>)

-- | For each intersection point the segments intersecting there.
type Intersections p r = Map.Map (Point 2 r) (Associated p r)

-- | An intersection point together with all segments intersecting at this point.
data IntersectionPoint p r =
  IntersectionPoint { _intersectionPoint :: !(Point 2 r)
                    , _associatedSegs    :: !(Associated p r)
                    } deriving (Show,Eq)
makeLenses ''IntersectionPoint


-- | reports true if there is at least one segment for which this intersection
-- point is interior.
--
-- \(O(1)\)
isEndPointIntersection :: Associated p r -> Bool
isEndPointIntersection = Map.null . _interiorTo


-- newtype E a b = E (a -> b)
