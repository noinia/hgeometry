module Algorithms.Geometry.RayShooting.Naive
  ( firstHit
  , firstHit'


  , firstHitSegments
  , intersectionDistance
  , labelWithDistances
  ) where

import           Control.Lens
import           Data.Bifunctor
import           Data.Ext
import           Data.Geometry.HalfLine
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Intersection
import qualified Data.List as List
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Vinyl.CoRec
import           Data.Vinyl

--------------------------------------------------------------------------------

-- |
--
-- pre: halfline should start in the interior
firstHit     :: (Fractional r, Ord r)
             => HalfLine 2 r
             -> Polygon t p r
             -> LineSegment 2 p r
firstHit ray = fromMaybe err . firstHit' ray
  where
    err = error "Algorithms.Geometry.RayShooting.Naive: no intersections; ray must have started outside the polygon"

-- | Compute the first edge hit by the ray, if it exists
firstHit'        :: (Fractional r, Ord r)
                 => HalfLine 2 r
                 -> Polygon t p r
                 -> Maybe (LineSegment 2 p r)
firstHit' ray pg = fmap (^.core) . firstHitSegments ray . map ext $ listEdges pg


-- | Compute the first segment hit by the ray, if it exists
firstHitSegments     :: (Ord r, Fractional r)
                     => HalfLine 2 r
                     -> [LineSegment 2 p r :+ e]
                     -> Maybe (LineSegment 2 p r :+ e)
firstHitSegments ray = fmap (^.extra) . minimumOn (^.core)
                     . mapMaybe (\(s :+ (md, e)) -> (:+ (s :+ e)) <$> md)
                     . labelWithDistances (ray^.startPoint) ray

minimumOn   :: Ord b => (a -> b) -> [a] -> Maybe a
minimumOn f = \case
  [] -> Nothing
  xs -> Just . List.minimumBy (comparing f) $ xs


-- | Given q, a ray, and a segment s, computes if the
-- segment intersects the initial, rightward ray starting in q, and if
-- so returns the (squared) distance from q to that point together
-- with the segment.
intersectionDistance         :: forall r p. (Ord r, Fractional r)
                             => Point 2 r -> HalfLine 2 r -> LineSegment 2 p r
                             -> Maybe r
intersectionDistance q ray s = match (seg `intersect` ray) $
       H (\NoIntersection                   -> Nothing)
    :& H (\p                                -> Just $ d p)
    :& H (\(LineSegment' (a :+ _) (b :+ _)) -> Just $ d a `min` d b)
    :& RNil
    -- TODO: there is some slight subtility if the segment is open.
  where
    d = squaredEuclideanDist q
    seg = first (const ()) s


-- | Labels the segments with the distance from q to their
-- intersection point with the ray.
labelWithDistances       :: (Ord r, Fractional r)
                         => Point 2 r -> HalfLine 2 r -> [LineSegment 2 p r :+ b]
                         -> [LineSegment 2 p r :+ (Maybe r, b)]
labelWithDistances q ray = map (\(s :+ e) -> s :+ (intersectionDistance q ray s, e))



--------------------------------------------------------------------------------
