--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Convex.Unbounded
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A type for representing unbounded, Convex, polygonal regions.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Convex.Unbounded
  ( UnboundedConvexRegion
  , UnboundedConvexRegionF(..)
  , extremalVertices
  , mapChain
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           HGeometry.HalfSpace
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Point
import           HGeometry.Polygon
import           HGeometry.Properties
import           HGeometry.Triangle
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | An unbounded polygonal Convex Region
type UnboundedConvexRegion vertex = UnboundedConvexRegionF (NumType vertex) NonEmpty vertex

-- | An unbounded polygonal ConvexRegion whose vertices are stored in an 'nonEmpty'
data UnboundedConvexRegionF r nonEmpty vertex =
  Unbounded (Vector 2 r)
            -- ^ vector indicating the direction of the unbounded edge
            -- incident to the first vertex. Note that this vector
            -- thus points INTO vertex v.
            (nonEmpty vertex)
            -- ^ the vertices in CCW order,
            (Vector 2 r)
            -- ^ the vector indicating the direction of the unbounded
            -- edge incident to the last vertex. The vector points
            -- away from the vertex (i.e. towards +infty).
  deriving stock (Show,Eq,Functor,Foldable,Traversable)

type instance NumType   (UnboundedConvexRegionF r nonEmpty vertex) = r
type instance Dimension (UnboundedConvexRegionF r nonEmpty vertex) = 2

-- | map a function over the sequence of points
mapChain                       :: (nonEmpty vertex -> nonEmpty' vertex')
                               -> UnboundedConvexRegionF r nonEmpty vertex
                               -> UnboundedConvexRegionF r nonEmpty' vertex'
mapChain f (Unbounded v pts w) = Unbounded v (f pts) w

-- | Compute the first and last vertex of the chain. Returns a Left if the first and last
-- are the same.
extremalVertices                            :: UnboundedConvexRegionF r NonEmpty vertex
                                            -> Either vertex (Vector 2 vertex)
extremalVertices (Unbounded _ (p :| pts) _) = case NonEmpty.nonEmpty pts of
                                                Nothing   -> Left p
                                                Just pts' -> Right $ Vector2 p (NonEmpty.last pts')


--------------------------------------------------------------------------------

type instance Intersection (Triangle corner) (UnboundedConvexRegionF r nonEmpty vertex)
  = Intersection (Triangle corner) (ConvexPolygonF nonEmpty vertex)

instance (Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `HasIntersectionWith` (UnboundedConvexRegionF r NonEmpty vertex)

instance ( Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `IsIntersectableWith` (UnboundedConvexRegionF r NonEmpty vertex) where
  tri `intersect` region = tri `intersect` (toBoundedFrom tri region)

-- | convert to a bounded polygon that contains the points given as the first argument.
--
-- note: this creates two new vertices; which are "copies" from the extremal vertices.
-- this is to avoid having to introduce yet another level of 'OriginalOrExtra''s
toBoundedFrom :: (Foldable nonEmpty, Point_ point 2 r, Point_ vertex 2 r, Ord r, Fractional r
                 )
              => nonEmpty point -> UnboundedConvexRegionF r NonEmpty vertex
              -> ConvexPolygonF NonEmpty vertex
toBoundedFrom tri reg@(Unbounded v pts w) = case extremalVertices reg of
    Right (Vector2 p q) -> let l@(LinePV _ u) = lineThrough p q
                               h              = HalfSpace Positive l
                           in compute p q h u
    Left p              -> let bisec = w ^+^ negated v -- note that v is pointing in the wrong dir.
                               u     = rot90 bisec -- perpendicular to bisec
                               h     = HalfSpace Positive (fromPointAndVec p u)
                           in compute p p h u
  where
    -- given the halfpsace h that goes through p and q (the extremal vertices),
    -- and the direction u of its bounding line.
    --
    -- computes two points a and b on the halflines so that tri is contained in the halfspace
    -- defined by a and b (that contains p and q).
    --
    -- it returns a clipped version of the bounded region with a and b as vertices.
    compute p q h u = let s    = view asPoint $ maximumOn (`squaredEuclideanDistTo` h) tri'
                          -- distance to the point furthest from this halfspace.
                          -- we then create two additional on the halflines
                          -- that are at least that distance away.

                          tri' = (q .+^ w)^.asPoint :| ((^.asPoint) <$> F.toList tri)
                          -- make sure that there is at least one point outside h,
                          -- so that s lies stricly outside h as well

                          a = case (fromPointAndVec @(LinePV 2 _) p v) `intersect` (LinePV s u) of
                                Just (Line_x_Line_Point a') -> p&xCoord .~ a'^.xCoord
                                                                &yCoord .~ a'^.yCoord
                                _                           -> error "absurd; a'"
                          b = case (fromPointAndVec @(LinePV 2 _) q w) `intersect` (LinePV s u) of
                                Just (Line_x_Line_Point b') -> q&xCoord .~ b'^.xCoord
                                                                &yCoord .~ b'^.yCoord
                                _                           -> error "absurd; b'"
                      in uncheckedFromCCWPoints (b NonEmpty.<| a NonEmpty.<| pts)


    maximumOn f = maximumBy (comparing f)
    rot90 (Vector2 x y) = Vector2 (-y) x

-- TODO: this would make for a good property test: test if the points all lie inside the reegion

--------------------------------------------------------------------------------
