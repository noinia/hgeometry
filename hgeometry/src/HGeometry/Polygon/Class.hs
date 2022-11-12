{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Class
  ( HasOuterBoundary(..)
  , signedArea2X
  , minimumVertexBy, maximumVertexBy
  -- , outerBoundaryEdgeSegments
  , outerBoundaryWithNeighbours

  , Polygon_(..)
  ) where

import Control.Lens
import Control.Lens.Internal.Fold (NonEmptyDList(..))
import Data.Functor.Apply (Apply)
import Data.Functor.Contravariant (phantom)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup.Foldable
-- import HGeometry.LineSegment.Class
import HGeometry.Point.Class
import HGeometry.Vector.Class
import Hiraffe.Graph

--------------------------------------------------------------------------------

-- ^ A class for items that have an outer boundary.
class HasVertices polygon polygon => HasOuterBoundary polygon where
  -- | A fold over all vertices of the outer boundary, the
  -- vertices are traversed in CCW order.
  --
  -- running time :: \(O(n)\)
  outerBoundary :: IndexedFold1 (VertexIx polygon) polygon (Vertex polygon)

  -- | A particular vertex of the outer polygon
  --
  -- running time: \(O(1)\)
  outerBoundaryVertexAt   :: VertexIx polygon
                          -> IndexedGetter (VertexIx polygon) polygon (Vertex polygon)

  -- | A fold over all edges in the polygon. The edges are folded over
  -- in CCW order, and each edge is associated with the index of its start vertex
  -- outerBoundaryEdges :: IndexedFold (VertexIx polygon) polygon (Vertex polygon, Vertex polygon)
  --
  --
  -- running time :: \(O(n)\)
  outerBoundaryEdges :: IndexedFold1 (VertexIx polygon) polygon (Vertex polygon, Vertex polygon)


  {-# MINIMAL outerBoundary, outerBoundaryVertexAt #-}

  default outerBoundaryEdges
    :: Enum (VertexIx polygon)
    => IndexedFold1 (VertexIx polygon) polygon (Vertex polygon, Vertex polygon)
  outerBoundaryEdges = ifolding1 $
    \pg -> ( \(i,u) -> (i,(u, pg ^.outerBoundaryVertexAt (succ i))) )
         <$> itoNonEmptyOf outerBoundary pg
    -- \pg -> fmap ( \(i,u) -> (i,(u, pg ^.outerBoundaryVertexAt (succ i))) )
    --      . NonEmpty.fromList
    --      $ itoListOf outerBoundary pg
    -- this feels much more clunky than it should be somehow.
    -- I would like an 'itoNonEmptyOf'

  -- | Get the edge that has the given vertex as its starting edge
  --
  -- running time: \(O(1)\)
  outerBoundaryEdgeAt   :: VertexIx polygon
                        -> IndexedGetter (VertexIx polygon) polygon
                                         (Vertex polygon, Vertex polygon)
  -- default implementation of outerBoundaryEdge. It achieves the
  -- desired running time when indexing is indeed constant.
  default outerBoundaryEdgeAt :: Enum (VertexIx polygon)
                              => VertexIx polygon
                              -> IndexedGetter (VertexIx polygon) polygon
                                               (Vertex polygon, Vertex polygon)
  outerBoundaryEdgeAt i = ito $
    \pg -> (i, (pg^.outerBoundaryVertexAt i, pg^.outerBoundaryVertexAt (succ i)))


--------------------------------------------------------------------------------
-- end of te HasOuterBoundary class
--------------------------------------------------------------------------------

-- | Version of ifolding to build an 'IndexedFold1'
--
-- taken and modified directly from lens
ifolding1       :: (Foldable1 f, Indexable i p, Contravariant g, Apply g)
                => (s -> f (i, a)) -> Over p g s t a b
ifolding1 sfa f = phantom . traverse1_ (phantom . uncurry (indexed f)) . sfa
{-# INLINE ifolding1 #-}

-- | indexed version of 'toNonEmptyOf'
itoNonEmptyOf   :: IndexedGetting i (NonEmptyDList (i,a)) s a -> s -> NonEmpty (i,a)
itoNonEmptyOf l = flip getNonEmptyDList [] . ifoldMapOf l (\i a -> NonEmptyDList $ ((i,a) :|))


--------------------------------------------------------------------------------
-- * HasOuterBoundary Helpers

-- | Yield the minimum  vertex of a polygon according to the given comparison function.
--
-- running time \( O(n) \)
minimumVertexBy     :: (HasOuterBoundary polygon)
                    => (Vertex polygon -> Vertex polygon -> Ordering)
                    -> polygon
                    -> Vertex polygon
minimumVertexBy cmp = fromMaybe (error "minimumVertexBy: absurd")
                    . minimumByOf outerBoundary cmp

-- | Yield the maximum vertex of a polygon according to the given comparison function.
--
-- running time \( O(n) \)
maximumVertexBy     :: (HasOuterBoundary polygon)
                    => (Vertex polygon -> Vertex polygon -> Ordering)
                    -> polygon
                    -> Vertex polygon
maximumVertexBy cmp = fromMaybe (error "maximumVertexBy: absurd")
                    . maximumByOf outerBoundary cmp



-- | Compute the signed area times 2 of a simple polygon. The the
-- vertices are in clockwise order, the signed area will be negative,
-- if the verices are given in counter clockwise order, the area will
-- be positive.
--
-- running time: \(O(n)\)
signedArea2X      :: (Num r, HasOuterBoundary simplePolygon
                     , Point_ point 2 r
                     , Vertex simplePolygon ~ point
                     ) => simplePolygon -> r
signedArea2X poly = sum [ p^.xCoord * q^.yCoord - q^.xCoord * p^.yCoord
                        | (p,q) <- poly ^..outerBoundaryEdges ]

{-

-- | Get the line segments representing the outer boundary of the polygon.
outerBoundaryEdgeSegments :: forall lineSegment polygon point r.
                             ( LineSegment_ lineSegment 2 point r
                             , HasOuterBoundary polygon
                             , Vertex polygon ~ point 2 r
                             )
                          => IndexedFold1 (VertexIx polygon) polygon (lineSegment 2 point r)
outerBoundaryEdgeSegments = outerBoundaryEdges . to toSeg
  -- ifolding1
  --                         $ over (mapped._2) toSeg . itoNonEmptyOf outerBoundaryEdges
  where
    toSeg :: (Vertex polygon, Vertex polygon) -> lineSegment 2 point r
    toSeg = uncurry uncheckedLineSegment

-}

-- | A fold that associates each vertex on the boundary with its
-- predecessor and successor (in that order) along the boundary.
outerBoundaryWithNeighbours :: ( HasOuterBoundary polygon
                               , Enum (VertexIx polygon)
                               )
                            =>  IndexedFold1 (VertexIx polygon)
                                        polygon
                                        (Vertex polygon, (Vertex polygon, Vertex polygon))
outerBoundaryWithNeighbours = ifolding1 $
    \pg -> (\(i,u) -> (i, f pg i u)) <$> itoNonEmptyOf outerBoundary pg
  where
    f pg i u = let v = pg^.outerBoundaryVertexAt (pred i)
                   w = pg^.outerBoundaryVertexAt (succ i)
               in (u, (v, w))


--------------------------------------------------------------------------------

-- class HasHoles face face' where
--   type HoleIx face
--   type Hole face
--   holes :: IndexedTraversal (HoleIx face) face face' (Hole face) (Hole face')

--------------------------------------------------------------------------------

-- | A class representing (planar) polygons. The edges of the polygon
-- may not intersect.
class ( HasOuterBoundary polygon
      , Vertex      polygon ~ point
      , Point_ point 2 r
      ) => Polygon_ polygon point r where

  -- | The area of a polygon
  --
  -- running time: \(O(n)\)
  area :: Fractional r => polygon -> r


  -- | Finds the extreme points, minimum and maximum, in a given direction
  --
  -- running time: \(O(n)\)
  extremes      :: (Num r, Ord r, Point_ point 2 r, VectorFor point ~ vector)
                => vector -> polygon -> (point, point)
  extremes u pg = ( minimumVertexBy (cmpExtreme u) pg
                  , maximumVertexBy (cmpExtreme u) pg
                  )

--------------------------------------------------------------------------------
-- end of te Polygon_ class
--------------------------------------------------------------------------------

-- | Comparison that compares which point is 'larger' in the direction given by
-- the vector u.
cmpExtreme       :: (Num r, Ord r, Point_ point 2 r, VectorFor point ~ vector)
                 => vector -> point -> point -> Ordering
cmpExtreme u p q = u `dot` (p .-. q) `compare` 0

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- data MultiPG
-- data SimplePG

-- class DerivingStrategyPolygon strat polygon point r | polygon point r -> strat where
--   derivedArea :: Fractional r => polygon point r -> r

-- instance (SimplePolygon_ polygon point r) =>  DerivingStrategyPolygon SimplePG polygon point r where
--   derivedArea = signedArea -- abs . signedArea
--                 -- since the polygon is stored in CCW order, there is no need to actually
--                 -- use the absolute value.


{-
-}
