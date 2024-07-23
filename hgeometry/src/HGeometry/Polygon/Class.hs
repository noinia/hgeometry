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
  , outerBoundaryEdgeSegmentAt, outerBoundaryEdgeSegments
  , outerBoundaryWithNeighbours

  , Polygon_(..)

  , HasVertices(..), HasVertices'(..)
  , HasEdges(..), HasEdges'(..)
  ) where

import Control.Lens
import Data.Function (on)
import HGeometry.Ext
import HGeometry.Lens.Util
-- import qualified Data.Functor.Apply as Apply
import Data.Semigroup (First(..))
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector
import Hiraffe.Graph


--------------------------------------------------------------------------------
-- ^ A class for items that have an outer boundary.
class HasVertices polygon polygon => HasOuterBoundary polygon where
  {-# MINIMAL outerBoundaryVertexAt, ccwOuterBoundaryFrom, cwOuterBoundaryFrom #-}

  -- | A fold over all vertices of the outer boundary, the
  -- vertices are traversed in CCW order.
  --
  -- running time :: \(O(n)\)
  outerBoundary :: IndexedTraversal1' (VertexIx polygon) polygon (Vertex polygon)
  default outerBoundary :: Num (VertexIx polygon)
                        => IndexedFold1 (VertexIx polygon) polygon (Vertex polygon)
  outerBoundary = ccwOuterBoundaryFrom 0

  -- | A CCW-traversal over all vertices of the outer boundary, starting
  -- from the vertex with the given index.
  --
  -- running time :: \(O(n)\)
  ccwOuterBoundaryFrom :: VertexIx polygon
                       -> IndexedTraversal1' (VertexIx polygon) polygon (Vertex polygon)

  -- | A CW-fold over all vertices of the outer boundary, starting
  -- from the vertex with the given index.
  --
  -- running time :: \(O(n)\)
  cwOuterBoundaryFrom        :: VertexIx polygon
                             -> IndexedTraversal1' (VertexIx polygon) polygon (Vertex polygon)

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
  outerBoundaryEdges :: IndexedFold1 (VertexIx polygon,VertexIx polygon) polygon
                                     (Vertex polygon, Vertex polygon)
  default outerBoundaryEdges
    :: Enum (VertexIx polygon)
    => IndexedFold1 (VertexIx polygon,VertexIx polygon) polygon (Vertex polygon, Vertex polygon)
  outerBoundaryEdges = ifolding1 $
    \pg -> ( \(i,u) -> let (j,v) = pg ^.outerBoundaryVertexAt (succ i).withIndex
                       in ((i,j) , (u,v))
           ) <$> itoNonEmptyOf outerBoundary pg
    -- \pg -> fmap ( \(i,u) -> (i,(u, pg ^.outerBoundaryVertexAt (succ i))) )
    --      . NonEmpty.fromList
    --      $ itoListOf outerBoundary pg
    -- this feels much more clunky than it should be somehow.
    -- I would like an 'itoNonEmptyOf'

  -- | Get the edge that has the given vertex as its starting edge
  --
  -- running time: \(O(1)\)
  outerBoundaryEdgeAt   :: VertexIx polygon
                        -> IndexedGetter (VertexIx polygon, VertexIx polygon) polygon
                                         (Vertex polygon, Vertex polygon)
  -- default implementation of outerBoundaryEdge. It achieves the
  -- desired running time when indexing is indeed constant.
  default outerBoundaryEdgeAt :: Enum (VertexIx polygon)
                              => VertexIx polygon
                              -> IndexedGetter (VertexIx polygon, VertexIx polygon) polygon
                                               (Vertex polygon, Vertex polygon)
  outerBoundaryEdgeAt i = ito $
    \pg -> let (j,v) = pg^.outerBoundaryVertexAt (succ i).withIndex
           in ( (i,j), (pg^.outerBoundaryVertexAt i, v))


--------------------------------------------------------------------------------
-- end of te HasOuterBoundary class
--------------------------------------------------------------------------------

instance HasOuterBoundary polygon => HasOuterBoundary (polygon :+ extra) where
  outerBoundary = core .> outerBoundary
  ccwOuterBoundaryFrom u = core .> ccwOuterBoundaryFrom u
  cwOuterBoundaryFrom u  = core .> cwOuterBoundaryFrom  u
  outerBoundaryVertexAt u = core .> outerBoundaryVertexAt u
  outerBoundaryEdges = core .> outerBoundaryEdges
  outerBoundaryEdgeAt u = core .> outerBoundaryEdgeAt u

--------------------------------------------------------------------------------
-- * HasOuterBoundary Helpers

-- | Yield the minimum  vertex of a polygon according to the given comparison function.
--
-- running time \( O(n) \)

-- | Yield the minimum  vertex of a polygon according to the given comparison function.
--
-- running time \( O(n) \)
minimumVertexBy     :: forall polygon. (HasOuterBoundary polygon)
                    => (Vertex polygon -> Vertex polygon -> Ordering)
                    -> IndexedFold1 (VertexIx polygon) polygon (Vertex polygon)
minimumVertexBy cmp = conjoined go igo
  where
    go :: Fold1 polygon (Vertex polygon)
    go = folding1 $ f outerBoundary cmp

    igo :: IndexedFold1 (VertexIx polygon) polygon (Vertex polygon)
    igo = ifolding1 $ f (outerBoundary.withIndex) (cmp `on` snd)

    f boundary cmp' = maybe (error "minimumVertexBy: absurd") First
                    . minimumByOf boundary cmp'

-- | Yield the maximum vertex of a polygon according to the given comparison function.
--
-- running time \( O(n) \)
maximumVertexBy     :: forall polygon. (HasOuterBoundary polygon)
                    => (Vertex polygon -> Vertex polygon -> Ordering)
                    -> IndexedFold1 (VertexIx polygon) polygon (Vertex polygon)
maximumVertexBy cmp = conjoined go igo
  where
    go :: Fold1 polygon (Vertex polygon)
    go = folding1 $ f outerBoundary cmp

    igo :: IndexedFold1 (VertexIx polygon) polygon (Vertex polygon)
    igo = ifolding1 $ f (outerBoundary.withIndex) (cmp `on` snd)

    f boundary cmp' = maybe (error "maximumVertexBy: absurd") First
                    . maximumByOf boundary cmp'

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

-- | Get the line segment representing the edge that has the given vertex as its starting edge
--
-- running time: \(O(1)\)
outerBoundaryEdgeSegmentAt   :: ( HasOuterBoundary polygon
                                , Vertex polygon ~ point
                                , Point_ point 2 r
                                )
                             => VertexIx polygon
                             -> IndexedGetter (VertexIx polygon, VertexIx polygon)
                                              polygon
                                              (ClosedLineSegment point)
outerBoundaryEdgeSegmentAt i = outerBoundaryEdgeAt i. to (uncurry ClosedLineSegment)

-- | Get the line segments representing the outer boundary of the polygon.
outerBoundaryEdgeSegments :: forall polygon point r.
                             ( HasOuterBoundary polygon
                             , Vertex polygon ~ point
                             , Point_ point 2 r
                             )
                          => IndexedFold1 (VertexIx polygon,VertexIx polygon)
                                          polygon
                                          (ClosedLineSegment point)
outerBoundaryEdgeSegments = outerBoundaryEdges . to (uncurry ClosedLineSegment)


-- | A fold that associates each vertex on the boundary with its
-- predecessor and successor (in that order) along the boundary.
outerBoundaryWithNeighbours :: ( HasOuterBoundary polygon
                               , VertexIx polygon ~ Int
                               )
                            =>  IndexedFold1 (VertexIx polygon,
                                                  (VertexIx polygon, VertexIx polygon))
                                        polygon
                                        (Vertex polygon, (Vertex polygon, Vertex polygon))
outerBoundaryWithNeighbours = ifolding1 $
    \pg -> f pg (numVertices pg) <$> itoNonEmptyOf outerBoundary pg
  where
    f pg n (i,u) = let (j,v) = pg^.outerBoundaryVertexAt ((i-1) `mod` n).withIndex
                       (k,w) = pg^.outerBoundaryVertexAt ((i+1) `mod` n).withIndex
                   in ( (i, (j,k)) , (u, (v, w)) )

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
      , NumType polygon ~ r, Dimension polygon ~ 2
      ) => Polygon_ polygon point r where

  -- | The area of a polygon
  --
  -- running time: \(O(n)\)
  area :: Fractional r => polygon -> r


  -- | Finds the extreme points, minimum and maximum, in a given direction
  --
  -- running time: \(O(n)\)
  extremes      :: (Num r, Ord r, Point_ point 2 r)
                => Vector 2 r -> polygon -> (point, point)
  extremes u pg = ( first1Of (minimumVertexBy (cmpInDirection u)) pg
                  , first1Of (maximumVertexBy (cmpInDirection u)) pg
                  )

  -- | Given a vertexIdx v; get an IndexedLens to access the CCW predecessor of v
  ccwPredecessorOf :: VertexIx polygon
                   -> IndexedLens' (VertexIx polygon) polygon (Vertex polygon)

  -- | Given a vertexIdx v; get an IndexedLens to access the CCW predecessor of v
  ccwSuccessorOf :: VertexIx polygon
                 -> IndexedLens' (VertexIx polygon) polygon (Vertex polygon)


--------------------------------------------------------------------------------
-- end of te Polygon_ class
--------------------------------------------------------------------------------

instance Polygon_ polygon point r  => Polygon_ (polygon :+ extra) point r where
  area = area . view core
  extremes u = extremes u . view core
  ccwPredecessorOf u = core .> ccwPredecessorOf u
  ccwSuccessorOf   u = core .> ccwSuccessorOf   u

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
