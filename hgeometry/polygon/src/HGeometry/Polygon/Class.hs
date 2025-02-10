{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
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

  , Hole
  , HasHoles(..)

  , Polygon_(..)

  , HasVertices(..), HasVertices'(..)
  , HasEdges(..), HasEdges'(..)
  ) where

import           Control.Lens hiding (holes)
import           Data.Function (on)
import qualified Data.Functor.Apply as Apply
import           Data.Kind (Type)
import           Data.Semigroup (First(..))
import           Data.Vector.NonEmpty.Internal (NonEmptyVector)
import           Data.Void
import           HGeometry.Cyclic (Cyclic)
import           HGeometry.Ext
import           HGeometry.Lens.Util
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.Polygon.Simple.Type
import           HGeometry.Properties
import           HGeometry.Triangle
import           HGeometry.Vector
import           Hiraffe.Graph

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

-- | A hole is a simple polygon
type Hole polygon = SimplePolygonF (HoleF polygon) (Vertex polygon)

-- | Accessing the holes in a polygon (if there are any.)
--
-- the default implementation assumes there are no holes
class VertexContainer (HoleF polygon) (Vertex polygon) => HasHoles polygon where
  {-# MINIMAL #-}

  -- | Type we use to index holes.
  type HoleIx polygon :: Type
  type HoleIx polygon = Void

  -- | The functor used in the holes
  type HoleF polygon :: Type -> Type
  type HoleF polygon = Cyclic NonEmptyVector

  -- ^ Traversal over the holes in the polygon. Each hole is a simple polygon.
  holes :: IndexedTraversal' (HoleIx polygon) polygon (Hole polygon)
  holes = \_ pg -> pure pg

  -- ^ Access a particular hole. This is supposed to be an affine traversal.
  holeAt   :: HoleIx polygon -> IndexedTraversal' (HoleIx polygon) polygon (Hole polygon)
  holeAt _ = \_ pg -> pure pg


-- | A class representing (planar) polygons. The edges of the polygon
-- may not intersect.
class ( HasOuterBoundary polygon
      , Vertex      polygon ~ point
      , Point_ point 2 r
      , NumType polygon ~ r, Dimension polygon ~ 2
      , HasHoles polygon
      ) => Polygon_ polygon point r where

    -- signedArea2X pg -


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

instance HasHoles polygon => HasHoles (polygon :+ extra) where
  type HoleIx (polygon :+ extra) = HoleIx polygon
  type HoleF  (polygon :+ extra) = HoleF  polygon
  holes    = core .> holes
  holeAt i = core .> holeAt i

instance Polygon_ polygon point r  => Polygon_ (polygon :+ extra) point r where
  extremes u = extremes u . view core
  ccwPredecessorOf u = core .> ccwPredecessorOf u
  ccwSuccessorOf   u = core .> ccwSuccessorOf   u

--------------------------------------------------------------------------------

instance (Point_ point 2 r, Num r, Eq r) => HasOuterBoundary (Triangle point) where
  outerBoundaryVertexAt i = singular (vertexAt $ i `mod` 3)
  outerBoundary = vertices
  ccwOuterBoundaryFrom i = \pvFv tri -> ifCCW tri id reversed (traverseTriangleFrom i) pvFv tri
  cwOuterBoundaryFrom  i = \pvFv tri -> ifCCW tri reversed id (traverseTriangleFrom i) pvFv tri

-- | Helper to reverse the orientation of the traversal depending on the orientation
-- of the triangle.
ifCCW         :: (Point_ point 2 r, Num r, Eq r)
              => Triangle point
              -> Iso' (Triangle point) (Triangle point)
              -> Iso' (Triangle point) (Triangle point)
              -> IndexedTraversal1' Int (Triangle point) point
              -> IndexedTraversal1' Int (Triangle point) point
ifCCW tri f g t
  | (\x -> x == abs x) . triangleSignedArea2X $ tri = f.t
  | otherwise                                       = g.t

-- | Traverse the boundary of a triangle from the given starting vertex
traverseTriangleFrom   :: forall point. Int -> IndexedTraversal1' Int (Triangle point) point
traverseTriangleFrom i = conjoined trav (itrav . indexed)
  where
    trav :: Apply.Apply f => (point -> f point) -> Triangle point -> f (Triangle point)
    trav f (Triangle a b c) = case i `mod` 3 of
                                0 -> Triangle
                                      <$> f a Apply.<.> f b Apply.<.> f c
                                1 -> (\b' c' a' -> Triangle a' b' c')
                                     <$> f b Apply.<.> f c Apply.<.> f a
                                _ -> (\c' a' b' -> Triangle a' b' c')
                                     <$> f c Apply.<.> f a Apply.<.> f b
    itrav :: Apply.Apply f => (Int -> point -> f point) -> Triangle point -> f (Triangle point)
    itrav f (Triangle a b c) = case i `mod` 3 of
                                 0 -> Triangle
                                      <$> f 0 a Apply.<.> f 1 b Apply.<.> f 2 c
                                 1 -> (\b' c' a' -> Triangle a' b' c')
                                      <$> f 1 b Apply.<.> f 2 c Apply.<.> f 0 a
                                 _ -> (\c' a' b' -> Triangle a' b' c')
                                      <$> f 2 c Apply.<.> f 0 a Apply.<.> f 1 b


-- We need Rectanlge to be an instance of HasVertices' first; but that requires
-- that vertices is a fold rather than a traversal.
-- instance (Point_ point 2 r, Num r) => HasOuterBoundary (Rectangle point) where
--   outerBoundaryVertexAt i = undefined
--   ccwOuterBoundaryFrom i = undefined
--   cwOuterBoundaryFrom i = undefined



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
