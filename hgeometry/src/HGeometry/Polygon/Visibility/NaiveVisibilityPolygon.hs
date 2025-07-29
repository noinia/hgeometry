--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Visibility.NaiveVisibilityPolygon
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Naive O(n^2) time algorithm to compute the Visibility polygon in a simple polygon
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Visibility.NaiveVisibilityPolygon
  ( visibilityPolygon
  , Definer, DefinerF(..)
  ) where

import           Control.Lens hiding (views)
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Semigroup
import qualified Data.Vector as Vector
import           HGeometry.Ext
import           HGeometry.Foldable.Sort
import           HGeometry.HalfLine
import           HGeometry.Intersection
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.Polygon
import           HGeometry.Properties

--------------------------------------------------------------------------------

-- | Naive O(n^2) time algorithm to compute the visibility polygon of a point inside a
-- simple polygon.
visibilityPolygon :: forall point vertex polygon r.
                     ( Point_ point 2 r, Point_ vertex 2 r
                     , HasSquaredEuclideanDistance vertex
                     , SimplePolygon_ polygon vertex r
                     , Ord r, Fractional r
                     )
                  => point -> polygon -> SimplePolygon (Definer polygon)
visibilityPolygon (view asPoint -> q) poly = fromMaybe err . fromPoints $ theVertices
  where
    -- the vertices of our visiblity polygon
    theVertices      :: Vector.Vector (DefinerF r (VertexIx polygon) (vertex :+ VertexIx polygon))
    theVertices      = fmap dropIx . sortBy alongBoundary $ originalVertices <> newVertices

    -- all vertices of the original polygon that are visible
    visibleVertices  :: [vertex :+ VertexIx polygon]
    visibleVertices  = poly^..vertices.asIndexedExt.filtered isVisible

    originalVertices
      , newVertices  :: [DefinerF r  (ClosedLineSegment vertex :+ VertexIx polygon)
                                     (vertex :+ VertexIx polygon)
                        ]
    originalVertices = OriginalVertex <$> visibleVertices
    newVertices      = [ w | v <- reflexVertices, w <- maybeToList $ rayThrough v ]

    obstacleEdges :: [ClosedLineSegment vertex :+ VertexIx polygon]
    obstacleEdges = poly^..(reindexed fst outerBoundaryEdgeSegments).asIndexedExt

    -- | test if the vertex is strictly visible; i.e. the open line segment between p and
    -- q lies strictly in the interior of the polygon.
    isStrictlyVisible   :: vertex :+ ix -> Bool
    isStrictlyVisible p =
      all (not . (intersects (OpenLineSegment (p^.asPoint) q)) . view core) obstacleEdges

    -- | test if a given vertex is visible; i.e. if the open linesegment between p and q
    -- lies in the closure of the polygon. This means we should test whether it does not
    -- intersect the interior of any of the edges. Overlap with edges is allowed.
    isVisible   :: vertex :+ ix -> Bool
    isVisible p = flip all obstacleEdges $ \(e :+ _) ->
        case (OpenLineSegment (p^.asPoint) q) `intersect` (asOpenEdge e) of
          Nothing                                        -> True
          Just (LineSegment_x_LineSegment_Point _)       -> False
          Just (LineSegment_x_LineSegment_LineSegment _) -> True

    -- | Compute the set of closest reflex vertices. Note that here we use the strict
    -- visibility so that we only get hte first visible reflex vertex in some direction
    -- (if the ray would go thorugh more vertices)
    reflexVertices :: [vertex :+ VertexIx polygon]
    reflexVertices = poly^..vertices.asIndexedExt.filtered (\v ->
                       isStrictlyVisible v && isInterior v)

    -- | test whether the two neighbours of v are on the same side of the line through q
    -- and v if so; then v is a reflex vertex that defines a new visible vertex.
    --
    -- if either the predecessor or successor is colinear with v, then v counts as a reflex
    -- vertex as well (since v was visible)
    isInterior          :: vertex :+ VertexIx polygon -> Bool
    isInterior (v :+ i) = case ( ccw q (poly^.ccwPredecessorOf i.asPoint) (v^.asPoint)
                               , ccw q (poly^.ccwSuccessorOf i.asPoint) (v^.asPoint)
                               ) of
                            (CoLinear, _)       -> True
                            (_       ,CoLinear) -> True
                            (sideP,   sideS)    -> sideP == sideS

    intersectionPoint     :: vertex :+ ix -> ClosedLineSegment vertex :+ ix
                          -> Maybe (DefinerF r (ClosedLineSegment vertex :+ ix) (vertex :+ ix))
    intersectionPoint v e = case HalfLine (v^.asPoint) ((v^.asPoint) .-. q)
                                 `intersect` (asOpenEdge e) of
      Just (HalfLine_x_LineSegment_Point p) -> Just (NewVertex p e v)
      _                                     -> Nothing

    -- treat e as an open line segment; i.e. we don't want duplicate vertices at endpoints
    -- anyway.
    asOpenEdge e = OpenLineSegment (e^.start.asPoint) (e^.end.asPoint)

    rayThrough   :: vertex :+ VertexIx polygon
                 -> Maybe (DefinerF r (ClosedLineSegment vertex :+ VertexIx polygon)
                                     (vertex :+ VertexIx polygon))
    rayThrough v = minimumOn (squaredEuclideanDistTo q)
                 $ mapMaybe (intersectionPoint v) obstacleEdges


    err = error "visibilityPolygon: absurd"

--------------------------------------------------------------------------------
-- * Data types representing the vertices of a visibility polygon

-- | The second parameter of DefinerF; the edge, actually stores the index of its starting
-- vertex.
type Definer polygon =
  DefinerF (NumType polygon) (VertexIx polygon) (Vertex polygon :+ VertexIx polygon)

-- | A vertex of a visibility polygon; which is either an original vertex of type 'orig' or
-- a vertex that is the intersection point of a ray through an original vertex and an edge.
data DefinerF r edge orig = OriginalVertex orig
                          | NewVertex (Point 2 r) edge orig
                            -- ^ the intersection point on the edge, defined by the edge
                            -- and the original vertex
                          deriving (Show,Eq,Functor)

instance Bifunctor (DefinerF r) where
  bimap f g = \case
    OriginalVertex p -> OriginalVertex (g p)
    NewVertex p e v  -> NewVertex p (f e) (g v)

-- | Lens to access the position of a vertex of a visibility polygon.
--
-- Note that using this as a setter may be unsafe/break invariants!
position :: Point_ orig 2 r => Lens' (DefinerF r edge orig) (Point 2 r)
position = lens (\case
                    OriginalVertex v -> v^.asPoint
                    NewVertex p _ _  -> p
                )
                (\v p@(Point2 x y) -> case v of
                    OriginalVertex v' -> OriginalVertex $ v'&xCoord .~ x
                                                            &yCoord .~ y
                    NewVertex _ e o   -> NewVertex p e o
                )


type instance Dimension (DefinerF r edge orig) = 2
type instance NumType (DefinerF r edge orig)   = r

instance ( HasSquaredEuclideanDistance orig, Point_ orig 2 r
         ) => HasSquaredEuclideanDistance (DefinerF r edge orig) where
  pointClosestTo _ = view asPoint

instance Point_ orig 2 r => HasVector (DefinerF r edge orig) (DefinerF r edge orig) where
  vector = position.vector
instance Point_ orig 2 r => Affine_ (DefinerF r edge orig) 2 r
instance Point_ orig 2 r => Point_ (DefinerF r edge orig) 2 r
instance Point_ orig 2 r => HasCoordinates (DefinerF r edge orig) (DefinerF r edge orig)


dropIx :: DefinerF r (edge :+ x) vertex  -> DefinerF r x vertex
dropIx = bimap (^.extra) id

alongBoundary :: ( Ord ix, Num r, Ord r, HasStart edge vertex, Point_ vertex 2 r
                 , HasSquaredEuclideanDistance vertex
                 )
              => DefinerF r (edge :+ ix) (vertex :+ ix)
              -> DefinerF r (edge :+ ix) (vertex :+ ix) -> Ordering
alongBoundary = comparing getIx
  where
    getIx = \case
      OriginalVertex (_ :+ i) -> (i,0)
      NewVertex p (e :+ i) _  -> (i,squaredEuclideanDistTo p (e^.start))
  -- if the polygon has holes this is not correct

minimumOn   :: (Ord b, Foldable f) => (a -> b) -> f a -> Maybe a
minimumOn f = fmap (\(Min (Arg _ x)) -> x) . foldMap (\x -> Just $ Min $ Arg (f x) x)
