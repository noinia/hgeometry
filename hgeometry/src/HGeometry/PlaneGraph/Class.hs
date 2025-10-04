{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlaneGraph.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Type type for planar graphs embedded in \(\mathbb{R}^2\). For functions that
-- export faces and edges etc, we assume the graph has a (planar) straight line
-- embedding.
--
--------------------------------------------------------------------------------
module HGeometry.PlaneGraph.Class
  ( PlaneGraph_(..)
  , ConstructablePlaneGraph_(..)
  -- , HasLocation(..)
  , defaultOuterFaceDart

  , dartSegmentAt
  , edgeSegmentAt
  , dartSegments
  , edgeSegments

  , interiorFacePolygonAt
  , interiorFacePolygons

  , outerBoundaryPolygonAt
  , outerBoundaryPolygons
  ) where

import           Control.Lens
import           Data.Coerce
import           Data.Foldable1
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Endo(..))
import           Data.Ord (comparing)
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.Polygon.Simple
import           HGeometry.Lens.Util
import           HGeometry.Polygon.WithHoles
import           HGeometry.Properties
import           HGeometry.Vector
import           Hiraffe.Graph.Class
import           Hiraffe.PlanarGraph.Class
import qualified Data.Vector.NonEmpty as NonEmptyV

--------------------------------------------------------------------------------

-- | A class representing Plane graphs, i.e. planar graphs that have a straight line
-- embedding in the plane.
class ( PlanarGraph_ planeGraph
      , vertex ~ Vertex planeGraph
      , Point_ vertex 2 (NumType vertex)
      , NumType vertex ~ NumType planeGraph
      -- , HasVertices graph graph
      , HasEdges planeGraph planeGraph
      , HasOuterFace planeGraph
      ) => PlaneGraph_ planeGraph vertex | planeGraph -> vertex where
  {-# MINIMAL #-}

-- | A class representing constructable Plane graphs, i.e. planar graphs that have a
-- straight line embedding in the plane.
class PlaneGraph_ planeGraph vertex => ConstructablePlaneGraph_ planeGraph vertex where
  {-# MINIMAL fromEmbedding #-}

  -- | Build a graph from its embedding; i.e. for each vertex we expect its adjacencies in
  -- CCW order.
  --
  -- If the, in the list of neighbours of vertex u we see a vertex v
  -- that itself does not appear in the adjacencylist, we may drop
  -- it. In other words if u has a neighbour v, then v better have a
  -- specification of its neighbours somewhere.
  fromEmbedding :: ( Foldable1 f, Functor f, Foldable h, Functor h
                   , vi ~ VertexIx planeGraph
                   , v ~ Vertex planeGraph
                   , e ~ Edge planeGraph
                   , GraphFromAdjListExtraConstraints planeGraph h
                   ) => f (vi, v, h (vi, e)) -> planeGraph

--------------------------------------------------------------------------------


-- | Computes a dart that is incident to the outer face (i.e. in
-- | particular, that has the outer face to its left.)
--
-- running time: \(O(n)\)
defaultOuterFaceDart   :: ( PlanarGraph_ planeGraph
                          , vertex ~ Vertex planeGraph, r ~ NumType planeGraph
                          , Point_ vertex 2 r
                          , Ord r, Num r
                          )
                       => planeGraph -> DartIx planeGraph
defaultOuterFaceDart pg = minimum1ByOf (outgoingDartsOf vi.asIndex) cmp pg
    where
      (vi,v) = minimum1ByOf (vertices.withIndex) (comparing (^._2.asPoint)) pg
          -- compare lexicographically; i.e. if same x-coord prefer the one with the lowest one
      cmp d1 d2 = cwCmpAroundWith (Vector2 (-1) 0) v (pg^.headOf d1) (pg^.headOf d2)

      -- based on the approach sketched at https://cstheory.stackexchange.com/questions/27586/finding-outer-face-in-plane-graph-embedded-planar-graph
      -- basically: find the leftmost vertex, find the incident edge with the largest slope
      -- and take the face left of that edge. This is the outerface.
      -- note that this requires that the edges are straight line segments
      --
      -- note that rather computing slopes we just ask for the first
      -- vertec cw vertex around v. First with respect to some direction
      -- pointing towards the left.


--------------------------------------------------------------------------------

-- | Get the line segment representing a particular dart.
--
-- Note that this is a fold rather than a getter for the same reason dartAt is a traversal
-- rather than a lens: i.e. if you pass some nonsensical DartIx the dart may not exist.
dartSegmentAt    :: forall planeGraph vertex.
                    ( PlaneGraph_ planeGraph vertex
                    , Point_ vertex 2 (NumType vertex)
                    )
                 => DartIx planeGraph
                 -> IndexedFold (DartIx planeGraph)
                                planeGraph
                                (ClosedLineSegment vertex)
dartSegmentAt ei = theFold
  where
    theFold pSegFSeg g = dartAt ei pDartFDart g
      where
        pDartFDart = dimap dartToSeg fSegTofDart pSegFSeg
        dartToSeg _ = uncurry ClosedLineSegment $ g^.endPointsOf ei
        fSegTofDart = contramap dartToSeg

-- | Get the line segment representing a particular edge.
--
-- Note that this is a fold rather than a getter for the same reason edgeAt is a traversal
-- rather than a lens: i.e. if you pass some nonsensical EdgeIx the edge may not exist.
edgeSegmentAt    :: forall planeGraph vertex.
                    ( PlaneGraph_ planeGraph vertex
                    , Point_ vertex 2 (NumType vertex)
                    )
                 => EdgeIx planeGraph
                 -> IndexedFold (EdgeIx planeGraph)
                                planeGraph
                                (ClosedLineSegment vertex)
edgeSegmentAt ei = theFold
  where
    theFold pSegFSeg g = edgeAt ei pEdgeFEdge g
      where
        pEdgeFEdge = dimap edgeToSeg (contramap edgeToSeg) pSegFSeg
        edgeToSeg _ = uncurry ClosedLineSegment $ g^.endPointsOf (getPositiveDart g ei)
  -- see dartSegment for more info.

-- | Renders all darts as line segments. Thesegments are all oriented in the direction of
-- the dart.
dartSegments :: forall planeGraph vertex.
                ( PlaneGraph_ planeGraph vertex
                , Point_ vertex 2 (NumType vertex)
                )
             => IndexedFold (DartIx planeGraph) planeGraph (ClosedLineSegment vertex)
dartSegments = theFold
  where
    theFold            :: forall p f.
                          ( Indexable (DartIx planeGraph) p, Applicative f, Contravariant f)
                       => p (ClosedLineSegment vertex) (f (ClosedLineSegment vertex))
                       -> planeGraph
                       -> f planeGraph
    theFold pSegFSeg g = darts (Indexed draw) g
      where
        draw      :: DartIx planeGraph -> Dart planeGraph -> f (Dart planeGraph)
        draw d _ = let seg = uncurry ClosedLineSegment $ g^.endPointsOf d
                   in seg >$ indexed pSegFSeg d seg

-- | Renders all edges as line segments.
edgeSegments :: forall planeGraph vertex.
                ( HasEdges planeGraph planeGraph
                , BidirGraph_ planeGraph
                , vertex ~ Vertex planeGraph
                , Point_ vertex 2 (NumType vertex)
                )
             => IndexedFold (EdgeIx planeGraph) planeGraph (ClosedLineSegment vertex)
edgeSegments = theFold
  where
    theFold            :: forall p f.
                          ( Indexable (EdgeIx planeGraph) p, Applicative f, Contravariant f)
                       => p (ClosedLineSegment vertex) (f (ClosedLineSegment vertex))
                       -> planeGraph
                       -> f planeGraph
    theFold pSegFSeg g = edges (Indexed draw) g
      where
        draw      :: EdgeIx planeGraph -> Edge planeGraph -> f (Edge planeGraph)
        draw ei _ = let seg = uncurry ClosedLineSegment $ g^.endPointsOf (getPositiveDart g ei)
                    in seg >$ indexed pSegFSeg ei seg

--------------------------------------------------------------------------------



-- instance HasInnerComponents (PlanarGraph w s v e f) where
--   innerComponentsAt fi = undefined
--    -- TODO: implement this

-- | Renders all interior faces as polygons (which may possibly contain holes)
interiorFacePolygons :: forall planeGraph vertex r.
                        ( PlaneGraph_ planeGraph vertex, HasOuterBoundaryOf planeGraph
                        , HasInnerComponents planeGraph
                        , Point_ vertex 2 r
                        , Ord r, Num r
                        , Eq (FaceIx planeGraph)
                        )
                     => IndexedFold (FaceIx planeGraph)
                                    planeGraph
                                    (PolygonalDomain (vertex :+ VertexIx planeGraph))
interiorFacePolygons = theFold
  where
    theFold              :: forall p f.
                            ( Indexable (FaceIx planeGraph) p, Applicative f, Contravariant f)
                         => p (PolygonalDomain (vertex :+ VertexIx planeGraph))
                              (f (PolygonalDomain (vertex :+ VertexIx planeGraph)))
                         -> planeGraph -> f planeGraph
    theFold pPolyFPoly g = interiorFaces (Indexed draw) g
      where
        draw      :: FaceIx planeGraph -> Face planeGraph -> f (Face planeGraph)
        draw fi _ = let poly = polygonFromFace g fi
                    in poly >$ indexed pPolyFPoly fi poly

-- | Render a given interior face as a polygon (which may cotnain holes)
interiorFacePolygonAt    :: forall planeGraph vertex.
                            ( PlaneGraph_ planeGraph vertex
                            , HasOuterBoundaryOf planeGraph
                            , HasInnerComponents planeGraph
                            , Point_ vertex 2 (NumType vertex)
                            )
                         => FaceIx planeGraph
                         -> IndexedFold (FaceIx planeGraph)
                                        planeGraph
                                        (PolygonalDomain (vertex :+ VertexIx planeGraph))
interiorFacePolygonAt fi = theFold
  where
    theFold pPolyFPoly gr = faceAt fi draw gr
      where
        -- draw   :: Face planeGraph -> f (Face planeGraph)
        draw _ = let poly = polygonFromFace gr fi
                 in poly >$ indexed pPolyFPoly fi poly

-- | The actual code that constructs the polygonal domain
polygonFromFace      :: forall planeGraph vertex r.
                        ( PlaneGraph_ planeGraph vertex
                        , HasOuterBoundaryOf planeGraph
                        , HasInnerComponents planeGraph
                        , Point_ vertex 2 r
                        )
                     => planeGraph -> FaceIx planeGraph
                     -> PolygonalDomain (vertex :+ VertexIx planeGraph)
polygonFromFace gr fi =
    PolygonalDomain (simplePolygonFromFace gr fi)
                    (mkHole <$> toVectorOf (innerComponentsAt fi.asIndex) gr)
  where
    mkHole   :: DartIx planeGraph -> SimplePolygon (vertex :+ VertexIx planeGraph)
    mkHole d = uncheckedFromCCWPoints
             . fmap (\d' -> gr^?!tailOf d'.asIndexedExt)
             . NonEmptyV.reverse
             $ toNonEmptyVectorOf (boundaryDartsFrom d.asIndex) gr
     -- make sure we get them in the right order.


----------------------------------------

-- | Renders all interior faces as simple polygons.
outerBoundaryPolygons :: forall planeGraph vertex r.
                        ( PlaneGraph_ planeGraph vertex, HasOuterBoundaryOf planeGraph
                        , Point_ vertex 2 r
                        , Ord r, Num r
                        , Eq (FaceIx planeGraph)
                        )
                     => IndexedFold (FaceIx planeGraph)
                                    planeGraph
                                    (SimplePolygon (vertex :+ VertexIx planeGraph))
outerBoundaryPolygons = theFold
  where
    theFold              :: forall p f.
                            ( Indexable (FaceIx planeGraph) p, Applicative f, Contravariant f)
                         => p (SimplePolygon (vertex :+ VertexIx planeGraph))
                              (f (SimplePolygon (vertex :+ VertexIx planeGraph)))
                         -> planeGraph
                         -> f planeGraph
    theFold pPolyFPoly g = interiorFaces (Indexed draw) g
      where
        draw      :: FaceIx planeGraph -> Face planeGraph -> f (Face planeGraph)
        draw fi _ = let poly = simplePolygonFromFace g fi
                    in poly >$ indexed pPolyFPoly fi poly

-- | Return the outer boundary of a face as a simple polygon
simplePolygonFromFace       :: forall planeGraph vertex r.
                                 ( PlaneGraph_ planeGraph vertex
                                 , HasOuterBoundaryOf planeGraph
                                 , Point_ vertex 2 r
                                 )
                            => planeGraph -> FaceIx planeGraph
                            -> SimplePolygon (vertex :+ VertexIx planeGraph)
simplePolygonFromFace gr fi = poly'&vertices.extra %~ coerce
  where
    poly' :: SimplePolygon (vertex :+ VertexIx planeGraph)
    poly' = uncheckedFromCCWPoints
          . fmap (\vi -> gr^?!vertexAt vi :+ vi)
          $ outerBoundaryVertices fi gr
        -- note that this is safe, since boundaryVerticesOf guarantees that for
        -- interior faces, the vertices are returned in CCW order.

    -- TODO: why can't I just coerce poly' to the right type?


-- | Renders a single interior face as a simple polygon.
--
-- Note that this is a fold rather than a getter for the same reason faceAt is a traversal
-- rather than a lens: i.e. if you pass some nonsensical FaceIx the face may not exist.
outerBoundaryPolygonAt    :: forall planeGraph vertex.
                            ( PlaneGraph_ planeGraph vertex
                            , HasOuterBoundaryOf planeGraph
                            , Point_ vertex 2 (NumType vertex)
                            )
                         => FaceIx planeGraph
                         -> IndexedFold (FaceIx planeGraph)
                                        planeGraph
                                        (SimplePolygon (vertex :+ VertexIx planeGraph))
outerBoundaryPolygonAt fi = theFold
  where
    theFold pPolyFPoly gr = faceAt fi draw gr
      where
        -- draw   :: Face planeGraph -> f (Face planeGraph)
        draw _ = let poly =  simplePolygonFromFace gr fi
                 in poly >$ indexed pPolyFPoly fi poly

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

-- | get the minimum of the elements the lens points to using the given comparison function
minimum1ByOf       :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> Ordering) -> s -> a
minimum1ByOf l cmp = fromMaybe (error "minimum1ByOf") . minimumByOf l cmp


-- -- type Getting r s a = (a -> Const r a) -> s -> Const r s

-- -- Endo a ===  a -> a
-- --
-- -- Endo (Endo (Maybe a)))
-- --
-- -- so r is here (Maybe a -> Maybe a) -> (Maybe a -> Maybe a)



-- -- Getting : a -> a -> a




-- -- | get the minimum of the elements the lens points to using the given comparison function
-- minimum1ByOf       :: Getting (Endo (Endo a)) s a
--                    -> (a -> a -> Ordering) -> s -> a
-- minimum1ByOf l cmp =


--   -- Endo (Endo (Maybe a)))


-- -- foldlOf' l mf Nothing where
-- --   mf Nothing y = Just $! y
-- --   mf (Just x) y = Just $! if cmp x y == GT then y else x
-- -- {-# INLINE minimumByOf #-}


-- --   fromMaybe (error "minimum1ByOf") . minimumByOf l cmp




-- -- foldl1Of'X        :: Getting (Endo (Endo a)) s a -> (a -> a -> a) -> s -> a
-- -- foldl1Of'X l f xs = (foldlOf' l mf Nothing xs) where
-- --   mf Nothing y = Just $! y
-- --   mf (Just x) y = Just $! f x y
-- -- {-# INLINE foldl1Of'X #-}


-- -- foldlOf' :: Getting (Endo (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
-- -- foldlOf' l f z0 xs = foldrOf l f' (Endo id) xs `appEndo` z0
-- --   where f' x (Endo k) = Endo $ \z -> k $! f z x
-- -- {-# INLINE foldlOf' #-}

-- -- foldMap1Of :: Getting r s a -> (a -> r) -> s -> r


-- foldrOfX :: Getting (Endo a) s a -> (a -> a -> a) -> a -> s -> a
-- foldrOfX l f z = flip appEndo z . foldMapOf l (Endo #. f)
-- {-# INLINE foldrOf #-}


-- -- type Getting r s a = (a -> Const r a) -> s -> Const r s

-- foldrMap1Of          :: Getting _ s a -> (a -> b) -> (a -> b -> b) -> s -> b
-- foldrMap1Of l f0 k s =
--   -- I guess generally, we want to maintain some function :: b -> b
--   -- that we in the end apply to (f0 x0)




-- foldr1Of   :: Getting _ s a -> (a -> a -> a) -> s -> a
-- foldr1Of l = foldrMap1Of l id

-- -- foldr1OfX :: Getting (Endo a) s a -> (a -> a -> a) -> s -> a
-- -- foldr1OfX l f xs = fromMaybe (error "foldr1Of: empty structure")
-- --                             (foldrOf l mf Nothing xs) where
-- --   mf x my = Just $ case my of
-- --     Nothing -> x
-- --     Just y -> f x y
-- -- {-# INLINE foldr1Of #-}
