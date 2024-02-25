{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometryPlaneGraph.Class
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
  -- , HasLocation(..)
  ) where

import Control.Lens
import Data.Functor.Apply
import Data.Kind (Constraint)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Polygon.Simple
import HGeometry.Properties
import Hiraffe.Graph.Class
import Hiraffe.PlanarGraph.Class

--------------------------------------------------------------------------------

-- -- | Types that have a location field.
-- class HasLocation s t where
--   -- | Lens to access the location
--   location :: Lens s t (Point (Dimension s) (NumType s)) (Point (Dimension t) (NumType t))




class ( PlanarGraph_ planeGraph
      , vertex ~ Vertex planeGraph
      , Point_ vertex 2 (NumType vertex)
      , NumType vertex ~ NumType planeGraph
      ) => PlaneGraph_ planeGraph vertex | planeGraph -> vertex where

  {-# MINIMAL outerFaceDart  #-}

  type OuterFaceIdConstraints planeGraph :: Constraint
  type OuterFaceIdConstraints planeGraph = ( Ord (NumType planeGraph)
                                           , Num (NumType planeGraph)
                                           )

  -- | Getter to access the outer face
  outerFace :: (OuterFaceIdConstraints planeGraph, Eq (FaceIx planeGraph))
            => IndexedLens' (FaceIx planeGraph) planeGraph (Face planeGraph)
  outerFace = singular $ theLens
    where
      theLens pFaceFFace g = faceAt theOuterFaceId pFaceFFace g
        where
          theOuterFaceId = outerFaceId g

  -- | Traversal of all interior faces in the graph
  interiorFaces :: (OuterFaceIdConstraints planeGraph, Eq (FaceIx planeGraph))
                => IndexedTraversal' (FaceIx planeGraph) planeGraph (Face planeGraph)
  interiorFaces = theTraversal
    where
      theTraversal pFaceFFace g = (faces.ifiltered (\i _ -> i /= theOuterFaceId)) pFaceFFace g
        where
          theOuterFaceId = outerFaceId g


  -- | gets the id of the outer face
  --
  outerFaceId    :: OuterFaceIdConstraints planeGraph => planeGraph -> FaceIx planeGraph
  outerFaceId ps = leftFace (outerFaceDart ps) ps

  -- | gets a dart incident to the outer face (in particular, that has the
  -- outerface on its left)
  --
  -- running time: \(O(n)\)
  --
  outerFaceDart    :: OuterFaceIdConstraints planeGraph => planeGraph -> DartIx planeGraph
{-
  outerFaceDart pg = d
    where
      (v,_)  = minimum1ByOf vertices (comparing^.location) pg
             -- compare lexicographically; i.e. if same x-coord prefer the one with the
             -- smallest y-coord

      (_ :+ d) = V.minimumBy (cwCmpAroundWith' (Vector2 (-1) 0) (pg^.locationOf v :+ ()))
               . fmap (\d' -> let u = headOf d' pg in (pg^.locationOf u) :+ d')
               $ outgoingEdges v pg
      -- based on the approach sketched at https://cstheory.stackexchange.com/questions/27586/finding-outer-face-in-plane-graph-embedded-planar-graph
      -- basically: find the leftmost vertex, find the incident edge with the largest slope
      -- and take the face left of that edge. This is the outerface.
      -- note that this requires that the edges are straight line segments
      --
      -- note that rather computing slopes we just ask for the first
      -- vertec cw vertex around v. First with respect to some direction
      -- pointing towards the left.

-}

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
                ( PlaneGraph_ planeGraph vertex
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

-- | Renders all interior faces as simple polygons.
interiorFacePolygons :: forall planeGraph vertex.
                        ( PlaneGraph_ planeGraph vertex
                        , Point_ vertex 2 (NumType vertex)
                        , OuterFaceIdConstraints planeGraph, Eq (FaceIx planeGraph)
                        )
                     => IndexedFold (FaceIx planeGraph) planeGraph (SimplePolygon vertex)
interiorFacePolygons = theFold
  where
    theFold              :: forall p f.
                            ( Indexable (FaceIx planeGraph) p, Applicative f, Contravariant f)
                         => p (SimplePolygon vertex) (f (SimplePolygon vertex))
                         -> planeGraph
                         -> f planeGraph
    theFold pPolyFPoly g = interiorFaces (Indexed draw) g
      where
        draw      :: FaceIx planeGraph -> Face planeGraph -> f (Face planeGraph)
        draw fi _ = let poly = uncheckedFromCCWPoints . fmap (\vi -> g^?!vertexAt vi)
                             $ boundaryVertices fi g
                    in poly >$ indexed pPolyFPoly fi poly
        -- note that this is safe, since boundaryVerticesOf guarantees that for
        -- interior faces, the vertices are returned in CCW order.


--------------------------------------------------------------------------------

-- | get the minimum of the elements the lens points to using the given comparison function
minimum1ByOf       :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> Ordering) -> s -> a
minimum1ByOf l cmp = fromMaybe (error "minimum1ByOf") . minimumByOf l cmp
