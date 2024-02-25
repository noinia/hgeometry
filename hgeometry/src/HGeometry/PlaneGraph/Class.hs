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
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import HGeometry.LineSegment
import HGeometry.Point
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

  -- | gets the id of the outer face
  --
  outerFaceId    :: (r ~ NumType planeGraph, Ord r, Num r) => planeGraph -> FaceIx planeGraph
  outerFaceId ps = leftFace (outerFaceDart ps) ps

  -- | gets a dart incident to the outer face (in particular, that has the
  -- outerface on its left)
  --
  -- running time: \(O(n)\)
  --
  outerFaceDart    :: (r ~ NumType planeGraph, Ord r, Num r) => planeGraph -> DartIx planeGraph
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
             => IndexedFold (EdgeIx planeGraph) planeGraph (ClosedLineSegment vertex)
dartSegments = \_pSegFSeg g -> (darts.asIndex) (drawDart g) g

drawDart     :: planeGraph -> DartIx planeGraph -> f (DartIx planeGraph)
drawDart g d = uncurry ClosedLineSegment $ g^.endPointsOf d

-- -- | Renders all edges as line segments.
-- edgeSegments :: forall planeGraph vertex.
--                 ( PlaneGraph_ planeGraph vertex
--                 , Point_ vertex 2 (NumType vertex)
--                 )
--              => IndexedFold (EdgeIx planeGraph) planeGraph (ClosedLineSegment vertex)
-- edgeSegments = theFold
--   where
--     theFold            :: forall p f.
--                           ( Indexable (EdgeIx planeGraph) p, Applicative f, Contravariant f)
--                        => p (ClosedLineSegment vertex) (f (ClosedLineSegment vertex))
--                        -> planeGraph
--                        -> f planeGraph
--     theFold pSegFSeg g = edges (Indexed draw) g
--       where
--         draw      :: EdgeIx planeGraph -> Edge planeGraph -> f (Edge planeGraph)
--         draw ei _ = let seg = uncurry ClosedLineSegment $ g^.endPointsOf (getPositiveDart g ei)
--                     in contramap (const seg) $ indexed pSegFSeg ei seg


-- interiorFacePolygons :: forall planeGraph vertex.
--                         ( PlaneGraph_ planeGraph vertex
--                         , Point_ vertex 2 (NumType vertex)
--                         )
--                      => IndexedFold (FaceIx planeGraph) planeGraph (SimplePolygon vertex)
-- interiorFacePolygons = theFold
--   where
--     theFold            :: forall p f.
--                           ( Indexable (FaceIx planeGraph) p, Applicative f, Contravariant f)
--                        => p (SimplePolygon vertex) (f (SimplePolygon vertex))
--                        -> planeGraph
--                        -> f planeGraph
--     theFold pSegFSeg g = faces.indices


--     (Indexed draw) g
--       where
--         draw      :: FaceIx planeGraph -> Face planeGraph -> f (Face planeGraph)
--         draw ei _ =


--           let seg = uncurry ClosedLineSegment $ g^.endPointsOf (getPositiveDart g ei)
--                     in contramap (const seg) $ indexed pSegFSeg ei seg



--------------------------------------------------------------------------------

-- | get the minimum of the elements the lens points to using the given comparison function
minimum1ByOf       :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> Ordering) -> s -> a
minimum1ByOf l cmp = fromMaybe (error "minimum1ByOf") . minimumByOf l cmp
