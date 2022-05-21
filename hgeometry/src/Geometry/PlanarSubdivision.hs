{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.PlanarSubdivision
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type to represent a PlanarSubdivision
--
--------------------------------------------------------------------------------
module Geometry.PlanarSubdivision
  ( -- $setup
    PlanarSubdivision

  , Component, ComponentId

    -- * Constructing Planar Subdivisions
  , fromSimplePolygon
  , fromConnectedSegments
  , fromPlaneGraph, fromPlaneGraph'
  , fromPolygons, fromPolygons'
  , fromPolygon

  -- * Quering the Planar Subdivision
  , numComponents, numVertices
  , numEdges, numFaces, numDarts
  , dual

  , components, component
  , vertices', vertices
  , edges', edges
  , faces', internalFaces', faces, internalFaces
  , darts'

  -- * Incidences and Adjacencies
  , headOf, tailOf, twin, endPoints

  , incidentEdges, incomingEdges, outgoingEdges
  , nextIncidentEdge, prevIncidentEdge
  , nextIncidentEdgeFrom, prevIncidentEdgeFrom
  , neighboursOf

  , leftFace, rightFace
  , outerBoundaryDarts, boundaryVertices, holesOf
  , outerFaceId
  , boundary'

  , Incident (incidences)
  , common, commonVertices, commonDarts, commonFaces

  -- * Data
  , locationOf
  , HasDataOf(..)

  , endPointsOf, endPointData

  , faceDataOf

  , traverseVertices, traverseDarts, traverseFaces
  , mapVertices, mapDarts, mapEdges, mapFaces

  -- * Obtaining a Geometric Representation
  , edgeSegment, edgeSegments
  , faceBoundary
  , internalFacePolygon, internalFacePolygons
  , outerFacePolygon, outerFacePolygon'
  , facePolygons

  -- * IO

  -- * ReExports
  , VertexId', FaceId'
  , VertexId(..), FaceId(..), Dart, World(..)
  , VertexData(VertexData), PG.vData, PG.location
  , PlanarGraph
  , PlaneGraph

  -- * Helper; dealing with the Raw types
  , PolygonFaceData(..)
  , FaceData(FaceData), holes, fData
  , Wrap
  , rawVertexData, rawDartData, rawFaceData
  , vertexData, dartData, faceData
  , dataVal
  , dartMapping, Raw(..)
  , asLocalD, asLocalV, asLocalF


  ) where

-- import           Algorithms.Geometry.PolygonTriangulation.Triangulate
import           Data.Ext
import           Data.Semigroup.Foldable
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as NonEmpty
import           Geometry.PlanarSubdivision.Basic
import           Geometry.PlanarSubdivision.Merge
import           Geometry.PlanarSubdivision.TreeRep()
import           Geometry.Polygon
import qualified Data.PlaneGraph as PG

-- import Geometry.Point
-- import qualified Data.List.NonEmpty as NonEmpty


-- import Debug.Trace

--------------------------------------------------------------------------------

-- | Constructs a planar subdivision from a collection of \(k\)
-- disjoint polygons of total complexity \(O(n)\).
--
-- pre: The boundary of the polygons is given in counterclockwise orientation
--
-- runningtime: \(O(n\log n\log k)\) in case of polygons with holes,
-- and \(O(n\log k)\) in case of simple polygons.
fromPolygons    :: forall s c t p r f. (Foldable1 c, Ord r, Num r)
                => f -- ^ outer face data
                -> c (Polygon t p r :+ f) -- ^ the disjoint polygons
                -> PlanarSubdivision s p () f r
fromPolygons oD = mergeAllWith const
                . fmap (\(pg :+ iD) -> fromPolygon pg iD oD) . toNonEmpty

-- | Version of 'fromPolygons' that accepts 'SomePolygon's as input.
fromPolygons'      :: forall s c p r f. (Foldable1 c, Ord r, Num r)
                   => f -- ^ outer face data
                   -> c (SomePolygon p r :+ f) -- ^ the disjoint polygons
                   -> PlanarSubdivision s p () f r
fromPolygons' oD =
    mergeAllWith const . fmap (\(pg :+ iD) -> either (build iD) (build iD) pg) . toNonEmpty
  where
    build       :: f -> Polygon t p r -> PlanarSubdivision s p () f r
    build iD pg = fromPolygon pg iD oD

-- | Construct a planar subdivision from a polygon. Since our PlanarSubdivision
-- models only connected planar subdivisions, this may add dummy/invisible
-- edges.
--
-- pre: The outer boundary of the polygons is given in counterclockwise orientation
--
-- running time: \(O(n)\) for a simple polygon, \(O(n\log n)\) for a
-- polygon with holes.
fromPolygon                              :: forall s t p f r. (Ord r, Num r)
                                         => Polygon t p r
                                         -> f -- ^ data inside
                                         -> f -- ^ data outside the polygon
                                         -> PlanarSubdivision s p () f r
fromPolygon pg@SimplePolygon{} iD oD   = fromSimplePolygon @s pg iD oD
fromPolygon (MultiPolygon vs hs) iD oD = case NonEmpty.nonEmpty hs of
    Nothing  -> outerPG
    Just hs' -> let hs'' = (\pg -> fromSimplePolygon @(Wrap s)
                                   (toCounterClockWiseOrder pg) oD iD) <$> hs'
                in embedAsHolesIn hs'' (\_ x -> x) i outerPG
  where
    outerPG = fromSimplePolygon @s vs iD oD
    i = V.last $ faces' outerPG






  -- subd&planeGraph.faceData .~ faceData'
  --                            &planeGraph.vertexData.traverse %~ getP
  -- where
  --   faceData' = fmap (\(fi, FaceData hs _) -> FaceData hs (getFData fi)) . faces $ subd

  --   -- given a faceId lookup the
  --   getFData fi = let v = boundaryVertices fi subd V.! 0
  --                 in subd^.dataOf v.to holeData

  --   -- note that we intentionally reverse the order of iDd and oD in the call below,
  --   -- as our holes are now outside
  --   subd = fromPolygon px (MultiPolygon (CSeq.fromList [a,b,c,d]) holes') (Just oD) Nothing

  --   -- for every polygon, construct a hole.
  --   holes' = map withF . F.toList $ pgs
  --   -- add the facedata to the vertex data
  --   withF (pg :+ f) = bimap (\p -> Hole f p) id pg

  --   -- corners of the slightly enlarged boundingbox
  --   (a,b,c,d) = corners . bimap (const $ Outer oD) id
  --             . grow 1 . boundingBoxList . fmap (^.core) $ pgs

    --TODO: We need to mark the edges of the outer square as invisible.

    -- Main Idea: Assign the vertices the hole-number on which they occur. For
    -- each face we then find an incident vertex to find the data corresponding
    -- to that face.

data HoleData f p = Outer !f | Hole !f !p deriving (Show,Eq)

_holeData            :: HoleData f p -> f
_holeData (Outer f)  = f
_holeData (Hole f _) = f

_getP            :: HoleData f p -> Maybe p
_getP (Outer _)  = Nothing
_getP (Hole _ p) = Just p

--------------------------------------------------------------------------------

-- data Test = Test
-- data Id a = Id a


-- simplePg  = fromSimplePolygon (Id Test) simplePg' Inside Outside
-- simplePg' = toCounterClockWiseOrder . fromPoints $ map ext $ [ Point2 160 736
--                                                              , Point2 128 688
--                                                              , Point2 176 672
--                                                              , Point2 256 672
--                                                              , Point2 272 608
--                                                              , Point2 384 656
--                                                              , Point2 336 768
--                                                              , Point2 272 720
--                                                              ]

-- triangle :: PlanarSubdivision Test () () PolygonFaceData Rational
-- triangle = (\pg -> fromSimplePolygon (Id Test) pg Inside Outside)
--          $ trianglePG

-- trianglePG = fromPoints . map ext $ [origin, Point2 10 0, Point2 10 10]


-- mySubDiv = fromSimplePolygons (Id Test)
--                               0
--                               (NonEmpty.fromList [simplePg' :+ 1, trianglePG :+ 2])




-- type R = Int
-- data MyWorld

-- mySubDiv :: PlanarSubdivision MyWorld Int (Int,Int) String R
-- mySubDiv = undefined

--     faceData xs = FaceData (Seq.fromList xs)



-- fromTreeRep                                 :: TreeRep v e f r -> PlanarSubdivision s v e f r
-- fromTreeRep (PlanarSD of' (InnerSD ajs fs)) = undefined


-- fromInnerRep                     :: forall s v e f r. (Ord r, Fractional r)
--                                  => InnerRep v e f r -> PlanarSubdivision s v e () r
-- fromInnerRep f (InnerSD ajs fs) = fromConnectedSegments (Proxy @s) segs
--   where
--     segs = adjs
