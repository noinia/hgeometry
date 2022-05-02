{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlaneGraph
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for planar graphs embedded in \(\mathbb{R}^2\). For functions that
-- export faces and edges etc, we assume the graph has a (planar) straight line
-- embedding.
--
--------------------------------------------------------------------------------
module Data.PlaneGraph( -- $setup
                       -- * The PlaneGraph data type
                        PlaneGraph, graph
                      , PlanarGraph
                      , VertexData(VertexData), vData, location, vtxDataToExt

                       -- * Constructing PlaneGraphs
                      , fromSimplePolygon, fromConnectedSegments
                      , toAdjRep, fromAdjRep

                       -- * Quering a PlaneGraph graph
                      , numVertices, numEdges, numFaces, numDarts
                      , dual

                      , vertices', vertices
                      , edges', edges
                      , faces', faces, internalFaces, faces''
                      , darts', darts

                      -- * Incidences and Adjacencies
                      , headOf, tailOf, twin, endPoints
                      , incidentEdges, incomingEdges, outgoingEdges
                      , neighboursOf
                      , nextIncidentEdge, prevIncidentEdge
                      , nextIncidentEdgeFrom, prevIncidentEdgeFrom

                      , leftFace, rightFace
                      , nextEdge, prevEdge
                      , boundary, boundary', boundaryDart, boundaryVertices
                      , outerFaceId, outerFaceDart

                      -- * Data

                      , vertexDataOf, locationOf, HasDataOf(..)

                      , endPointsOf, endPointData
                      , vertexData, faceData, dartData, rawDartData

                      , traverseVertices, traverseDarts, traverseFaces

                      -- * Obtaining a Geometric Representation

                      , edgeSegment, edgeSegments
                      , faceBoundary, internalFacePolygon
                      , outerFacePolygon, outerFacePolygon'
                      , facePolygons, facePolygons'

                      , withEdgeDistances

                      -- * IO
                      , writePlaneGraph, readPlaneGraph

                      -- * Reexports
                      , VertexId(..), FaceId(..), Dart, World(..), VertexId', FaceId'
                      ) where

import           Data.PlaneGraph.IO
import           Data.PlaneGraph.Core


--------------------------------------------------------------------------------

-- $setup
-- >>> import Data.Proxy
-- >>> import Data.PlaneGraph.AdjRep(Gr(Gr),Face(Face),Vtx(Vtx))
-- >>> import Data.PlaneGraph.IO(fromAdjRep)
-- >>> import Data.PlanarGraph.Dart(Dart(..),Arc(..))
-- >>> :{
-- let dart i s = Dart (Arc i) (read s)
--     small :: Gr (Vtx Int String Int) (Face String)
--     small = Gr [ Vtx 0 (Point2 0 0) [ (2,"0->2")
--                                     , (1,"0->1")
--                                     , (3,"0->3")
--                                     ] 0
--                , Vtx 1 (Point2 2 2) [ (0,"1->0")
--                                     , (2,"1->2")
--                                     , (3,"1->3")
--                                     ] 1
--                , Vtx 2 (Point2 2 0) [ (0,"2->0")
--                                     , (1,"2->1")
--                                     ] 2
--                , Vtx 3 (Point2 (-1) 4) [ (0,"3->0")
--                                        , (1,"3->1")
--                                        ] 3
--                ]
--                [ Face (2,1) "OuterFace"
--                , Face (0,1) "A"
--                , Face (1,0) "B"
--                ]
--     smallG = fromAdjRep (Proxy :: Proxy ()) small
-- :}
--
--
-- This represents the following graph. Note that the graph is undirected, the
-- arrows are just to indicate what the Positive direction of the darts is.
--
-- ![myGraph](docs/Data/PlaneGraph/small.png)
--
--
-- Here is also a slightly larger example graph:
-- ![myGraph](docs/Data/PlaneGraph/planegraph.png)
--
-- >>> import Data.RealNumber.Rational
-- >>> data MyWorld
-- >>> :{
-- let myPlaneGraph :: PlaneGraph MyWorld Int () String (RealNumber 5)
--     myPlaneGraph = fromAdjRep (Proxy @MyWorld) myPlaneGraphAdjrep
--     myPlaneGraphAdjrep :: Gr (Vtx Int () (RealNumber 5)) (Face String)
--     myPlaneGraphAdjrep = Gr [ vtx 0 (Point2 0   0   ) [e 9, e 5, e 1, e 2]
--                             , vtx 1 (Point2 4   4   ) [e 0, e 5, e 12]
--                             , vtx 2 (Point2 3   7   ) [e 0, e 3]
--                             , vtx 3 (Point2 0   5   ) [e 4, e 2]
--                             , vtx 4 (Point2 3   8   ) [e 3, e 13]
--                             , vtx 5 (Point2 8   1   ) [e 0, e 6, e 8, e 1]
--                             , vtx 6 (Point2 6   (-1)) [e 5, e 9]
--                             , vtx 7 (Point2 9   (-1)) [e 8, e 11]
--                             , vtx 8 (Point2 12  1   ) [e 7, e 12, e 5]
--                             , vtx 9 (Point2 8   (-5)) [e 0, e 10, e 6]
--                             , vtx 10 (Point2 12 (-3)) [e 9, e 11]
--                             , vtx 11 (Point2 14 (-1)) [e 10, e 7]
--                             , vtx 12 (Point2 10 4   ) [e 1, e 8, e 13, e 14]
--                             , vtx 13 (Point2 9  6   ) [e 4, e 14, e 12]
--                             , vtx 14 (Point2 8  5   ) [e 13, e 12]
--                             ]
--                             [ Face (0,9) "OuterFace"
--                             , Face (0,5) "A"
--                             , Face (0,1) "B"
--                             , Face (0,2) "C"
--                             , Face (14,13) "D"
--                             , Face (1,12) "E"
--                             , Face (5,8) "F"
--                             ]
--       where
--         e i = (i,())
--         vtx i p es = Vtx i p es i
-- :}
