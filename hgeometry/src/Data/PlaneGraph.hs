{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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
module Data.PlaneGraph( PlaneGraph(PlaneGraph), graph
                      , PlanarGraph
                      , VertexData(VertexData), vData, location, vtxDataToExt
                      , fromSimplePolygon, fromConnectedSegments
                      , toAdjRep, fromAdjRep

                      , numVertices, numEdges, numFaces, numDarts
                      , dual

                      , vertices', vertices
                      , edges', edges
                      , faces', faces, internalFaces, faces''
                      , darts', darts
                      , traverseVertices, traverseDarts, traverseFaces

                      , headOf, tailOf, twin, endPoints

                      , incidentEdges, incomingEdges, outgoingEdges
                      , neighboursOf
                      , nextIncidentEdge, prevIncidentEdge


                      , leftFace, rightFace
                      , nextEdge, prevEdge
                      , boundary, boundary', boundaryDart, boundaryVertices
                      , outerFaceId, outerFaceDart

                      , vertexDataOf, locationOf, HasDataOf(..)

                      , endPointsOf, endPointData
                      , vertexData, faceData, dartData, rawDartData

                      , edgeSegment, edgeSegments
                      , rawFacePolygon, rawFaceBoundary
                      , rawFacePolygons

                      , VertexId(..), FaceId(..), Dart, World(..), VertexId', FaceId'


                      , withEdgeDistances
                      , writePlaneGraph, readPlaneGraph
                      ) where

import           Data.PlaneGraph.IO
import           Data.PlaneGraph.Core
