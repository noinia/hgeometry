{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlanarGraph
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing connected planar graphs
--------------------------------------------------------------------------------
module Data.PlanarGraph( -- $setup
                         -- * The Planar Graph type
                         PlanarGraph
                       , embedding, vertexData, dartData, faceData, rawDartData
                       , edgeData

                       , World(..)
                       , DualOf

                       -- * Representing edges: Arcs and Darts
                       , Arc(..)
                       , Direction(..), rev

                       , Dart(..), arc, direction
                       , twin, isPositive

                       -- * Vertices

                       , VertexId(..), VertexId'

                       -- * Building a planar graph

                       , planarGraph, planarGraph', fromAdjacencyLists
                       , toAdjacencyLists
                       , fromAdjRep, toAdjRep

                       -- , buildFromJSON

                       -- * Quering a planar graph

                       , numVertices, numDarts, numEdges, numFaces
                       , darts', darts, edges', edges, vertices', vertices, faces', faces
                       , traverseVertices, traverseDarts, traverseFaces

                       , tailOf, headOf, endPoints
                       , incidentEdges, incomingEdges, outgoingEdges, neighboursOf
                       , nextIncidentEdge, prevIncidentEdge

                       -- * Associated Data

                       , HasDataOf(..), endPointDataOf, endPointData

                       , dual

                       -- * Faces

                       , FaceId(..), FaceId'
                       , leftFace, rightFace
                       , boundaryDart, boundary, boundary', boundaryVertices
                       , nextEdge, prevEdge

                       ) where


import           Data.PlanarGraph.Core
import           Data.PlanarGraph.Dart
import           Data.PlanarGraph.Dual
import           Data.PlanarGraph.IO

--------------------------------------------------------------------------------
-- $setup
-- >>> :{
-- let dart i s = Dart (Arc i) (read s)
--     (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]
--     myGraph :: PlanarGraph () Primal () String ()
--     myGraph = planarGraph [ [ (Dart aA Negative, "a-")
--                             , (Dart aC Positive, "c+")
--                             , (Dart aB Positive, "b+")
--                             , (Dart aA Positive, "a+")
--                             ]
--                           , [ (Dart aE Negative, "e-")
--                             , (Dart aB Negative, "b-")
--                             , (Dart aD Negative, "d-")
--                             , (Dart aG Positive, "g+")
--                             ]
--                           , [ (Dart aE Positive, "e+")
--                             , (Dart aD Positive, "d+")
--                             , (Dart aC Negative, "c-")
--                             ]
--                           , [ (Dart aG Negative, "g-")
--                             ]
--                           ]
-- :}
--
--
-- This represents the following graph. Note that the graph is undirected, the
-- arrows are just to indicate what the Positive direction of the darts is.
--
-- ![myGraph](docs/Data/PlanarGraph/testG.png)




--------------------------------------------------------------------------------
-- Testing stuff

-- testPerm :: Permutation (Dart s)
-- testPerm = let (a:b:c:d:e:g:_) = take 6 [Arc 0..]
--            in toCycleRep 12 [ [ Dart a Negative
--                               , Dart c Positive
--                               , Dart b Positive
--                               , Dart a Positive
--                               ]
--                             , [ Dart e Negative
--                               , Dart b Negative
--                               , Dart d Negative
--                               , Dart g Positive
--                               ]
--                             , [ Dart e Positive
--                               , Dart d Positive
--                               , Dart c Negative
--                               ]
--                             , [ Dart g Negative
--                               ]
--                             ]

-- data Test

-- testG :: PlanarGraph Test Primal () String ()
-- testG = planarGraph [ [ (Dart aA Negative, "a-")
--                       , (Dart aC Positive, "c+")
--                       , (Dart aB Positive, "b+")
--                       , (Dart aA Positive, "a+")
--                       ]
--                     , [ (Dart aE Negative, "e-")
--                       , (Dart aB Negative, "b-")
--                       , (Dart aD Negative, "d-")
--                       , (Dart aG Positive, "g+")
--                       ]
--                     , [ (Dart aE Positive, "e+")
--                       , (Dart aD Positive, "d+")
--                       , (Dart aC Negative, "c-")
--                       ]
--                     , [ (Dart aG Negative, "g-")
--                       ]
--                     ]
--   where
--     (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]






--------------------------------------------------------------------------------
