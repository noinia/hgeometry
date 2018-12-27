{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlanarGraph.IO
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Converting from/to our JSON/Yaml representation of the plane graph
--
--------------------------------------------------------------------------------
module Data.PlanarGraph.IO where

import           Control.Lens
import           Control.Monad (forM_)
import           Data.Aeson
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Maybe (fromJust)
import           Data.PlanarGraph.Core
import           Data.PlanarGraph.Derived
import           Data.PlanarGraph.Dual
import           Data.PlanarGraph.EdgeOracle
import           Data.PlanarGraph.JSON (Face(Face), Vtx(Vtx),Gr(Gr))
import           Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import           Data.PlanarGraph.Dart
import Debug.Trace
--------------------------------------------------------------------------------

instance (ToJSON v, ToJSON e, ToJSON f) => ToJSON (PlanarGraph s w v e f) where
  toEncoding = toEncoding . toJSONRep
  toJSON     = toJSON     . toJSONRep

-- instance (FromJSON v, FromJSON e, FromJSON f) => FromJSON (PlanarGraph s Primal v e f) where
--   parseJSON v = fromJSONRep (Proxy :: Proxy s) <$> parseJSON v



--------------------------------------------------------------------------------


-- | Transforms the planar graph into a format taht can be easily converted
-- into JSON format. For every vertex, the adjacent vertices are given in
-- counter clockwise order.
--
-- See 'toAdjacencyLists' for notes on how we handle self-loops.
--
-- running time: \(O(n)\)
toJSONRep   :: PlanarGraph s w v e f -> Gr (Vtx v e) (Face f)
toJSONRep g = Gr vs fs
  where
    vs = [ Vtx ui (map (mkEdge u) $ F.toList us) (g^.dataOf u)
         | (u@(VertexId ui),us) <- toAdjacencyLists g
         ]
    fs = [ Face (outerComponentEdge f) x
         | (f,x) <- F.toList $ faces g
         ]

    outerComponentEdge f = bimap (^.unVertexId) (^.unVertexId)
                         $ endPoints (boundaryDart f g) g

    eo = edgeOracle g

    findData u v = (\d -> g^.dataOf d) <$> findDart u v eo
    mkEdge u v@(VertexId vi) = (vi,fromJust $ findData u v)






-- | Read a planar graph, given in JSON format into a planar graph. The adjacencylists
-- should be in counter clockwise order.
--
-- running time: \(O(n)\)
fromJSONRep                 :: (Show f)
                             => proxy s -> Gr (Vtx v e) (Face f) -> PlanarGraph s Primal v e f
fromJSONRep px gr@(Gr as fs) = g&vertexData .~ reorder vs _unVertexId
                                &dartData   .~ ds
                                &faceData   .~ reorder fs' (_unVertexId._unFaceId)
  where
    -- build the actual graph using the adjacencies
    g = buildGraph px gr
    -- build an edge oracle so that we can quickly lookup the dart corresponding to a
    -- pair of vertices.
    oracle = edgeOracle g
    -- function to lookup a given dart
    findEdge' u v = fromJust $ findDart u v oracle
    -- faces are right of oriented darts
    findFace ui vi = let d = findEdge' (VertexId ui) (VertexId vi) in rightFace d g

    vs = V.fromList [ VertexId vi :+ v     | Vtx vi _ v <- as ]
    fs'' = V.fromList [ findFace ui vi :+ f | Face (ui,vi) f <- fs ]
    fs' = traceShow ("FS: ", fs'') fs''

    ds = V.fromList $ concatMap (\(Vtx vi us _) ->
                                   [(findEdge' (VertexId vi) (VertexId ui), x) | (ui,x) <- us]
                                ) as

  -- TODO: Properly handle graphs with self-loops

-- | Builds the graph from the adjacency lists (but ignores all associated data)
buildGraph              :: proxy s -> Gr (Vtx v e) (Face f) -> PlanarGraph s Primal () () ()
buildGraph _ (Gr as' _) = fromAdjacencyLists as
  where
    as = [ (VertexId vi, V.fromList [VertexId ui | (ui,_) <- us])
         | Vtx vi us _ <- as'
         ]

-- make sure we order the data values appropriately
reorder     :: V.Vector (i :+ a) -> (i -> Int) -> V.Vector a
reorder v f = V.create $ do
                           v' <- MV.new (V.length v)
                           forM_ v $ \(i :+ x) ->
                             MV.write v' (f i) x
                           pure v'



--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

data Test

type Vertex = VertexId Test Primal

testEdges :: [(Vertex,[Vertex])]
testEdges = map (\(i,vs) -> (VertexId i, map VertexId vs))
            [ (0, [1])
            , (1, [0,1,2,4])
            , (2, [1,3,4])
            , (3, [2,5])
            , (4, [1,2,5])
            , (5, [3,4])
            ]


myGraph :: PlanarGraph Test Primal () String ()
myGraph = planarGraph [ [ (Dart aA Negative, "a-")
                        , (Dart aC Positive, "c+")
                        , (Dart aB Positive, "b+")
                        , (Dart aA Positive, "a+")
                        ]
                      , [ (Dart aE Negative, "e-")
                        , (Dart aB Negative, "b-")
                        , (Dart aD Negative, "d-")
                        , (Dart aG Positive, "g+")
                        ]
                      , [ (Dart aE Positive, "e+")
                        , (Dart aD Positive, "d+")
                        , (Dart aC Negative, "c-")
                            ]
                      , [ (Dart aG Negative, "g-")
                        ]
                      ]
  where
    -- dart i s = Dart (Arc i) (read s)
    (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]
