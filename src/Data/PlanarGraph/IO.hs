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
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Maybe (fromJust)
import           Data.PlanarGraph.Core
import           Data.PlanarGraph.Derived
import           Data.PlanarGraph.Dual
import           Data.PlanarGraph.EdgeOracle
import           Data.PlanarGraph.JSON (Face(Face), Vtx(Vtx),Gr(Gr))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

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


fromJSONRep                :: proxy s -> Gr (Vtx v e) (Face f) -> PlanarGraph s Primal v e f
fromJSONRep _ (Gr as' fs') = g&vertexData .~ reorder vs _unVertexId
                              &dartData   .~ ds
                              &faceData   .~ reorder fs (_unVertexId._unFaceId)
  where
    vs = V.fromList [ VertexId vi :+ v     | Vtx vi _ v <- as' ]
    as = [ (VertexId vi, V.fromList [VertexId ui | (ui,_) <- us])
         | Vtx vi us _ <- as'
         ]
    fs = V.fromList [ findFace ui vi :+ f | Face (ui,vi) f <- fs' ]

    ds = V.fromList $ concatMap (\(Vtx vi us _) ->
                                   [(findEdge' (VertexId ui, VertexId vi),x) | (ui,x) <- us]
                                ) as'

    findFace ui vi = let d = findEdge' (VertexId ui, VertexId vi)
                     in rightFace d g

    g = fromAdjacencyLists as
    oracle = edgeOracle g
    findEdge' (u,v) = fromJust $ findDart u v oracle


-- make sure we order the data values appropriately
reorder     :: V.Vector (i :+ a) -> (i -> Int) -> V.Vector a
reorder v f = V.create $ do
                           v' <- MV.new (V.length v)
                           forM_ v $ \(i :+ x) ->
                             MV.write v' (f i) x
                           pure v'
