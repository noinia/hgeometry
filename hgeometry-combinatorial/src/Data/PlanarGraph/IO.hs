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
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.Bifunctor
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Maybe (fromJust)
import           Data.Permutation
import           Data.PlanarGraph.AdjRep (Face(Face), Vtx(Vtx),Gr(Gr))
import           Data.PlanarGraph.Core
import           Data.PlanarGraph.Dart
import           Data.PlanarGraph.Dual
import           Data.PlanarGraph.EdgeOracle
import           Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

--------------------------------------------------------------------------------

instance (ToJSON v, ToJSON e, ToJSON f) => ToJSON (PlanarGraph s w v e f) where
  toEncoding = toEncoding . toAdjRep
  toJSON     = toJSON     . toAdjRep

instance (FromJSON v, FromJSON e, FromJSON f) => FromJSON (PlanarGraph s Primal v e f) where
  parseJSON v = fromAdjRep (Proxy :: Proxy s) <$> parseJSON v

--------------------------------------------------------------------------------


-- | Transforms the planar graph into a format that can be easily converted
-- into JSON format. For every vertex, the adjacent vertices are given in
-- counter-clockwise order.
--
-- See 'toAdjacencyLists' for notes on how we handle self-loops.
--
-- running time: \(O(n)\)
toAdjRep   :: PlanarGraph s w v e f -> Gr (Vtx v e) (Face f)
toAdjRep g = Gr vs fs
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
fromAdjRep                  :: proxy s -> Gr (Vtx v e) (Face f) -> PlanarGraph s Primal v e f
fromAdjRep px gr@(Gr as fs) = g&vertexData .~ reorder vs' _unVertexId
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

    vs' = V.fromList [ VertexId vi :+ v     | Vtx vi _ v <- as ]
    fs' = V.fromList [ findFace ui vi :+ f | Face (ui,vi) f <- fs ]

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

-- | Construct a planar graph from a adjacency matrix. For every vertex, all
-- vertices should be given in counter-clockwise order.
--
-- pre: No self-loops, and no multi-edges
--
-- running time: \(O(n)\).
fromAdjacencyLists      :: forall s w h. (Foldable h, Functor h)
                        => [(VertexId s w, h (VertexId s w))]
                        -> PlanarGraph s w () () ()
fromAdjacencyLists adjM = planarGraph' . toCycleRep n $ perm
  where
    n    = sum . fmap length $ perm
    perm = map toOrbit  $ adjM'

    adjM' = fmap (second F.toList) adjM

    -- -- | Assign Arcs
    -- adjM' = (^._1) . foldr assignArcs (SP [] 0) $ adjM

    -- Build an edgeOracle, so that we can query the arcId assigned to
    -- an edge in O(1) time.
    oracle :: EdgeOracle s w Int
    oracle = fmap (^.core) . assignArcs . buildEdgeOracle
           . map (second $ map ext)  $ adjM'

    toOrbit (u,adjU) = concatMap (toDart u) adjU

    -- if u = v we have a self-loop, so we add both a positive and a negative dart
    toDart u v = let Just a = findEdge u v oracle
                 in case u `compare` v of
                      LT -> [Dart (Arc a) Positive]
                      EQ -> [Dart (Arc a) Positive, Dart (Arc a) Negative]
                      GT -> [Dart (Arc a) Negative]


assignArcs   :: EdgeOracle s w e -> EdgeOracle s w (Int :+ e)
assignArcs o = evalState (traverse f o) 0
  where
    f   :: e -> State Int (Int :+ e)
    f e = do i <- get ; put (i+1) ; pure (i :+ e)
