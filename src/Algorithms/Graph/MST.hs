{-# LANGUAGE ScopedTypeVariables #-}
module Algorithms.Graph.MST( mst
                           , mstEdges
                           , makeTree
                           ) where

import           Algorithms.Graph.DFS (AdjacencyLists, dfs')
import           Control.Monad (forM_, when, filterM)
import           Control.Monad.ST (ST,runST)
import qualified Data.List as L
import           Data.PlanarGraph
import           Data.Tree
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV

--------------------------------------------------------------------------------


-- | Minimum spanning tree of the edges. The result is a rooted tree, in which
-- the nodes are the vertices in the planar graph together with the edge weight
-- of the edge to their parent. The root's weight is zero.
--
-- The algorithm used is Kruskal's.
--
-- running time: \(O(n \log n)\)
mst   :: Ord e => PlanarGraph s w v e f -> Tree (VertexId s w)
mst g = makeTree g $ mstEdges g
  -- TODO: Add edges/darts to the output somehow.

-- | Computes the set of edges in the Minimum spanning tree
--
-- running time: \(O(n \log n)\)
mstEdges   :: Ord e => PlanarGraph s w v e f -> [Dart s]
mstEdges g = runST $ do
          uf <- new (numVertices g)
          filterM (\e -> union uf (headOf e g) (tailOf e g)) edges''
  where
    edges'' = map fst . L.sortOn snd . V.toList $ edges g


-- | Given an underlying planar graph, and a set of edges that form a tree,
-- create the actual tree.
--
-- pre: the planar graph has at least one vertex.
makeTree   :: forall s w v e f.
              PlanarGraph s w v e f -> [Dart s] -> Tree (VertexId s w)
makeTree g = flip dfs' start . mkAdjacencyLists
  where
    n = numVertices g
    start = V.head $ vertices' g

    append                  :: MV.MVector s' [a] -> VertexId s w -> a -> ST s' ()
    append v (VertexId i) x = MV.read v i >>= MV.write v i . (x:)

    mkAdjacencyLists         :: [Dart s] -> AdjacencyLists s w
    mkAdjacencyLists edges'' = V.create $ do
                                 vs <- MV.replicate n []
                                 forM_ edges'' $ \e -> do
                                   let u = headOf e g
                                       v = tailOf e g
                                   append vs u v
                                   append vs v u
                                 pure vs
--------------------------------------------------------------------------------

-- | Union find DS
newtype UF s a = UF { _unUF :: UMV.MVector s (Int,Int) }

new   :: Int -> ST s (UF s a)
new n = do
          v <- UMV.new n
          forM_ [0..n-1] $ \i ->
            UMV.write v i (i,0)
          pure $ UF v

-- | Union the components containing x and y. Returns weather or not the two
-- components were already in the same component or not.
union               :: (Enum a, Eq a) => UF s a -> a -> a -> ST s Bool
union uf@(UF v) x y = do
                        (rx,rrx) <- find' uf x
                        (ry,rry) <- find' uf y
                        let b = rx /= ry
                            rx' = fromEnum rx
                            ry' = fromEnum ry
                        when b $ case rrx `compare` rry of
                            LT -> UMV.write v rx'  (ry',rrx)
                            GT -> UMV.write v ry' (rx',rry)
                            EQ -> do UMV.write v ry' (rx',rry)
                                     UMV.write v rx' (rx',rrx+1)
                        pure b


-- | Get the representative of the component containing x
-- find    :: (Enum a, Eq a) => UF s a -> a -> ST s a
-- find uf = fmap fst . find' uf

-- | get the representative (and its rank) of the component containing x
find'             :: (Enum a, Eq a) => UF s a -> a -> ST s (a,Int)
find' uf@(UF v) x = do
                      (p,r) <- UMV.read v (fromEnum x) -- get my parent
                      if toEnum p == x then
                        pure (x,r) -- I am a root
                      else do
                        rt@(j,_) <- find' uf (toEnum p)  -- get the root of my parent
                        UMV.write v (fromEnum x) (fromEnum j,r)   -- path compression
                        pure rt


--------------------------------------------------------------------------------

-- partial implementation of Prims
-- mst g = undefined

-- -- | runs MST with a given root
-- mstFrom     :: (Ord e, Monoid e)
--             => VertexId s w -> PlanarGraph s w v e f -> Tree (VertexId s w, e)
-- mstFrom r g = prims initialQ (Node (r,mempty) [])
--   where
--     update' k p q = Q.adjust (const p) k q

--     -- initial Q has the value of the root set to the zero element, and has no
--     -- parent. The others are all set to Top (and have no parent yet)
--     initialQ = update' r (ValT (mempty,Nothing))
--              . GV.foldr (\v q -> Q.insert v (Top,Nothing) q) Q.empty $ vertices g

--     prims qq t = case Q.minView qq of
--       Nothing -> t
--       Just (v Q.:-> (w,p), q) -> prims $

--------------------------------------------------------------------------------
-- Testing Stuff

-- testG = planarGraph' [ [ (Dart aA Negative, "a-")
--                        , (Dart aC Positive, "c+")
--                        , (Dart aB Positive, "b+")
--                        , (Dart aA Positive, "a+")
--                        ]
--                      , [ (Dart aE Negative, "e-")
--                        , (Dart aB Negative, "b-")
--                        , (Dart aD Negative, "d-")
--                        , (Dart aG Positive, "g+")
--                        ]
--                      , [ (Dart aE Positive, "e+")
--                        , (Dart aD Positive, "d+")
--                        , (Dart aC Negative, "c-")
--                        ]
--                      , [ (Dart aG Negative, "g-")
--                        ]
--                      ]
--   where
--     (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]
