{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlaneGraph.IO
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Converting from/to Adjacency Representation of the plane graph
--
--------------------------------------------------------------------------------
module Data.PlaneGraph.IO where

import           Control.Lens
import           Control.Monad (forM_)
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString as B
import           Data.Ext
import           Data.Geometry.Point
import qualified Data.List as List
import qualified Data.PlanarGraph.AdjRep as PGA
import qualified Data.PlanarGraph.IO as PGIO
import           Data.PlaneGraph.Core
import           Data.PlaneGraph.AdjRep (Face,Vtx(Vtx),Gr(Gr))
import           Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.Yaml (ParseException)
import           Data.Yaml.Util

--------------------------------------------------------------------------------

-- $setup
-- >>> import Data.PlanarGraph.Dart
-- >>> import Data.PlanarGraph.AdjRep(Face(..))
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

--------------------------------------------------------------------------------
-- * Reading and Writing the Plane Graph

-- | Reads a plane graph from a bytestring
readPlaneGraph   :: (FromJSON v, FromJSON e, FromJSON f, FromJSON r)
                 => proxy s -> B.ByteString
                 -> Either ParseException (PlaneGraph s v e f r)
readPlaneGraph _ = decodeYaml

-- | Writes a plane graph to a bytestring
writePlaneGraph :: (ToJSON v, ToJSON e, ToJSON f, ToJSON r)
                => PlaneGraph s v e f r -> B.ByteString
writePlaneGraph = encodeYaml

--------------------------------------------------------------------------------

instance (ToJSON v, ToJSON e, ToJSON f, ToJSON r) => ToJSON (PlaneGraph s v e f r) where
  toEncoding = toEncoding . toAdjRep
  toJSON     = toJSON     . toAdjRep

instance (FromJSON v, FromJSON e, FromJSON f, FromJSON r)
         => FromJSON (PlaneGraph s v e f r) where
  parseJSON v = fromAdjRep (Proxy :: Proxy s) <$> parseJSON v

--------------------------------------------------------------------------------

-- | Transforms the plane graph into adjacency lists. For every
-- vertex, the adjacent vertices are given in counter clockwise order.
--
-- See 'toAdjacencyLists' for notes on how we handle self-loops.
--
-- running time: \(O(n)\)
toAdjRep :: PlaneGraph s v e f r -> Gr (Vtx v e r) (Face f)
toAdjRep = first (\(PGA.Vtx v aj (VertexData p x)) -> Vtx v p aj x) . PGIO.toAdjRep
         .  view graph

-- | Given the AdjacencyList representation of a plane graph,
-- construct the plane graph representing it. All the adjacencylists
-- should be in counter clockwise order.
--
-- running time: \(O(n)\)
fromAdjRep    :: proxy s -> Gr (Vtx v e r) (Face f) -> PlaneGraph s v e f r
fromAdjRep px = PlaneGraph . PGIO.fromAdjRep px
              . first (\(Vtx v p aj x) -> PGA.Vtx v aj $ VertexData p x)

--------------------------------------------------------------------------------

-- | Orders the adjacencylists of a plane graph (with \(n\) vertices) (in Adj
-- repr) so that they are all counter-clockwise around the vertices.
--
-- running time: \(O(n \log n)\)
makeCCW            :: (Num r, Ord r) => Gr (Vtx v e r) f -> Gr (Vtx v e r) f
makeCCW (Gr vs fs) = Gr (map sort' vs) fs
  where
    -- create an array that we can use to lookup the vertex locations in constant time.
    location' = V.create $ do
                   a <- MV.new (length vs)
                   forM_ vs $ \(Vtx i p _ _) ->
                     MV.write a i $ ext p
                   pure a
    -- sort the adjacencies around every vertex v
    sort' (Vtx v p ajs x) = Vtx v p (List.sortBy (around p) ajs) x
    around p (a,_) (b,_) = ccwCmpAround (ext p) (location' V.! a) (location' V.! b)
                           -- note: since the graph is planar, there should not be
                           -- any pairs of points for which ccwCmpAround returns EQ
                           -- hence, no need to pick a secondary comparison

--------------------------------------------------------------------------------
