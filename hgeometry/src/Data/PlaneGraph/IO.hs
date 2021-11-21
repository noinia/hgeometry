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
module Data.PlaneGraph.IO
  ( -- $setup
    --  * Reading and Writing the Plane Graph to a file
    readPlaneGraph
  , writePlaneGraph
  -- * Converting to and from Adjacency list representions
  , toAdjRep
  , fromAdjRep, fromAdjRep'
  -- * Helper functions
  , makeCCW
  ) where

import           Control.Lens
import           Control.Monad (forM_)
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString as B
import           Data.Geometry.Point
import qualified Data.List as List
import qualified Data.PlanarGraph.AdjRep as PGA
import qualified Data.PlanarGraph.IO as PGIO
import           Data.PlaneGraph.Core
import           Data.PlaneGraph.AdjRep
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.Yaml (ParseException)
import           Data.Yaml.Util


-- import Data.RealNumber.Rational
-- import Data.PlanarGraph.Dart
-- import Data.PlaneGraph.AdjRep

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
--     smallG = fromAdjRep @() small
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
readPlaneGraph :: forall s v e f r. (FromJSON v, FromJSON e, FromJSON f, FromJSON r)
               => B.ByteString
               -> Either ParseException (PlaneGraph s v e f r)
readPlaneGraph = decodeYaml

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
  parseJSON v = fromAdjRep @s <$> parseJSON v

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
fromAdjRep :: forall s v e f r. Gr (Vtx v e r) (Face f) -> PlaneGraph s v e f r
fromAdjRep = PlaneGraph . PGIO.fromAdjRep . first wrapVtx

-- | Given the AdjacencyList representation of a plane graph,
-- construct the plane graph representing it. All the adjacencylists
-- should be in counter clockwise order.
--
-- running time: \(O(n)\)
fromAdjRep' :: forall s v e r. [Vtx v e r] -> PlaneGraph s v e () r
fromAdjRep' = PlaneGraph . PGIO.fromAdjRep' . map wrapVtx

-- | Convert a vertex in PlaneGeometry format to generic PlanarGraph Vtx format.
wrapVtx                :: Vtx v e r -> PGA.Vtx (VertexData r v) e
wrapVtx (Vtx v p aj x) = PGA.Vtx v aj $ VertexData p x

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
                     MV.write a i p
                   pure a
    -- sort the adjacencies around every vertex v
    sort' (Vtx v p ajs x) = Vtx v p (List.sortBy (around p) ajs) x
    around p (a,_) (b,_) = ccwCmpAround p (location' V.! a) (location' V.! b)
                           -- note: since the graph is planar, there should not be
                           -- any pairs of points for which ccwCmpAround returns EQ
                           -- hence, no need to pick a secondary comparison

--------------------------------------------------------------------------------

-- smallG = fromAdjRep (Proxy :: Proxy ()) small
--   where
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

-- dart i s = Dart (Arc i) (read s)

-- data MyWorld

-- -- ![myGraph](docs/Data/PlaneGraph/planegraph.png)
-- myPlaneGraph :: PlaneGraph MyWorld Int () String (RealNumber 5)
-- myPlaneGraph = fromAdjRep @MyWorld myPlaneGraphAdjrep

-- myPlaneGraphAdjrep :: Gr (Vtx Int () (RealNumber 5)) (Face String)
-- myPlaneGraphAdjrep = Gr [ vtx 0 (Point2 0   0   ) [e 9, e 5, e 1, e 2]
--                         , vtx 1 (Point2 4   4   ) [e 0, e 5, e 12]
--                         , vtx 2 (Point2 3   7   ) [e 0, e 3]
--                         , vtx 3 (Point2 0   5   ) [e 4, e 2]
--                         , vtx 4 (Point2 3   8   ) [e 3, e 13]
--                         , vtx 5 (Point2 8   1   ) [e 0, e 6, e 8, e 1]
--                         , vtx 6 (Point2 6   (-1)) [e 5, e 9]
--                         , vtx 7 (Point2 9   (-1)) [e 8, e 11]
--                         , vtx 8 (Point2 12  1   ) [e 7, e 12, e 5]
--                         , vtx 9 (Point2 8   (-5)) [e 0, e 10, e 6]
--                         , vtx 10 (Point2 12 (-3)) [e 9, e 11]
--                         , vtx 11 (Point2 14 (-1)) [e 10, e 7]
--                         , vtx 12 (Point2 10 4   ) [e 1, e 8, e 13, e 14]
--                         , vtx 13 (Point2 9  6   ) [e 4, e 14, e 12]
--                         , vtx 14 (Point2 8  5   ) [e 13, e 12]
--                         ]
--                         [ Face (0,9) "OuterFace"
--                         , Face (0,5) "A"
--                         , Face (0,1) "B"
--                         , Face (0,2) "C"
--                         , Face (14,13) "D"
--                         , Face (1,12) "E"
--                         , Face (5,8) "F"
--                         ]
--   where
--     e i = (i,())
--     vtx i p es = Vtx i p es i


-- myPlaneGraph' :: IO (PlaneGraph MyWorld () () () (RealNumber 5))
-- myPlaneGraph' = let err x  = error $ show x
--                 in either err id . readPlaneGraph
--                 <$> B.readFile "docs/Data/PlaneGraph/myPlaneGraph.yaml"
