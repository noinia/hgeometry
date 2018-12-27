{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlaneGraph.IO
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Converting from/to our JSON/Yaml representation of the plane graph
--
--------------------------------------------------------------------------------
module Data.PlaneGraph.IO where

import           Control.Monad (forM_)
import           Control.Lens
import           Data.Aeson
import           Data.Bifunctor
import           Data.Ext
import           Data.Geometry.Point
import qualified Data.List as List
import qualified Data.PlanarGraph.IO as PGIO
import qualified Data.PlanarGraph.JSON as PGJ
import           Data.PlaneGraph
import           Data.PlaneGraph.JSON (Face(Face), Vtx(Vtx),Gr(Gr))
import           Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.Yaml (ParseException)
import           Data.Yaml.Util


import Data.PlanarGraph.EdgeOracle

--------------------------------------------------------------------------------

-- $setup
-- >>> :{
-- :}
--
--
-- This represents the following graph. Note that the graph is undirected, the
-- arrows are just to indicate what the Positive direction of the darts is.
--
-- ![myGraph](docs/Data/PlaneGraph/planegraph.png)


-- --------------------------------------------------------------------------------
-- -- * Reading and Writing the Plane Graph

-- -- | Reads a plane graph from a bytestring
-- readPlaneGraph   :: (FromJSON v, FromJSON e, FromJSON f, FromJSON r)
--                  => proxy s -> B.ByteString
--                  -> Either ParseException (PlaneGraph s v e f r)
-- readPlaneGraph _ = decodeYaml

-- -- | Writes a plane graph to a bytestring
-- writePlaneGraph :: (ToJSON v, ToJSON e, ToJSON f, ToJSON r)
--                 => PlaneGraph s v e f r -> B.ByteString
-- writePlaneGraph = encodeYaml

--------------------------------------------------------------------------------

instance (ToJSON v, ToJSON e, ToJSON f, ToJSON r) => ToJSON (PlaneGraph s v e f r) where
  toEncoding = toEncoding . toJSONRep
  toJSON     = toJSON     . toJSONRep

-- instance (FromJSON v, FromJSON e, FromJSON f, FromJSON r)
--          => FromJSON (PlaneGraph s v e f r) where
--   parseJSON v = fromJSONRep (Proxy :: Proxy s) <$> parseJSON v

--------------------------------------------------------------------------------

-- | Transforms the planar graph into a format taht can be easily converted
-- into JSON format. For every vertex, the adjacent vertices are given in
-- counter clockwise order.
--
-- See 'toAdjacencyLists' for notes on how we handle self-loops.
--
-- running time: \(O(n)\)
toJSONRep :: PlaneGraph s v e f r -> Gr (Vtx v e r) (Face f)
toJSONRep = first (\(PGJ.Vtx v aj (VertexData p x)) -> Vtx v p aj x) . PGIO.toJSONRep
         .  view graph

fromJSONRep    :: (Show f) =>
  proxy s -> Gr (Vtx v e r) (Face f) -> PlaneGraph s v e f r
fromJSONRep px = PlaneGraph . PGIO.fromJSONRep px
               . first (\(Vtx v p aj x) -> PGJ.Vtx v aj $ VertexData p x)


-- build' px = PGIO.buildGraph px . first (\(Vtx v p aj x) -> PGJ.Vtx v aj $ VertexData p x)


--------------------------------------------------------------------------------

-- | Orders the adjacencylists of a plane graph (with \(n\) vertices) (in json
-- repr) so that they are all counter-clockwise around the vertices.
--
-- running time: \(O(n \log n)\)
makeCCW            :: (Num r, Ord r) => Gr (Vtx v e r) f -> Gr (Vtx v e r) f
makeCCW (Gr vs fs) = Gr (map sort' vs) fs
  where
    -- create an array that we can use to lookup the vertex locations in constant time.
    location = V.create $ do
                  a <- MV.new (length vs)
                  forM_ vs $ \(Vtx i p _ _) ->
                    MV.write a i $ ext p
                  pure a
    -- sort the adjacencies around every vertex v
    sort' (Vtx v p ajs x) = Vtx v p (List.sortBy (around p) ajs) x
    around p (a,_) (b,_) = ccwCmpAround (ext p) (location V.! a) (location V.! b)

--------------------------------------------------------------------------------

myGraph :: Gr (Vtx () () Int) (Face String)
myGraph = makeCCW myGraph'

triangle :: Gr (Vtx Int String Int) (Face String)
triangle = Gr [ Vtx 0 (Point2 0 0) [ (2,"0->2")
                                , (1,"0->1")
                                ] 0
           , Vtx 1 (Point2 2 2) [ (0,"1->0")
                                , (2,"1->2")
                                ] 1
           , Vtx 2 (Point2 2 0) [ (0,"2->0")
                                , (1,"2->1")
                                ] 2
           ]
           [ Face (2,1) "OuterFace"
           , Face (0,1) "A"
           ]

small :: Gr (Vtx Int String Int) (Face String)
small = Gr [ Vtx 0 (Point2 0 0) [ (2,"0->2")
                                , (1,"0->1")
                                , (3,"0->3")
                                ] 0
           , Vtx 1 (Point2 2 2) [ (0,"1->0")
                                , (2,"1->2")
                                , (3,"1->3")
                                ] 1
           , Vtx 2 (Point2 2 0) [ (0,"2->0")
                                , (1,"2->1")
                                ] 2
           , Vtx 3 (Point2 (-1) 4) [ (0,"3->0")
                                   , (1,"3->1")
                                   ] 3
           ]
           [ Face (2,1) "OuterFace"
           , Face (0,1) "A"
           , Face (1,0) "B"
           ]

myGraph' :: Gr (Vtx () () Int) (Face String)
myGraph' = Gr [ Vtx 0 (Point2 0 0) [ (1,())
                                  , (5,())
                                  , (9,())
                                  , (2,())
                                  ] ()
             , Vtx 1 (Point2 4 4) [ (0,())
                                  , (5,())
                                  , (12,())
                                  ] ()
             , Vtx 2 (Point2 3 7) [ (3,())
                                  , (0,())
                                  ] ()
             , Vtx 3 (Point2 0 5) [(4,())
                                  , (2,())
                                  ] ()
             , Vtx 4 (Point2 3 8) [ (3,())
                                  , (13,())
                                  ] ()
             , Vtx 5 (Point2 8 1) [ (1,())
                                  , (0,())
                                  , (6,())
                                  , (8,())
                                  ] ()
             , Vtx 6 (Point2 6 (-1)) [ (5,())
                                     , (9,())
                                     ] ()
             , Vtx 7 (Point2 9 (-1)) [ (8,())
                                     , (11,())
                                     ] ()
             , Vtx 8 (Point2 12 1) [ (5,())
                                   , (7,())
                                   , (12,())
                                   ] ()
             , Vtx 9 (Point2 8 (-5)) [ (6,())
                                     , (0,())
                                     , (10,())
                                     ] ()
             , Vtx 10 (Point2 12 (-3)) [ (9,())
                                       , (11,())
                                       ] ()
             , Vtx 11 (Point2 14 (-1)) [ (10,())
                                       , (7,())
                                       ] ()
             , Vtx 12 (Point2 10 4) [ (8,())
                                    , (14,())
                                    , (1,())
                                    , (13,())
                                    ] ()
             , Vtx 13 (Point2 9 6) [ (4,())
                                   , (14,())
                                   , (12,())
                                   ] ()
             , Vtx 14 (Point2 8 5) [ (13,())
                                   , (12,())
                                   ] ()
             ]
             [ Face (4,3) "OuterFace"
             , Face (0,5) "A"
             , Face (1,5) "B"
             , Face (4,13) "C"
             , Face (13,12) "D"
             , Face (8,5) "E"
             , Face (9,6) "F"
             ]


-- gg = build' (Proxy :: Proxy PGIO.Test) small :: PlanarGraph PGIO.Test Primal () () ()

-- oracleX = edgeOracle gg

-- Just d0 = findDart (VertexId 0) (VertexId 1) oracleX
