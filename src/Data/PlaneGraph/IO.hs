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

import           Control.Lens
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.PlanarGraph.IO as PGIO
import qualified Data.PlanarGraph.JSON as PGJ
import           Data.PlaneGraph
import           Data.PlaneGraph.JSON (Face(Face), Vtx(Vtx),Gr(Gr))
import           Data.Proxy
import           Data.Yaml (ParseException)
import           Data.Yaml.Util

import Data.Geometry.Point

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



--------------------------------------------------------------------------------

myGraph :: Gr (Vtx () () Int) (Face String)
myGraph = Gr [ Vtx 0 (Point2 0 0) [ (1,())
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
             , Face (12,13) "D"
             , Face (8,5) "E"
             , Face (9,6) "F"
             ]
