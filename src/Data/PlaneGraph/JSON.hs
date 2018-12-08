--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlaneGraph.JSON
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data types that help encode/decode a planegraph as a JSON/YAML file.
--
--------------------------------------------------------------------------------
module Data.PlaneGraph.JSON( Gr(..)
                           , Vtx(..)
                           , Face(..)
                           ) where


import qualified Data.PlanarGraph.JSON as PGJ
import Data.PlanarGraph.JSON(Gr(..), Face(..))
import Data.Aeson
import Data.Geometry.Point
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | A vertex, represented by an id, location, its adjacencies, and its data.
data Vtx v e r = Vtx { id    :: Int
                     , loc   :: Point 2 r
                     , adj   :: [(Int,e)] -- ^ adjacent vertices + data on the
                                          -- edge. Adjacencies are given in
                                          -- arbitrary order
                     , vData :: v
                     } deriving (Generic, Functor)

instance (ToJSON r,   ToJSON v, ToJSON e)     => ToJSON   (Vtx v e r) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON r, FromJSON v, FromJSON e) => FromJSON (Vtx v e r)

--------------------------------------------------------------------------------

myGraph :: Gr () () String Int
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
