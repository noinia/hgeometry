--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.PlanarSubdivision.TreeRep
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data types that help encode/decode a planegraph as a JSON/YAML file.
--
--------------------------------------------------------------------------------
module Data.Geometry.PlanarSubdivision.TreeRep( PlanarSD(..)
                                              , Vtx(..)
                                              , myTreeRep
                                              ) where

-- FIXME; uncomment myTreeRep

import Data.Aeson
import Data.PlaneGraph.AdjRep (Vtx(..))
import GHC.Generics (Generic)

import Data.Geometry.Point
import Data.RealNumber.Rational

--------------------------------------------------------------------------------



-- | Specify the planar subdivison as a tree of components
data PlanarSD v e f r = PlanarSD
  { outerFace :: f           -- ^ outer face
  , inner     :: InnerSD v e f r
  } deriving (Show,Eq,Functor,Generic)

instance (ToJSON r,   ToJSON v, ToJSON e, ToJSON f)     => ToJSON   (PlanarSD v e f r) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON r, FromJSON v, FromJSON e, FromJSON f) => FromJSON (PlanarSD v e f r)


data InnerSD v e f r = InnerSD
  { adjs     :: [Vtx v e r] -- ^ list of vertices and edges in the
                                -- components incident to the outer
                                -- face
  , faces    :: [(f, [InnerSD v e f r])] -- ^ for each internal
                 -- face in the component described by adjs its data,
                 -- and possible holes
  } deriving (Show,Eq,Functor,Generic)

instance (ToJSON r,   ToJSON v, ToJSON e, ToJSON f)     => ToJSON   (InnerSD v e r f) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON r, FromJSON v, FromJSON e, FromJSON f) => FromJSON (InnerSD v e r f)



--------------------------------------------------------------------------------

-- | This represents the following Planar subdivision. Note that the
-- graph is undirected, the arrows are just to indicate what the
-- Positive direction of the darts is.
--
-- ![mySubDiv](docs/Data/Geometry/PlanarSubdivision/mySubDiv.jpg)
myTreeRep :: PlanarSD Int () String (RealNumber 3)
myTreeRep = PlanarSD "f_infty" (InnerSD ads fs)
  where
    fs = [ ("f_1", [])
         , ("f_2", [f5, f6])
         , ("f_3", [])
         , ("f_4", [f7])
         ]

    f5 = InnerSD [ vtx 16 (Point2 3    8) [e 17, e 18]
                 , vtx 17 (Point2 0    7) [e 16, e 18]
                 , vtx 18 (Point2 (-1) 4) [e 16, e 17]
                 ] [("f_5",[])]

    f6 = InnerSD [ vtx 15 (Point2 3   3) [e 14, e 13]
                 , vtx 13 (Point2 6   4) [e 14, e 15]
                 , vtx 14 (Point2 3   6) [e 13, e 15]
                 ] [("f_6",[])]

    f7 = InnerSD [ vtx 19 (Point2 0   9) [e 20, e 23]
                 , vtx 20 (Point2 0   4) [e 19, e 21]
                 , vtx 21 (Point2 15  2) [e 20, e 22]
                 , vtx 22 (Point2 17  5) [e 21, e 23]
                 , vtx 23 (Point2 15  8) [e 19, e 22]
                 ] [("f_7",[f8])]

    f8 = InnerSD [ vtx 24 (Point2 14  6) [e 25, e 26]
                 , vtx 25 (Point2 13  8) [e 24, e 26]
                 , vtx 26 (Point2 12  5) [e 24, e 25]
                 ] [("f_8",[])]

    ads = [ vtx 0 (Point2 0    0)    [e 1, e 4]
          , vtx 1 (Point2 10   2)    [e 0, e 5]
          , vtx 2 (Point2 9    9)    [e 1, e 7, e 3]
          , vtx 3 (Point2 0    10)   [e 2, e 4]
          , vtx 4 (Point2 (-4) 5)    [e 0, e 3]
          , vtx 5 (Point2 15   3)    [e 1, e 6]
          , vtx 6 (Point2 20   6)    [e 5, e 7]
          , vtx 7 (Point2 10   14)   [e 2, e 6, e 8]
          , vtx 8 (Point2 4    13)   [e 7, e 3]
          , vtx 9 (Point2 4    (-4)) [e 10, e 11]
          , vtx 10 (Point2 8   (-4)) [e 11, e 9]
          , vtx 11 (Point2 11  (-2)) [e 10, e 12]
          , vtx 12 (Point2 7   (-1)) [e 9, e 11]
          ]

    e i = (i,())

    vtx i p as = Vtx i p as i
