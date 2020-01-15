--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlanarGraph.AdjRep
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data types that to represent a planar graph as Adjacency Lists. The main
-- purpose is to help encode/decode a PlanarGraph as a JSON/YAML file.
--
--------------------------------------------------------------------------------
module Data.PlanarGraph.AdjRep where

import Data.Aeson
import GHC.Generics (Generic)
import Control.Lens(Bifunctor(..))

--------------------------------------------------------------------------------

-- | Data type representing the graph in its JSON/Yaml format
data Gr v f = Gr { adjacencies :: [v]
                 , faces       :: [f]
                 } deriving (Generic)

instance Bifunctor Gr where
  bimap f g (Gr vs fs) = Gr (map f vs) (map g fs)

instance (ToJSON v, ToJSON f)     => ToJSON   (Gr v f) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON v, FromJSON f) => FromJSON (Gr v f)

----------------------------------------

-- | A vertex, represented by an id, its adjacencies, and its data.
data Vtx v e = Vtx { id    :: Int
                   , adj   :: [(Int,e)] -- ^ adjacent vertices + data
                                        -- on the edge. Some
                                        -- functions, like
                                        -- 'fromAdjRep' may assume
                                        -- that the adjacencies are
                                        -- given in counterclockwise
                                        -- order around the
                                        -- vertices. This is not (yet)
                                        -- enforced by the data type.
                   , vData :: v
                   } deriving (Generic)

instance Bifunctor Vtx where
  bimap f g (Vtx i as x) = Vtx i (map (\(j,y) -> (j,g y)) as) (f x)

instance (ToJSON v, ToJSON e)     => ToJSON   (Vtx v e) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON v, FromJSON e) => FromJSON (Vtx v e)

----------------------------------------

-- | Faces
data Face f = Face { incidentEdge :: (Int,Int) -- ^ an edge (u,v) s.t. the face
                                               -- is right from (u,v)
                   , fData        :: f
                   } deriving (Generic,Functor)

instance ToJSON f   => ToJSON   (Face f) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON f => FromJSON (Face f)


--------------------------------------------------------------------------------
