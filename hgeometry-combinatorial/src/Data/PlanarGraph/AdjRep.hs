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

import           Control.Lens   (Bifunctor (..))
import           Data.Aeson
import           Data.Bifunctor (second)
import           Data.Bitraversable
import           Data.Bifoldable
import           GHC.Generics   (Generic)

--------------------------------------------------------------------------------

-- | Data type representing the graph in its JSON/Yaml format
data Gr v f = Gr { adjacencies :: [v]
                 , faces       :: [f]
                 } deriving (Generic, Show, Eq)

instance Functor (Gr v) where
  fmap f (Gr vs fs) = Gr vs (map f fs)
instance Foldable (Gr v) where
  foldMap f (Gr _ fs) = foldMap f fs
instance Traversable (Gr v) where
  traverse f (Gr vs fs) = Gr vs <$> traverse f fs

instance Bifunctor Gr where
  bimap f g (Gr vs fs) = Gr (map f vs) (map g fs)
instance Bifoldable Gr where
  bifoldMap f g (Gr vs fs) = foldMap f vs <> foldMap g fs
instance Bitraversable Gr where
  bitraverse f g (Gr vs fs) = Gr <$> traverse f vs <*> traverse g fs


instance (ToJSON v, ToJSON f)     => ToJSON   (Gr v f) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON v, FromJSON f) => FromJSON (Gr v f)

----------------------------------------

-- | A vertex, represented by an id, its adjacencies, and its data.
data Vtx v e = Vtx { id    :: {-# UNPACK #-} !Int
                   , adj   :: [(Int,e)] -- ^ adjacent vertices + data
                                        -- on the edge. Some
                                        -- functions, like
                                        -- 'fromAdjRep' may assume
                                        -- that the adjacencies are
                                        -- given in counterclockwise
                                        -- order around the
                                        -- vertices. This is not (yet)
                                        -- enforced by the data type.
                   , vData :: !v
                   } deriving (Generic, Show, Eq)

instance Functor (Vtx v) where
  fmap f (Vtx i as x) = Vtx i (map (second f) as) x
instance Foldable (Vtx v) where
  foldMap f (Vtx _ ads _) = foldMap (f . snd) ads
instance Traversable (Vtx v) where
  traverse f (Vtx i adjs x) = Vtx i <$> traverse (traverse f) adjs <*> pure x

instance Bifunctor Vtx where
  bimap f g (Vtx i as x) = Vtx i (map (second g) as) (f x)
instance Bifoldable Vtx where
  bifoldMap f g (Vtx _ ads x) = foldMap (g . snd) ads <> f x
instance Bitraversable Vtx where
  bitraverse f g (Vtx i adjs x) = Vtx i <$> traverse (traverse g) adjs <*> f x

instance (ToJSON v, ToJSON e)     => ToJSON   (Vtx v e) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON v, FromJSON e) => FromJSON (Vtx v e)

----------------------------------------

-- | Faces
data Face f = Face { incidentEdge :: (Int,Int) -- ^ an edge (u,v) s.t. the face
                                               -- is right from (u,v)
                   , fData        :: !f
                   } deriving (Generic,Functor,Foldable,Traversable,Show, Eq)

instance ToJSON f   => ToJSON   (Face f) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON f => FromJSON (Face f)


--------------------------------------------------------------------------------
