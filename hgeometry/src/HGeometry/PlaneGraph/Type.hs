--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometryPlaneGraph.Type
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Type type for planar graphs embedded in \(\mathbb{R}^2\). For functions that
-- export faces and edges etc, we assume the graph has a (planar) straight line
-- embedding.
--
--------------------------------------------------------------------------------
module HGeometry.PlaneGraph.Type
  ( PlaneGraph
  -- , VertexData(VertexData), location
  ) where

import           Control.Lens hiding (holes, holesOf, (.=))
import           GHC.Generics (Generic)
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.Point
import           HGeometry.Properties
import           Hiraffe.Graph.Class
import qualified Hiraffe.PlanarGraph as PG
import           Hiraffe.PlanarGraph(FaceId, PlanarGraph, VertexId, DartId
                                    , World (..), dual, planarGraph, twin
                                    )

--------------------------------------------------------------------------------
-- * The PlaneGraph type

-- | An Embedded, *connected*, planar graph
newtype PlaneGraph s v e f =
    PlaneGraph (PlanarGraph s Primal v e f)
      deriving stock (Show,Eq,Generic)

type instance NumType   (PlaneGraph s v e f) = NumType v
type instance Dimension (PlaneGraph s v e f) = 2

-- | Iso to access the graph
_PlanarGraph :: Iso (PlaneGraph s v e f)         (PlaneGraph s v' e' f')
                    (PlanarGraph s Primal v e f) (PlanarGraph s Primal v' e' f')
_PlanarGraph = coerced
{-# INLINE _PlanarGraph #-}

----------------------------------------

instance HasVertices' (PlaneGraph s v e f) where
  type Vertex   (PlaneGraph s v e f) = v
  type VertexIx (PlaneGraph s v e f) = VertexId s
  vertexAt i = _PlanarGraph.vertexAt i

instance HasVertices (PlaneGraph s v e f) (PlaneGraph s v' e f) where
  vertices = _PlanarGraph.vertices

----------------------------------------

instance HasDarts' (PlaneGraph s v e f) where
  type Dart   (PlaneGraph s v e f) = e
  type DartIx (PlaneGraph s v e f) = DartId s
  dartAt i = _PlanarGraph.dartAt i

instance HasDarts (PlaneGraph s v e f) (PlaneGraph s v e' f) where
  darts = _PlanarGraph.darts

----------------------------------------

instance HasEdges' (PlaneGraph s v e f) where
  type Edge   (PlaneGraph s v e f) = e
  type EdgeIx (PlaneGraph s v e f) = DartId s
  edgeAt d = _PlanarGraph.edgeAt d

instance HasEdges (PlaneGraph s v e f) (PlaneGraph s v e f) where
  edges = _PlanarGraph.edges

----------------------------------------

instance HasFaces' (PlaneGraph s v e f) where
  type Face   (PlaneGraph s v e f) = f
  type FaceIx (PlaneGraph s v e f) = FaceId s
  faceAt fi = _PlanarGraph.faceAt fi


instance HasFaces (PlaneGraph s v e f) (PlaneGraph s v e f') where
  faces = _PlanarGraph.faces

----------------------------------------

instance Graph_ (PlaneGraph s v e f) where




-- instance Functor (PlaneGraph s v e) where
--   fmap f pg = pg&_PlanarGraph.PG.vertexData.traverse.location %~ fmap f


-- instance ( Point_ v 2 r, Point_ v' 2 r'
--          ) => HasPoints (PlaneGraph s v e f)
--                         (PlaneGraph s v' e f) v v' where
--   allPoints = vertices


-- instance IsBoxable (PlaneGraph s vertex e f)



  -- boundingBox = boundingBoxList' . F.toList . fmap (^._2.location) . vertices






-- -- | Note that the functor instance is in v
-- data VertexData v r = VertexData { _location :: !(Point 2 r)
--                                  , _vData    :: !v
--                                  } deriving (Show,Eq,Ord,Generic
--                                             ,Functor,Foldable,Traversable)

-- type instance Dimension (VertexData v r) = 2
-- type instance NumType   (VertexData v r) = r

-- -- | Vertex data is essentially just an ext
-- _VertexDataExt :: Iso (VertexData v r) (VertexData v' s) (Point 2 r :+ v) (Point 2 s :+ v')
-- _VertexDataExt = iso (\(VertexData l v) -> l :+ v) (\(l :+ v) -> VertexData l v)

-- instance AsExt (VertexData v r) where
--   _Ext = _VertexDataExt

-- instance HasPoints (VertexData v r) (VertexData v s)
--                    (Point 2 r :+ v) (Point 2 s :+ v) where
--   allPoints = _VertexDataExt

-- class HasLocation s t where
--   -- | Lens to access the location
--   location :: Lens s t (Point (Dimension s) (NumType s)) (Point (Dimension t) (NumType t))

-- instance HasLocation (VertexData v r) (VertexData v s) where
--   location = lens _location (\vd l -> vd { _location = l })

-- -- class HasVertexData s t v v' where
-- --   -- | Lens to access the Vertex data
-- --   vertexData :: Lens s t v v'

-- instance Bifunctor VertexData where
--   bimap f g (VertexData p v) = VertexData (fmap g p) (f v)

-- -- instance (FromJSON r, FromJSON v) => FromJSON (VertexData v r ) where
-- --   parseJSON = fmap (\(l :+ d) -> VertexData l d) . parseJSON

-- -- instance (ToJSON r, ToJSON v) => ToJSON (VertexData v r ) where
-- --   toJSON     = toJSON     . vtxDataToExt
-- --   toEncoding = toEncoding . vtxDataToExt
