{-# LANGUAGE UndecidableInstances #-}
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
  ( PlaneGraph(..)
  -- , VertexData(VertexData), location
  ) where

import           Control.Lens hiding (holes, holesOf, (.=))
import           Data.Coerce
import           Data.Foldable1
import           Data.YAML
import           GHC.Generics (Generic)
import           HGeometry.Box
import           HGeometry.PlaneGraph.Class
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Transformation
import           Hiraffe.PlanarGraph
import qualified Hiraffe.PlanarGraph as PG

--------------------------------------------------------------------------------
-- * The PlaneGraph type

-- | An Embedded, *connected*, planar graph
newtype PlaneGraph s v e f =
    PlaneGraph (PlanarGraph s Primal v e f)
      deriving stock (Show,Eq,Generic)
      deriving newtype (ToYAML,FromYAML)

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
instance DiGraph_ (PlaneGraph s v e f) where
  type DiGraphFromAdjListExtraConstraints (PlaneGraph s v e f) h = (f ~ (), Foldable1 h)
  diGraphFromAdjacencyLists = PlaneGraph . diGraphFromAdjacencyLists
  endPoints (PlaneGraph g) = endPoints g
  twinDartOf d = twinOf d . to Just
  outgoingDartsOf v = _PlanarGraph.outgoingDartsOf v

instance BidirGraph_ (PlaneGraph s v e f) where
  twinOf d = to $ const (PG.twin d)
  getPositiveDart (PlaneGraph g) e = getPositiveDart g e


instance Graph_ (PlaneGraph s v e f) where
  type GraphFromAdjListExtraConstraints (PlaneGraph s v e f) h = (f ~ (), Foldable1 h)
  fromAdjacencyLists = PlaneGraph . fromAdjacencyLists

  neighboursOf u = _PlanarGraph.neighboursOf u
  incidentEdgesOf u = _PlanarGraph.incidentEdgesOf u

instance PlanarGraph_ (PlaneGraph s v e f) where
  type DualGraphOf (PlaneGraph s v e f) = PlanarGraph s Dual f e v

  dualGraph = dualGraph . coerce @_ @(PlanarGraph s Primal v e f)

  leftFaceOf  d = _PlanarGraph.leftFaceOf d
  rightFaceOf d = _PlanarGraph.rightFaceOf d

  nextDartOf d = _PlanarGraph.nextDartOf d
  prevDartOf d = _PlanarGraph.prevDartOf d

  boundaryDartOf f = _PlanarGraph.boundaryDartOf f
  boundaryDarts f = boundaryDarts f . coerce @_ @(PlanarGraph s Primal v e f)


instance ( Point_ v 2 (NumType v)
         , Ord (NumType v), Num (NumType v)
         ) => PlaneGraph_ (PlaneGraph s v e f) v

instance ( Point_ v 2 r, Point_ v' 2 r'
         ) => HasPoints (PlaneGraph s v e f)
                        (PlaneGraph s v' e f) v v' where
  allPoints = vertices

instance ( Point_ v 2 r
         , Ord r, Num r
         ) => IsBoxable (PlaneGraph s v e f)

instance ( Point_ v 2 r
         , DefaultTransformByConstraints (PlaneGraph s v e f) 2 r
         ) => IsTransformable (PlaneGraph s v e f)



  -- boundingBox = boundingBoxList' . F.toList . fmap (^._2.location) . vertices
