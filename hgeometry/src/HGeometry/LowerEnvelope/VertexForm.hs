{-# LANGUAGE UndecidableInstances #-}
module HGeometry.LowerEnvelope.VertexForm
  ( VertexForm(VertexForm)
  , singleton
  , LEVertex, pattern LEVertex, Definers
  , BoundedVertexF(Vertex)
  , location, definers, location2
  ) where

import           Control.Lens
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           HGeometry.Foldable.Sort
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.Line.LineEQ
import           HGeometry.LowerEnvelope.Type
import           HGeometry.Point
import           HGeometry.Properties
import           Hiraffe.Graph

--------------------------------------------------------------------------------

-- | Vertices of a lower envelope in vertex form.
type LEVertex = BoundedVertexF (Const ())

-- | Convenience Constructor for computing vertices of the lower
-- envelope that are in Vertex form.
pattern LEVertex        :: Point 3 (NumType plane) -> Definers plane -> LEVertex plane
pattern LEVertex v defs = Vertex v defs (Const ())
{-# COMPLETE LEVertex #-}

-- | The definers of a vertex.
type Definers plane = Set.Set plane

-- | The lower envelope in vertex form
newtype VertexForm plane =
  VertexForm (Map.Map (Point 3 (NumType plane))
                      (Definers plane)
             )

-- | Iso to access the underlying Map.
_VertexFormMap :: Iso (VertexForm plane) (VertexForm plane')
                      (Map.Map (Point 3 (NumType plane))  (Definers plane))
                      (Map.Map (Point 3 (NumType plane')) (Definers plane'))
_VertexFormMap = coerced


deriving instance ( Show plane, Show (NumType plane)
                  ) => Show (VertexForm plane)
deriving instance ( Eq plane
                  , Eq (NumType plane)
                  ) => Eq   (VertexForm plane)

-- | Computes a lower envelope consisting of a single vertex.
singleton   :: LEVertex plane -> VertexForm plane
singleton v = VertexForm $ Map.singleton (v^.location) (v^.definers)

instance (Ord (NumType plane), Ord plane) => Semigroup (VertexForm plane) where
  (VertexForm m) <> (VertexForm m') = VertexForm $ Map.unionWith (<>) m m'
instance (Ord (NumType plane), Ord plane) => Monoid (VertexForm plane) where
  mempty = VertexForm mempty

instance Ord (NumType plane) => HasVertices' (VertexForm plane) where
  type Vertex   (VertexForm plane) = Definers plane
  type VertexIx (VertexForm plane) = Point 3 (NumType plane)
  vertexAt i = _VertexFormMap . iix i

instance Ord (NumType plane) => HasVertices (VertexForm plane) (VertexForm plane) where
  vertices = _VertexFormMap . itraversed
