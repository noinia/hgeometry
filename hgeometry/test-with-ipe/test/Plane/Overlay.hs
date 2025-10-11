{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Plane.Overlay
  ( V(V), location, definingVertices, coveringPolygonsV
  , E(E), definingEdges, coveringPolygons
  ) where
-- TODO: rename the module


import Control.Lens
import Data.Sequence qualified as Seq
import HGeometry.Sequence.NonEmpty (ViewL1)
import HGeometry.Properties
import HGeometry.Point
import HGeometry.LineSegment
import Hiraffe.Graph.Class

--------------------------------------------------------------------------------

-- | Vertices in the overlay
data V polygon = V { _location          :: Point 2 (NumType polygon)
                   , _definingVertices  :: Seq.Seq (Vertex polygon)
                   , _coveringPolygonsV :: Seq.Seq polygon
                   }

type instance NumType   (V polygon) = NumType polygon
type instance Dimension (V polygon) = 2

makeLenses ''V

instance (NumType polygon ~ r) => Affine_ (V polygon) 2 r

instance HasVector (V polygon) (V polygon) where
  vector = location.vector
instance HasCoordinates (V polygon) (V polygon)

instance (Point_ (Vertex polygon) 2 r, NumType polygon ~ r) => Point_ (V polygon) 2 r

--------------------------------------------------------------------------------

-- | Edges in the overlay
data E polygon = E { _definingEdges    :: ViewL1 (ClosedLineSegment (Vertex polygon))
                   , _coveringPolygons :: Seq.Seq polygon
                   }

makeLenses ''E
