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
import HGeometry.Ext
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

-- | The data associated with edges in the overlay.
--
-- An edge in the overlay corresponds to some line segment uv.
data E polygon = E { _definingEdges    :: ViewL1 (ClosedLineSegment (Vertex polygon) :+ polygon)
                   -- ^ The defining polygon edges; i.e. each such an edge (e :+ P) contains
                   -- the line segment uv representing this edge.  Furthermore, each such a
                   -- segment is tagged with the polygon P that defines it. I.e. e is a
                   -- counterclockwise edge in P.
                   , _coveringPolygons :: Seq.Seq polygon
                   -- ^ The polygons from our set that completely cover this edge uv.
                   }

makeLenses ''E
