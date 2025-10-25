{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlaneGraph.Connected.PolygonOverlay.Types
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Helper types for polygon overlay
--
--------------------------------------------------------------------------------
module HGeometry.PlaneGraph.Connected.PolygonOverlay.Types
  ( HasCoveringPolygons(..)
  , V(V), location, definingVertices
  , E(E), definingEdges, pointInEdge
  , F(F), pointInFace
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

deriving instance (Show polygon, Show (NumType polygon), Show (Vertex polygon)) => Show (V polygon)

--------------------------------------------------------------------------------

-- | The data associated with edges in the overlay.
--
-- An edge in the overlay corresponds to some line segment uv.
data E polygon = E { _definingEdges     :: ViewL1 (ClosedLineSegment (Vertex polygon) :+ polygon)
                   -- ^ The defining polygon edges; i.e. each such an edge (e :+ P) contains
                   -- the line segment uv representing this edge.  Furthermore, each such a
                   -- segment is tagged with the polygon P that defines it. I.e. e is a
                   -- counterclockwise edge in P.
                   , _coveringPolygonsE :: Seq.Seq polygon
                   -- ^ The polygons from our set that completely cover this edge uv.
                   , _pointInEdge :: Point 2 (NumType polygon)
                   -- ^ A point in the interior of the edge.
                   }

makeLenses ''E

deriving instance (Show polygon, Show (NumType polygon), Show (Vertex polygon)) => Show (E polygon)


--------------------------------------------------------------------------------

-- | The data associated with a face in the overlay.
data F polygon = F { _coveringPolygonsF :: Seq.Seq polygon
                   -- ^  the polgyons covering this face.
                   , _pointInFace       :: Maybe (Point 2 (NumType polygon))
                   -- ^ A point strictly inside the face. For internal
                   -- faces this value should be a Just point
                   }

makeLenses ''F

deriving instance (Show polygon, Show (NumType polygon)) => Show (F polygon)

--------------------------------------------------------------------------------

-- | Class for types that have some polygons covering the object.
class HasCoveringPolygons geom polygon | geom -> polygon where
  -- | Get the polygons covering this geometry
  coveringPolygons :: Lens' geom (Seq.Seq polygon)

instance HasCoveringPolygons (V polygon) polygon where
  coveringPolygons = coveringPolygonsV

instance HasCoveringPolygons (E polygon) polygon where
  coveringPolygons = coveringPolygonsE

instance HasCoveringPolygons (F polygon) polygon where
  coveringPolygons = coveringPolygonsF

--------------------------------------------------------------------------------
