{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell            #-}
module GeometryStore
  ( GeometryStore
  , empty
  , Key
  , insertNew

  ) where

import           Attributes
import           Base
import           Control.Lens
import qualified Data.EnumMap as EnumMap
import qualified Data.EnumSet as EnumSet
import           GeometryStore.Helper (lensFieldNamer)
import           HGeometry.Ext
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Triangulation
import           PolyLineMode
import           PolygonMode
import           RectangleMode
--------------------------------------------------------------------------------

newtype Key = Key Int
  deriving stock   (Show)
  deriving newtype (Eq,Ord,Enum)

--------------------------------------------------------------------------------

data Geom = G_Point      (Point 2 R :+ Attributes (Point 2 R))
          | G_PolyLine   (PolyLine' R :+ Attributes (PolyLine' R))
          | G_Polygon    (SimplePolygon' R :+ Attributes (SimplePolygon' R))
          | G_Rect       (Rectangle' R :+ Attributes (Rectangle' R))
          | G_PlaneGraph (PlaneGraph' R)
          deriving (Show,Eq)

data MyWorld

type PlaneGraph' r = PlaneGraph MyWorld (Point 2 r) PolygonEdgeType PolygonFaceData

--------------------------------------------------------------------------------

data GeometryStore =
  GeometryStore { _store       :: EnumMap.EnumMap Key Geom
                  -- ^ place where we store the actual geometries
                , _points      :: EnumSet.EnumSet Key
                  -- ^ indices of all points
                , _polyLines   :: EnumSet.EnumSet Key
                  -- ^ indices of all polylines
                , _polygons    :: EnumSet.EnumSet Key
                  -- ^ indices of all polygons
                , _planeGraphs :: EnumSet.EnumSet Key
                  -- ^  indices of all planeGraphs
                } deriving (Show,Eq)


-- | Generate actuall lenses of the form 'geomstoreMyFieldName'
makeLensesWith (defaultFieldRules&lensField .~ lensFieldNamer) ''GeometryStore


points :: IndexedTraversal' Key GeometryStore (Point 2 R :+ Attributes (Point 2 r))
points = undefined
  -- TODO

--------------------------------------------------------------------------------

-- | Creates an empty geometry store
empty :: GeometryStore
empty = GeometryStore
        { _store       = mempty
        , _points      = mempty
        , _polyLines   = mempty
        , _polygons    = mempty
        , _planeGraphs = mempty
        }


-- | Inserts a new element in the store (with a new key). Returns the new key and the
-- updated store.
insertNew         :: v -> GeometryStore -> (Key, GeometryStore)
insertNew x store = undefined

  -- (k, store&_DMap %~ DMap.insert k (Identity x))
  -- where
  --   k = case DMap.lookupMax $ store^._DMap of
  --         Nothing            -> Key 0
  --         Just (Key m :=> _) -> Key (m+1)
