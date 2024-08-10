{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.GeoJSON
  (

  ) where

import           Control.Lens
import           Data.Geospatial
import           Data.LinearRing
import qualified Data.Sequence as Seq
import           HGeometry.Ext
import           HGeometry.Point.Class
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Properties
import           HGeometry.Vector
import Data.Foldable1

--------------------------------------------------------------------------------
-- * PointXY

type instance NumType   PointXY = Double
type instance Dimension PointXY = 2

instance HasVector PointXY PointXY where
  vector = lens (\(PointXY x y) -> Vector2 x y) (\_ (Vector2 x y) -> PointXY x y)

instance HasCoordinates PointXY PointXY
instance Affine_ PointXY 2 Double
instance Point_  PointXY 2 Double

--------------------------------------------------------------------------------
-- * PointXYZ

type instance NumType   PointXYZ = Double
type instance Dimension PointXYZ = 3

instance HasVector PointXYZ PointXYZ where
  vector = lens (\(PointXYZ x y z) -> Vector3 x y z) (\_ (Vector3 x y z) -> PointXYZ x y z)

instance HasCoordinates PointXYZ PointXYZ
instance Affine_ PointXYZ 3 Double
instance Point_  PointXYZ 3 Double

--------------------------------------------------------------------------------
-- * PointXYZM

type instance NumType   PointXYZM = Double
type instance Dimension PointXYZM = 4

instance HasVector PointXYZM PointXYZM where
  vector = lens (\(PointXYZM x y z m) -> Vector4 x y z m)
                (\_ (Vector4 x y z m) -> PointXYZM x y z m)

instance HasCoordinates PointXYZM PointXYZM
instance Affine_ PointXYZM 4 Double
instance Point_  PointXYZM 4 Double

--------------------------------------------------------------------------------
-- * GeoPositionWithoutCRS

type GeoPositionWithoutCRS' = PointXY :+ Maybe RestGeoPosition

type instance NumType GeoPositionWithoutCRS = Double

data RestGeoPosition = Z  {-#UNPACK#-}!Double
                     | ZM {-#UNPACK#-}!Double {-#UNPACK#-}!Double
                     deriving (Show,Eq,Ord)

-- | Convert between a GeoPositionWithoutCRS and a PointXY
_GeoPositionWithoutCRS :: Prism' GeoPositionWithoutCRS (PointXY :+ Maybe RestGeoPosition)
_GeoPositionWithoutCRS = prism' toGeoP fromGeoP
  where
    toGeoP (p@(PointXY x y) :+ mr) = case mr of
      Nothing       -> GeoPointXY p
      Just (Z z)    -> GeoPointXYZ $ PointXYZ x y z
      Just (ZM z m) -> GeoPointXYZM $ PointXYZM x y z m
    fromGeoP = \case
      GeoEmpty                         -> Nothing
      GeoPointXY p                     -> Just $ p             :+ Nothing
      GeoPointXYZ (PointXYZ x y z)     -> Just $ (PointXY x y) :+ Just (Z z)
      GeoPointXYZM (PointXYZM x y z m) -> Just $ (PointXY x y) :+ Just (ZM z m)


--------------------------------------------------------------------------------
-- * Polygon

type instance NumType   GeoPolygon = Double

type instance Dimension GeoPolygon = 2


instance Wrapped GeoPolygon
instance Rewrapped GeoPolygon GeoPolygon

--------------------------------------------------------------------------------
type instance Index   (LinearRing a) = Int
type instance IxValue (LinearRing a) = a

-- instance Ixed (LinearRing a) where

{-
-- traverse1Ring :: Apply f => (a -> f b) -> LinearRing a -> f (LinearRing b)
-- traverse1Ring f


--------------------------------------------------------------------------------

instance HasVertices' GeoPolygon where
  type VertexIx GeoPolygon = (Int,Int) -- first one is the ringId
  type Vertex   GeoPolygon = GeoPositionWithoutCRS'

  vertexAt (i,j) = _Wrapped .> iix i <.> iix j <. _GeoPositionWithoutCRS

  numVertices (GeoPolygon ss) = foldl' (\a s -> a + length s - 1) 0 ss
  -- the ring incluces a copy of the first element, so it overestimates the length by 1

instance HasVertices GeoPolygon GeoPolygon where
  -- (Indexable i p, Apply f) => p a (f b) -> s -> f t
  -- vertices = conjoined

instance HasOuterBoundary GeoPolygon where
  outerBoundaryVertexAt v@(i,_)
    | i == 0    = vertexAt v
    | otherwise = \_ pg -> pure pg
  -- the first ring is outer boundary apparently
  ccwOuterBoundaryFrom i = undefined
  cwOuterBoundaryFrom  i = undefined

instance Polygon_ GeoPolygon GeoPositionWithoutCRS' Double where
  area = _
  ccwPredecessorOf ()= _
  ccwSuccessorOf   = _

instance SimplePolygon_ GeoPolygon GeoPositionWithoutCRS' Double where
  uncheckedFromCCWPoints = undefined
  fromPoints = undefined


-- featureCollection ::

-- just make instances

-}
