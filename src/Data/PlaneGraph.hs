module Data.PlaneGraph( module Data.PlanarGraph
                      , PlaneGraph

                      , withEdgeDistances
                      , faceToSimplePolygon
                      ) where

import Data.Ext
import Control.Lens
import Data.PlanarGraph
import Data.Geometry.Point
import Data.Geometry.Polygon(fromPoints, SimplePolygon)
import Data.Geometry.Properties
import qualified Data.Vector as V


--------------------------------------------------------------------------------

type PlaneGraph s w v e f r = PlanarGraph s w (Point 2 r :+ v) e f

type instance NumType (PlaneGraph s w v e f r) = r


-- | Labels the edges of a plane graph with their distances, as specified by
-- the distance function.
withEdgeDistances     :: (Point 2 r ->  Point 2 r -> a)
                      -> PlaneGraph s w p e f r -> PlaneGraph s w p (a :+ e) f r
withEdgeDistances f g = g&dartData %~ fmap (\(d,x) -> (d,len d :+ x))
  where
    len d = uncurry f . over both (^.core) $ endPointData d g


faceToSimplePolygon      :: FaceId s w -> PlaneGraph s w p e f r
                         -> SimplePolygon p r :+ f
faceToSimplePolygon i gr = poly (boundaryVertices i gr) :+ gr^.fDataOf i
  where
    poly = fromPoints . V.toList . fmap (\j -> gr^.vDataOf j)
