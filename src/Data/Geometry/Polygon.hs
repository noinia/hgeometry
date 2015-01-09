module Data.Geometry.Polygon where


-- import           Control.Applicative
-- import           Control.Lens
import qualified Data.Foldable as F

import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Box

import qualified Data.CircularList as C

--------------------------------------------------------------------------------


-- | We distinguish between simple polygons (without holes) and Polygons with holes.
data PolygonType = Simple | Multi


data Polygon (t :: PolygonType) p r where
  SimplePolygon :: C.CList (Point 2 r :+ p)                         -> Polygon Simple p r
  MultiPolygon  :: C.CList (Point 2 r :+ p) -> [Polygon Simple p r] -> Polygon Multi  p r

type SimplePolygon = Polygon Simple

type MultiPolygon  = Polygon Multi


-- | Polygons are per definition 2 dimensional
type instance Dimension (Polygon t p r) = 2
type instance NumType   (Polygon t p r) = r


-- vertices = lens get set


fromPoints :: [Point 2 r :+ p] -> SimplePolygon p r
fromPoints = SimplePolygon . C.fromList
