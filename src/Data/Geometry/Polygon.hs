{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Data.Geometry.Polygon where


import           Control.Applicative
import           Control.Lens hiding (Simple)
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



outerBoundary :: forall t p r. Lens' (Polygon t p r) (C.CList (Point 2 r :+ p))
outerBoundary = lens get set
  where
    get                     :: Polygon t p r -> C.CList (Point 2 r :+ p)
    get (SimplePolygon vs)  = vs
    get (MultiPolygon vs _) = vs

    set                           :: Polygon t p r -> C.CList (Point 2 r :+ p) -> Polygon t p r
    set (SimplePolygon _)      vs = SimplePolygon vs
    set (MultiPolygon  _   hs) vs = MultiPolygon vs hs

holes :: forall p r. Lens' (Polygon Multi p r) [Polygon Simple p r]
holes = lens get set
  where
    get :: Polygon Multi p r -> [Polygon Simple p r]
    get (MultiPolygon _ hs) = hs
    set :: Polygon Multi p r -> [Polygon Simple p r] -> Polygon Multi p r
    set (MultiPolygon vs _) hs = MultiPolygon vs hs


-- vertices :: Polygon t p r -> [Point 2 r :+ p]
-- vertices (SimplePolygon vs)   = F.toList vs
-- vertices (MultiPolygon vs hs) = concat (F.toList vs : hs)



fromPoints :: [Point 2 r :+ p] -> SimplePolygon p r
fromPoints = SimplePolygon . C.fromList
