{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Geometry.Polygon.MultiPolygon
  ( MultiPolygon

  ) where

import           Control.Lens hiding (holes)
import qualified Data.Foldable as F
import           Data.Vector (Vector)
-- import qualified Data.Vector as Vector
import           Geometry.Polygon.Class
import           Geometry.Polygon.Simple
import           Geometry.Point.Class
import           Geometry.Properties

--------------------------------------------------------------------------------

-- | A polygon that may have holes.
data MultiPolygon point r = MultiPolygon { _outerBoundaryPolygon :: SimplePolygon point r
                                         , _theHoles             :: Vector (SimplePolygon point r)
                                         }

type instance Dimension (MultiPolygon point r) = 2
type instance NumType   (MultiPolygon point r) = r

-- | The outer boundary of the polygon
outerBoundaryPolygon :: IndexedGetter ComponentIx (MultiPolygon point r) (SimplePolygon point r)
outerBoundaryPolygon = undefined -- ito (\pg -> (0,_outerBoundaryPolygon pg))

-- | Fold over all holes in the polygon
holes :: IndexedFold ComponentIx (MultiPolygon point r) (SimplePolygon point r)
holes = to _theHoles . reindexed (+1) itraversed

type ComponentIx = Int

instance HasVertices' (MultiPolygon point r) where
  type Vertex   (MultiPolygon point r) = Vertex (SimplePolygon point r)
  type VertexIx (MultiPolygon point r) = (ComponentIx, VertexIx (SimplePolygon point r))
  vertexAt (i,j)
    | i == 0    = undefined -- outerBoundaryPolygon .> vertexAt j
    | otherwise = undefined -- (holes .> iix i)     <.> vertexAt j


instance HasVertices (MultiPolygon point r) (MultiPolygon point' r') where
  vertices f (MultiPolygon outerPG holes') =
    MultiPolygon <$> traverseWithComponentIx 0 f outerPG
                 <*> itraverse (\j h -> traverseWithComponentIx (j+1) f h) holes'

-- | constructs a traversal with the hole index as well
traverseWithComponentIx   :: ComponentIx -- ^ the holeIx
                           -> IndexedTraversal (ComponentIx,Int)
                                               (SimplePolygon point r) (SimplePolygon point' r')
                                               (point 2 r)             (point' 2 r')
traverseWithComponentIx h = reindexed (h,) vertices

instance HasOuterBoundary (MultiPolygon point r) where
  outerBoundary = undefined -- outerBoundaryPolygon .> traverseWithComponentIx 0
  outerBoundaryVertexAt (i,j)
    | i == 0    = undefined -- outerBoundaryPolygon . outerBoundaryVertexAt j
    | otherwise = error "outerBoundaryVertexAt: vertex not on the outer boundary!"
  outerBoundaryEdges = undefined
  outerBoundaryEdgeAt (i,j)
    | i == 0    = undefined -- outerBoundaryPolygon . outerBoundaryVertexAt j
    | otherwise = error "outerBoundaryEdgeAt: starting vertex not on the outer boundary!"

instance ( Show (point 2 r)
         , SimplePolygon_ SimplePolygon point r
         ) => Show (MultiPolygon point r) where
  show (MultiPolygon outerPG hs) = "MultiPolygon (" <> show (outerPG^..vertices) <> ") ("
                                                    <> show hs <> ")"

instance (Point_ point 2 r) => Polygon_ MultiPolygon point r where
  area (MultiPolygon outer hs) = area outer - sum [area h | h <- F.toList hs]
