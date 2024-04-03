{-# LANGUAGE PartialTypeSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Triangulation
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Polygon.Triangulation
  ( triangulate
  , computeDiagonals

  , PolygonEdgeType(..)
  , PolygonFaceData(..)
  , Diagonal
  ) where

import           Control.Lens
import           Data.Coerce
import           Data.Either (lefts)
import qualified Data.Foldable as F
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.PlaneGraph
import           HGeometry.Polygon.Simple
import qualified HGeometry.Polygon.Triangulation.MakeMonotone as MM
import qualified HGeometry.Polygon.Triangulation.TriangulateMonotone as TM
import           HGeometry.Polygon.Triangulation.Types
import           HGeometry.Vector
import           Hiraffe.PlanarGraph

--------------------------------------------------------------------------------

-- | Triangulates a polygon of \(n\) vertices
--
-- running time: \(O(n \log n)\)
triangulate    :: forall s polygon point r.
                  (SimplePolygon_ polygon point r, Ord r, Num r)
               => polygon
               -> PlaneGraph s point PolygonEdgeType PolygonFaceData
triangulate pg = constructGraph pg (computeDiagonals pg)

-- | Computes a set of diagaonals that together triangulate the input polygon
-- of \(n\) vertices.
--
-- running time: \(O(n \log n)\)
computeDiagonals    :: forall polygon point r.
                       (SimplePolygon_ polygon point r, Ord r, Num r)
                    => polygon -> [Diagonal polygon]
computeDiagonals pg = monotoneDiags <> extraDiags
  where
    monotoneSubdiv :: PlaneGraph () point PolygonEdgeType PolygonFaceData
    monotoneSubdiv = MM.makeMonotone @() pg
    -- use some arbitrary proxy type

    -- get the existing diagonals
    monotoneDiags :: [Diagonal polygon]
    monotoneDiags = map (\(d,_) -> let (u,v) = monotoneSubdiv ^. endPointsOf d.asIndex
                                   in coerce $ Vector2 u v
                        )
                  . filter ((== Diagonal) . snd)
                  $ monotoneSubdiv ^.. edges.withIndex

    -- and compute the diagonals in each interior y-monotone polygon
    extraDiags    :: [Diagonal polygon]
    extraDiags    = foldMap (collectDiags . fst)
                  $ filter ((== Inside) . snd)
                  $ monotoneSubdiv^..interiorFaces.withIndex

    collectDiags _i = []

--     -- collectDiags   :: FaceIx planeGraph -> [Diagonal polygon]
--     collectDiags i = let yMonotonePoly  = monotoneSubdiv ^?! interiorFacePolygonAt i
--                      in map (withOriginalId yMonotonePoly) $ TM.computeDiagonals yMonotonePoly

-- withOriginalId :: ( TM.YMonotonePolygon_ yMonotonePolygon
--                                          ((point :+ i) :+ vectorIxPlaneGraph) r
--                   ) => yMonotonePolygon -> Diagonal yMonotonePolygon -> Vector 2 i
-- withOriginalId yMonotonePoly = fmap (\j -> yMonotonePoly^?!vertexAt j.core.extra)

    -- getOriginalID :: VertexId monotonePolygon -> VertexId polygon
