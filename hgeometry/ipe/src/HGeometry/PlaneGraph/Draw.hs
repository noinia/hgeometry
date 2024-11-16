--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlaneGraph.Draw
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Helper functions to draw a Plane Graphs  in ipe
--
--------------------------------------------------------------------------------
module HGeometry.PlaneGraph.Draw
  (

  ) where

import           Control.Lens
import           Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.PlaneGraph
import           HGeometry.Polygon
import           Ipe

--------------------------------------------------------------------------------

-- | Draws only the values for which we have a Just attribute
drawPlaneGraph :: forall s r. (Num r, Ord r) =>
                         IpeOut (PlaneGraph s (Maybe (IpeAttributes IpeSymbol r))
                                                     (Maybe (IpeAttributes Path      r))
                                                     (Maybe (IpeAttributes Path      r))
                                r) Group r
drawPlaneGraph = drawPlaneGraphWith fv fe ff ff
  where
    fv                     :: (VertexId' s, VertexData r (Maybe (IpeAttributes IpeSymbol r)))
                           -> Maybe (IpeObject' IpeSymbol r)
    fv (_,VertexData p ma) = (\a -> defIO p ! a) <$> ma -- draws a point
    fe (_,s :+ ma)         = (\a -> defIO s ! a) <$> ma -- draw segment
    ff (_,f :+ ma)         = (\a -> defIO f ! a) <$> ma -- draw a face


-- | Draw everything using the defaults
drawPlaneGraph'    :: forall s v e f r. (Num r, Ord r)
                          => IpeOut (PlaneGraph s v e f r) Group r
drawPlaneGraph' ps = drawPlaneGraph
  (ps&vertexData.traverse   ?~ (mempty :: IpeAttributes IpeSymbol r)
     &dartData.traverse._2  ?~ (mempty :: IpeAttributes Path      r)
     &faceData.traverse     ?~ (mempty :: IpeAttributes Path      r))


-- | Function to draw a planar subdivision by giving functions that
-- specify how to render vertices, edges, the internal faces, and the outer face.
drawPlaneGraphWith               :: ( PlaneGraph_ planeGraph vertex
                                    , ToObject vi, ToObject ei, ToObject fi, Num r, Ord r
                                    ,
                                    )
                                 => IpeOut' Maybe (VertexIx planeGraph, vertex)    vi r
                                 -> IpeOut' Maybe (DartIx planeGraph,

  Dart s,     ClosedLineSegment v :+ e) ei r
                                 -> IpeOut' Maybe (FaceId' s,  SimplePolygon v :+ f)     fi r
                                 -> IpeOut' Maybe (FaceId' s,  MultiPolygon v r :+ f)    fi r
                                 -> IpeOut planeGraph Group r


                                 (PlaneGraph s v e f r) Group r
drawPlaneGraphWith fv fe fi fo g = ipeGroup . concat $ [o <> fs, es, vs]
  where
    vs = mapMaybe (fmap iO . fv) $ g^..vertices

      . V.toList . vertices     $ g
    es = mapMaybe (fmap iO . fe)  . V.toList . edgeSegments $ g
    fs = mapMaybe (fmap iO . fi) . V.toList . internalFacePolygons $ g
    o  = mapMaybe (fmap iO . fo) [(outerFaceId g, outerFacePolygon g)]
