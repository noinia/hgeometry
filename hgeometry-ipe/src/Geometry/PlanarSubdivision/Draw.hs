--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.PlanarSubdivision.Draw
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Helper functions to draw a PlanarSubdivision in ipe
--
--------------------------------------------------------------------------------
module Geometry.PlanarSubdivision.Draw where

import           Control.Lens
import           Data.Ext
import           Ipe
import           Geometry.LineSegment
import           Geometry.PlanarSubdivision
import           Geometry.Polygon
import           Data.Maybe (mapMaybe)
import qualified Data.Vector as V

--------------------------------------------------------------------------------

-- | Draws only the values for which we have a Just attribute
drawPlanarSubdivision :: forall s r. (Num r, Ord r) =>
                         IpeOut (PlanarSubdivision s (Maybe (IpeAttributes IpeSymbol r))
                                                     (Maybe (IpeAttributes Path      r))
                                                     (Maybe (IpeAttributes Path      r))
                                r) Group r
drawPlanarSubdivision = drawPlanarSubdivisionWith fv fe ff ff
  where
    fv                     :: (VertexId' s, VertexData r (Maybe (IpeAttributes IpeSymbol r)))
                           -> Maybe (IpeObject' IpeSymbol r)
    fv (_,VertexData p ma) = (\a -> defIO p ! a) <$> ma -- draws a point
    fe (_,s :+ ma)         = (\a -> defIO s ! a) <$> ma -- draw segment
    ff (_,f :+ ma)         = (\a -> defIO f ! a) <$> ma -- draw a face


-- | Draw everything using the defaults
drawPlanarSubdivision'    :: forall s v e f r. (Num r, Ord r)
                          => IpeOut (PlanarSubdivision s v e f r) Group r
drawPlanarSubdivision' ps = drawPlanarSubdivision
  (ps&vertexData.traverse   ?~ (mempty :: IpeAttributes IpeSymbol r)
     &dartData.traverse._2  ?~ (mempty :: IpeAttributes Path      r)
     &faceData.traverse     ?~ (mempty :: IpeAttributes Path      r))


-- | Function to draw a planar subdivision by giving functions that
-- specify how to render vertices, edges, the internal faces, and the outer face.
drawPlanarSubdivisionWith                 :: (ToObject vi, ToObject ei, ToObject fi, Num r, Ord r)
                                          => IpeOut' Maybe (VertexId' s, VertexData r v)          vi r
                                          -> IpeOut' Maybe (Dart s,      LineSegment 2 v r :+ e)  ei r
                                          -> IpeOut' Maybe (FaceId' s,   SomePolygon v r :+ f)    fi r
                                          -> IpeOut' Maybe (FaceId' s,   MultiPolygon (Maybe v) r :+ f)    fi r
                                          -> IpeOut (PlanarSubdivision s v e f r) Group r
drawPlanarSubdivisionWith fv fe fi fo g = ipeGroup . concat $ [o <> fs, es, vs]
  where
    vs = mapMaybe (fmap iO . fv)  . V.toList . vertices     $ g
    es = mapMaybe (fmap iO . fe)  . V.toList . edgeSegments $ g
    fs = mapMaybe (fmap iO . fi) . V.toList . internalFacePolygons $ g
    o  = mapMaybe (fmap iO . fo) [(outerFaceId g, outerFacePolygon g)]
