{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlaneGraph.Draw
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Helper functions to draw a PlaneGraph in ipe
--
--------------------------------------------------------------------------------
module Data.PlaneGraph.Draw where

import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.LineSegment
import           Data.Geometry.Polygon
import           Data.Maybe (mapMaybe)
import           Data.PlaneGraph
import           Ipe

--------------------------------------------------------------------------------

-- | Draws only the values for which we have a Just attribute
drawPlaneGraph :: forall s r. (Num r, Ord r)
               => IpeOut (PlaneGraph s (Maybe (IpeAttributes IpeSymbol r))
                                       (Maybe (IpeAttributes Path  r))
                                       (Maybe (IpeAttributes Path  r))
                           r) Group r
drawPlaneGraph = drawPlaneGraphWith fv fe ff ff
  where
    fv                     :: (VertexId' s, VertexData r (Maybe (IpeAttributes IpeSymbol r)))
                           -> Maybe (IpeObject' IpeSymbol r)
    fv (_,VertexData p ma) = (\a -> defIO p ! a) <$> ma -- draws a point
    fe (_,s :+ ma)         = (\a -> defIO s ! a) <$> ma -- draw segment
    ff (_,f :+ ma)         = (\a -> defIO f ! a) <$> ma -- draw a face

-- | Draw everything using the defaults
drawPlaneGraph'    :: forall s v e f r. (Ord r, Num r)
                   => IpeOut (PlaneGraph s v e f r) Group r
drawPlaneGraph' pg = drawPlaneGraph
  (pg&vertexData.traverse   ?~ (mempty :: IpeAttributes IpeSymbol r)
     &dartData.traverse._2  ?~ (mempty :: IpeAttributes Path      r)
     &faceData.traverse     ?~ (mempty :: IpeAttributes Path      r))


-- | Function to draw a graph by giving functions that specify how to
-- render vertices, edges, and faces.
drawPlaneGraphWith                 :: (ToObject vi, ToObject ei, ToObject fi, Num r, Ord r)
                                   => IpeOut' Maybe (VertexId' s, VertexData r v)          vi r
                                   -> IpeOut' Maybe (Dart s,      LineSegment 2 v r :+ e)  ei r
                                   -> IpeOut' Maybe (FaceId' s,   SimplePolygon v r :+ f)  fi r
                                   -> IpeOut' Maybe (FaceId' s,   MultiPolygon (Maybe v) r :+ f)   fi r
                                   -> IpeOut (PlaneGraph s v e f r) Group r
drawPlaneGraphWith fs fe fif fof g = drawPlaneGraphWith' (outerFaceId g) fs fe fif fof g

-- | Function to draw a graph by giving the outer faceId and the
-- functions that specify how to render vertices, edges, and faces.
drawPlaneGraphWith'                    :: (ToObject vi, ToObject ei, ToObject fi, Num r, Ord r)
                                      => FaceId' s -- ^ outerface Id
                                      -> IpeOut' Maybe (VertexId' s, VertexData r v)          vi r
                                      -> IpeOut' Maybe (Dart s,      LineSegment 2 v r :+ e)  ei r
                                      -> IpeOut' Maybe (FaceId' s,   SimplePolygon v r :+ f)  fi r
                                      -> IpeOut' Maybe (FaceId' s,   MultiPolygon (Maybe v) r :+ f)   fi r
                                      -> IpeOut (PlaneGraph s v e f r) Group r
drawPlaneGraphWith' i fv fe fif fof g = ipeGroup . concat $ [vs, es, ifs, of']
  where
    (outerF,innerFs) = facePolygons i g
    vs  = mapMaybe (fmap iO . fv)  . F.toList . vertices        $ g
    es  = mapMaybe (fmap iO . fe)  . F.toList . edgeSegments    $ g
    ifs = mapMaybe (fmap iO . fif) . F.toList $ innerFs
    of' = mapMaybe (fmap iO . fof) [outerF]
