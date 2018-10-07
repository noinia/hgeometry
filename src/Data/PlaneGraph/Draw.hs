{-# LANGUAGE ScopedTypeVariables #-}
module Data.PlaneGraph.Draw where

import           Data.Ext
import           Data.Geometry.Ipe
import           Data.PlaneGraph
import qualified Data.Vector as V


drawPlaneGraph   :: IpeOut (PlaneGraph s v e f r) Group r
drawPlaneGraph g = ipeGroup $ concatMap V.toList [vs, es, fs]
  where
    vs = (\(_,VertexData p _) -> iO $ defIO p) <$> vertices g
    es = (\(_,s :+ _)         -> iO $ defIO s) <$> edgeSegments g
    fs = (\(_,f :+ _)         -> iO $ defIO f) <$> rawFacePolygons g


-- drawPlaneGraphWith :: (VertexId' s :+ v -> IpeObject r)
--                    -> (Dart s :+ e      -> IpeObject r)
--                    -> (FaceId' s :+ f   -> IpeObject r)
--                    -> IpeObject
-- drawPlaneGraphWith vertexF edgeF faceF g = asIpeGroup $ vs ++ es ++ fs
--   where
--     vs = map vertices ()
