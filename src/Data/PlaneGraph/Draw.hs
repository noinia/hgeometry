{-# LANGUAGE ScopedTypeVariables #-}
module Data.PlaneGraph.Draw where

import Data.PlaneGraph
import           Data.Geometry.Ipe
import           Data.Ext
import           Control.Lens
import qualified Data.Vector as V



drawPlaneGraph :: forall s v e f r. IpeOut (PlaneGraph s v e f r) (IpeObject r)
drawPlaneGraph = IpeOut draw
  where
    draw   :: PlaneGraph s v e f r -> IpeObject r
    draw g = asIpeGroup $ concatMap V.toList [vs, es, fs]
      where
        vs = (\(_,VertexData p _) -> asIpeObject p mempty) <$> vertices g
        es = (\(_,s :+ _) -> asIpeObject s mempty) <$> edgeSegments g
        fs = (\(_,f :+ _) -> asIpeObject f mempty) <$> rawFacePolygons g


-- drawPlaneGraphWith :: (VertexId' s :+ v -> IpeObject r)
--                    -> (Dart s :+ e      -> IpeObject r)
--                    -> (FaceId' s :+ f   -> IpeObject r)
--                    -> IpeObject
-- drawPlaneGraphWith vertexF edgeF faceF g = asIpeGroup $ vs ++ es ++ fs
--   where
--     vs = map vertices ()
