module Data.Geometry.PlanarSubdivision.Draw where

import           Data.Geometry.Ipe
import           Data.Ext
import           Control.Lens
import qualified Data.Vector as V
import Data.Geometry.PlanarSubdivision

drawPlanarSubdivision :: forall s v e f r. IpeOut (PlanarSubdivision s v e f r) (IpeObject r)
drawPlanarSubdivision = IpeOut draw
  where
    draw   :: PlanarSubdivision s v e f r -> IpeObject r
    draw g = asIpeGroup $ concatMap V.toList [vs, es, fs]
      where
        vs = (\(_,VertexData p _) -> asIpeObject p mempty) <$> vertices g
        es = (\(_,s :+ _) -> asIpeObject s mempty) <$> edgeSegments g
        fs = (\(_,f :+ _) -> asIpeObject f mempty) <$> rawFacePolygons g
