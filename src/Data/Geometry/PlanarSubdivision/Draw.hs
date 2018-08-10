module Data.Geometry.PlanarSubdivision.Draw where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.Ipe
import           Data.Geometry.PlanarSubdivision
import qualified Data.Vector as V
import           Data.Vinyl

drawPlanarSubdivision :: forall s v e f r. IpeOut (PlanarSubdivision s v e f r) (IpeObject r)
drawPlanarSubdivision = IpeOut draw
  where
    draw   :: PlanarSubdivision s v e f r -> IpeObject r
    draw g = asIpeGroup $ concatMap V.toList [vs, es, fs]
      where
        vs = (\(_,VertexData p _) -> asIpeObject p mempty) <$> vertices g
        es = (\(_,s :+ _) -> asIpeObject s mempty) <$> edgeSegments g
        fs = (\(_,f :+ _) -> asIpeObject f mempty) <$> rawFacePolygons g


drawColoredPlanarSubdivision :: forall s v e r.
                                IpeOut (PlanarSubdivision s v e (Maybe (IpeColor r)) r)
                                       (IpeObject r)
drawColoredPlanarSubdivision = IpeOut draw
  where
    draw   :: PlanarSubdivision s v e (Maybe (IpeColor r)) r -> IpeObject r
    draw g = asIpeGroup $ concatMap V.toList [vs, es, fs]
      where
        vs = (\(_,VertexData p _) -> asIpeObject p mempty) <$> vertices g
        es = (\(_,s :+ _)  -> asIpeObject s mempty) <$> edgeSegments g
        fs = (\(_,f :+ mc) -> asIpeObject f (mkFill mc)) <$> rawFacePolygons g
        mkFill = maybe mempty (attr SFill)
