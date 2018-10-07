module Data.Geometry.Arrangement.Draw where

import Control.Lens
import Data.Geometry.Arrangement
import Data.Geometry.Ipe
import Data.Geometry.PlanarSubdivision.Draw



drawArrangement :: IpeOut (Arrangement s l v e f r) Group r
drawArrangement = drawPlanarSubdivision' . view subdivision

drawColoredArrangement :: IpeOut (Arrangement s l v e (Maybe (IpeColor r)) r) Group r
drawColoredArrangement = drawColoredPlanarSubdivision . view subdivision
