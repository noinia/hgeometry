module Data.Geometry.Arrangement.Draw where

import Control.Lens
import Data.Geometry.Arrangement
import Data.Geometry.Ipe
import Data.Geometry.PlanarSubdivision.Draw



drawArrangement :: IpeOut (Arrangement s l v e f r) (IpeObject r)
drawArrangement = dimap (^.subdivision) (asIpeGroup . (: [])) drawPlanarSubdivision


drawColoredArrangement :: IpeOut (Arrangement s l v e (Maybe (IpeColor r)) r) (IpeObject r)
drawColoredArrangement = lmap (^.subdivision) drawColoredPlanarSubdivision
