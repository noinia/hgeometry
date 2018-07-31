module Data.Geometry.Arrangement.Draw where

import Control.Lens
import Data.Ext
import Data.Geometry.Arrangement
import Data.Geometry.Ipe
import Data.Geometry.PlanarSubdivision.Draw



drawArrangement :: forall s v e f r. IpeOut (Arrangement s v e f r) (IpeObject r)
drawArrangement = IpeOut draw
  where
    draw     :: Arrangement s v e f r -> IpeObject r
    draw arr = asIpeGroup [subdiv]
      where
        subdiv = asIpe drawPlanarSubdivision $ arr^.subdivision
