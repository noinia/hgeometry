module Data.Geometry.Arrangement.Draw where

import Control.Lens
import Data.Geometry.Arrangement
import Data.Geometry.Ipe
import Data.Geometry.PlanarSubdivision.Draw



drawArrangement :: forall s l v e f r. IpeOut (Arrangement s l v e f r) (IpeObject r)
drawArrangement = IpeOut draw
  where
    draw     :: Arrangement s l v e f r -> IpeObject r
    draw arr = asIpeGroup [subdiv]
      where
        subdiv = asIpe drawPlanarSubdivision $ arr^.subdivision
