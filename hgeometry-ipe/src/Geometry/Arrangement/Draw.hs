--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Arrangement.Draw
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Functions for Drawing arrangements

--------------------------------------------------------------------------------
module Geometry.Arrangement.Draw where

import Control.Lens
import Geometry.Arrangement
import Ipe
import Geometry.PlanarSubdivision.Draw

-- | Draws an arrangement
drawArrangement :: (Ord r, Num r) => IpeOut (Arrangement s l v e f r) Group r
drawArrangement = drawPlanarSubdivision' . view subdivision
