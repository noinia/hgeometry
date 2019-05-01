--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Arrangement.Draw
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Functions for Drawing arrangements

--------------------------------------------------------------------------------
module Data.Geometry.Arrangement.Draw where

import Control.Lens
import Data.Geometry.Arrangement
import Data.Geometry.Ipe
import Data.Geometry.PlanarSubdivision.Draw

-- | Draws an arrangement
drawArrangement :: IpeOut (Arrangement s l v e f r) Group r
drawArrangement = drawPlanarSubdivision' . view subdivision

-- | Draws an arrangement
drawColoredArrangement :: IpeOut (Arrangement s l v e (Maybe (IpeColor r)) r) Group r
drawColoredArrangement = drawColoredPlanarSubdivision . view subdivision
