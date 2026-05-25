--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.Matrix
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Matrix Attributes as defined in Ipe
--
--------------------------------------------------------------------------------
module Ipe.Matrix where

import Control.Lens hiding (rmap, transform)
import HGeometry.Ext
import Ipe.Attributes
import Ipe.Types
import HGeometry.Transformation

--------------------------------------------------------------------------------

-- | Takes and Applies the matrix to an ipe object if it has one. Also
-- applies it recursively to any groups.
applyMatrix :: (Fractional r, Eq r) => IpeObject r -> IpeObject r
applyMatrix = applyMatrix' . \case
  IpeGroup gr -> IpeGroup $ gr&core.groupItems.mapped %~ applyMatrix
  o           -> o

-- | Take and apply the matrix at this particular object. Does *not*
-- apply it recursively.
applyMatrix'   :: (Fractional r, Eq r) => IpeObject r -> IpeObject r
applyMatrix' o = maybe o transform $ o^.matrix
  where
    transform m = (transformBy (Transformation m) o)&matrix .~ Nothing

-- | Applies all matrices in the file.
applyMatrices   :: (Fractional r, Eq r) => IpeFile r -> IpeFile r
applyMatrices f = f&pages.traverse %~ applyMatricesPage

-- | Applies all Matrices on a given page.
applyMatricesPage   :: (Fractional r, Eq r) => IpePage r -> IpePage r
applyMatricesPage p = p&content.traverse %~ applyMatrix
