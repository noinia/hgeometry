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

import           Control.Lens hiding (rmap)
import           HGeometry.Ext
import qualified Ipe.Attributes as AT
import           Ipe.Attributes hiding (Matrix)
import           Ipe.Types
import           HGeometry.Properties
import           HGeometry.Transformation
import           Data.Proxy
import           Data.Vinyl hiding (Label)

--------------------------------------------------------------------------------

-- | Takes and applies the ipe Matrix attribute of this item.
applyMatrix'              :: ( IsTransformable (i r)
                             , AT.Matrix ∈ AttributesOf i
                             , Dimension (i r) ~ 2, r ~ NumType (i r))
                          => IpeObject' i r -> IpeObject' i r
applyMatrix' o@(i :+ ats) = maybe o (\m -> transformBy (Transformation m) i :+ ats') mm
  where
    (mm,ats') = takeAttr (Proxy :: Proxy AT.Matrix) ats

-- | Applies the matrix to an ipe object if it has one.
applyMatrix                  :: (Fractional r, Eq r) => IpeObject r -> IpeObject r
applyMatrix (IpeGroup i)     = IpeGroup . applyMatrix'
                             $ i&core.groupItems.traverse %~ applyMatrix
                             -- note that for a group we first (recursively)
                             -- apply the matrices, and then apply
                             -- the matrix of the group to its members.
applyMatrix (IpeImage i)     = IpeImage     $ applyMatrix' i
applyMatrix (IpeTextLabel i) = IpeTextLabel $ applyMatrix' i
applyMatrix (IpeMiniPage i)  = IpeMiniPage  $ applyMatrix' i
applyMatrix (IpeUse i)       = IpeUse       $ applyMatrix' i
applyMatrix (IpePath i)      = IpePath      $ applyMatrix' i

-- | Applies all matrices in the file.
applyMatrices   :: (Fractional r, Eq r) => IpeFile r -> IpeFile r
applyMatrices f = f&pages.traverse %~ applyMatricesPage

-- | Applies all Matrices on a given page.
applyMatricesPage   :: (Fractional r, Eq r) => IpePage r -> IpePage r
applyMatricesPage p = p&content.traverse %~ applyMatrix
