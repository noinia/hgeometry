module Data.Geometry.Ipe.Matrix where

import           Control.Lens hiding (rmap)
import           Data.Ext
import qualified Data.Geometry.Ipe.Attributes as AT
import           Data.Geometry.Ipe.Attributes hiding (Matrix)
import           Data.Geometry.Ipe.Types
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
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
applyMatrix                  :: Fractional r => IpeObject r -> IpeObject r
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

applyMatrices   :: Fractional r => IpeFile r -> IpeFile r
applyMatrices f = f&pages.traverse %~ applyMatricesPage

applyMatricesPage   :: Fractional r => IpePage r -> IpePage r
applyMatricesPage p = p&content.traverse %~ applyMatrix
