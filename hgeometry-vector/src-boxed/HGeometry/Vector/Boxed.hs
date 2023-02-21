module HGeometry.Vector.Boxed
  ( Vector(..)
  ) where

--------------------------------------------------------------------------------


import Control.Lens
import Data.Kind (Type)
import GHC.TypeLits
import HGeometry.Properties
import HGeometry.Vector.Class
import           Linear.V1 (V1(..))
import           Linear.V2 (V2(..))
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))

--------------------------------------------------------------------------------

-- | d-dimensional boxed vectors
data family Vector (d :: Nat) (r :: Type) :: Type

type instance Index     (Vector d r) = Int
type instance IxValue   (Vector d r) = r
type instance NumType   (Vector d r) = r
type instance Dimension (Vector d r) = d
