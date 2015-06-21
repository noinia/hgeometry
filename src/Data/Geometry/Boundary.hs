module Data.Geometry.Boundary where


import Data.Geometry.Properties
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import           Data.Geometry.Transformation

--------------------------------------------------------------------------------

-- | The boundary of a geometric object.
newtype Boundary g = Boundary g
                   deriving (Show,Eq,Ord,Read,IsTransformable)


type instance NumType (Boundary g)   = NumType g
type instance Dimension (Boundary g) = Dimension g
