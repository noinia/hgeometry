module Data.Geometry.Boundary where

import Control.Lens (iso,Iso)
import Data.Geometry.Properties
import Data.Geometry.Transformation

--------------------------------------------------------------------------------

-- | The boundary of a geometric object.
newtype Boundary g = Boundary g
                   deriving (Show,Eq,Ord,Read,IsTransformable
                            ,Functor,Foldable,Traversable)

type instance NumType (Boundary g)   = NumType g
type instance Dimension (Boundary g) = Dimension g

-- | Iso for converting between things with a boundary and without its boundary
_Boundary :: Iso g h (Boundary g) (Boundary h)
_Boundary = iso Boundary (\(Boundary b) -> b)


-- | Result of a query that asks if something is Inside a g, *on* the boundary
-- of the g, or outside.
data PointLocationResult = Inside | OnBoundary | Outside deriving (Show,Read,Eq)
