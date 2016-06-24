module Data.Geometry.Boundary where

import           Data.Geometry.Properties
import           Data.Geometry.Transformation

--------------------------------------------------------------------------------

-- | The boundary of a geometric object.
newtype Boundary g = Boundary g
                   deriving (Show,Eq,Ord,Read,IsTransformable)


type instance NumType (Boundary g)   = NumType g
type instance Dimension (Boundary g) = Dimension g


-- | Result of a query that asks if something is Inside a g, *on* the boundary
-- of the g, or outside.
data PointLocationResult = Inside | OnBoundary | Outside deriving (Show,Read,Eq)
