--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Boundary
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Geometry.Boundary
  ( Boundary(Boundary)
  , _Boundary
  , PointLocationResult(..)

  , InRegion(..)
  ) where

import Control.Lens (iso,Iso)
import Geometry.Point.Class
import Geometry.Properties
import Geometry.Transformation

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

-- | Class for regions that support In region tests.
class InRegion region where
  {-# MINIMAL inRegion #-}
  -- | Test where the point lies with respect to the region.
  inRegion :: (Point_ point d r
              , NumType region ~ r, Dimension region ~ d)
           => point d r -> region -> PointLocationResult

  -- | Test if the point lies strictly inside the region
  insideRegion     :: (Point_ point d r
                      , NumType region ~ r, Dimension region ~ d)
                   => point d r -> region -> Bool
  insideRegion q r = inRegion q r == Inside
