{-# LANGUAGE UndecidableInstances #-}
module Algorithms.Geometry.ConvexHull.Minimalist.Point where

import           Control.Lens (to, view)
import           Data.Ext
import           Data.Function (on)
import           Data.Geometry.Point (xCoord)
import qualified Data.Geometry.Point as Point
import           Data.Geometry.Properties
import           Data.Ord (comparing)

--------------------------------------------------------------------------------

class ( Ord (NumType point), Fractional (NumType point)
      , Point.ToAPoint point 3 (NumType point)
      ) => Point point where
  toPt3 :: point -> Point.Point 3 (NumType point)
  toPt3 = view Point.toPoint
  {-# INLINE toPt3 #-}
  compareX :: point -> point -> Ordering
  compareX = comparing (view xCoord . toPt3)
  {-# INLINE compareX #-}

toPt2                                :: Point point
                                     => Time point -> point -> Point.Point 2 (NumType point)
toPt2 t (toPt3 -> Point.Point3 x y z) = Point.Point2 x (z - t*y)
{-# INLINABLE  toPt2 #-}

instance (Ord r, Fractional r) => Point (Point.Point 3 r)

instance (Ord r, Fractional r) => Point (Point.Point 3 r :+ p)



-- compareX' :: forall point r.
--              (Point.ToAPoint point 3 r, Ord r) => point -> point -> Ordering
-- compareX' = comparing (view xCoord . id @(Point.Point 3 r) . view Point.toPoint)


--------------------------------------------------------------------------------

type Time point = NumType point


--------------------------------------------------------------------------------

type Index = Int

class HasIndex a where
  indexOf :: a -> Index

-- | comapre on indices
compareIdx :: HasIndex point => point -> point -> Ordering
compareIdx = comparing indexOf
{-# INLINE compareIdx #-}

data WithIndex a = WithIndex {-# UNPACK #-} !Index a
                     deriving (Show)
type instance NumType (WithIndex a) = NumType a
type instance Dimension (WithIndex a) = Dimension a

instance HasIndex (WithIndex a) where
  indexOf (WithIndex i _) = i
  {-# INLINE indexOf #-}

instance Eq (WithIndex a) where
  (==) = (==) `on` indexOf
instance Ord (WithIndex a) where
  compare = compareIdx

instance Point.ToAPoint point d r => Point.ToAPoint (WithIndex point) d r where
  toPoint = to $ view Point.toPoint . (\(WithIndex _ p) -> p)
  {-# INLINE Point.toPoint #-}

instance Point point => Point (WithIndex point) where
  -- ^ Assumes the indices are assigned in increasing x-coordinates.
  compareX = compareIdx
  {-# INLINE compareX #-}
