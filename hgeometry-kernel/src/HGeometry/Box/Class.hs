{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Box.Class
  ( Box_(..)
  , HasMinPoint(..)
  , HasMaxPoint(..)

  , Rectangle_
  , width
  , height

  , BoxFor
  , IsBoxable(..)

  , size
  ) where

import Control.Lens
import Data.Type.Ord
import GHC.TypeLits
import HGeometry.Interval.Boxed (ClosedInterval)
import HGeometry.Interval.Class
import HGeometry.Point.Class
import HGeometry.Properties
import HGeometry.Vector (Vector)
import HGeometry.Vector.Class
-- import HGeometry.Interval.EndPoint

--------------------------------------------------------------------------------

class HasMinPoint box point where
  -- | Lens to access the lexicographically smallest point
  minPoint :: Lens' box point

class HasMaxPoint box point where
  -- | Lens to access the lexicographically largest point
  maxPoint :: Lens' box point

-- | d-dimensional Boxes
class ( HasMinPoint box point
      , HasMaxPoint box point
      , Point_ (PointFor box) d (NumType box)
      , PointFor box ~ point
      , Dimension box ~ d
      ) => Box_ box d point | box -> d
                            , box -> point where

  -- | Get a vector with the extent of the box in each dimension. Note
  -- that the resulting vector is 0 indexed whereas one would normally
  -- count dimensions starting at zero.
  extent :: ( Vector_ vector d interval
            , ClosedInterval_  interval r
            , NumType box ~ r
            , Num r
            ) => box -> vector

-- | Rectangles are two dimensional boxes.
type Rectangle_ rectangle = Box_ rectangle 2

-- | The data type for boxes
type family BoxFor g

-- | Types for which we can compute an axis parallel boundingbox
class Box_ (BoxFor g) (Dimension g) (NumType g) => IsBoxable g where
  -- | Compute the axis-parallel boundingbox of the given geometry.
  boundingBox :: g -> BoxFor g


--------------------------------------------------------------------------------

-- | D-dimensional boxes.
data Box d point = Box !point !point
  deriving (Show,Eq,Ord)

type instance PointFor  (Box d point) = point
type instance Dimension (Box d point) = d
type instance NumType   (Box d point) = NumType point

instance HasMinPoint (Box d point) point where
  minPoint = lens (\(Box p _) -> p) (\(Box _ q) p -> Box p q)

instance HasMaxPoint (Box d point) point where
  maxPoint = lens (\(Box _ q) -> q) (\(Box p _) q -> Box p q)

instance ( Affine_ point
         , Point_ point d (NumType point)
         ) => Box_ (Box d point) d point where
  extent (Box p q) = vZipWith mkClosedInterval (p^.vector) (q^.vector)

type instance BoxFor (Box d point) = Box d point

instance (Box_ (Box d point) d (NumType point)
         ) => IsBoxable (Box d point) where
  boundingBox = id

--------------------------------------------------------------------------------

-- | Get the size of the box (in all dimensions). Note that the
-- resulting vector is 0 indexed whereas one would normally count
-- dimensions starting at zero.
size :: forall box d point vector vector' interval r. ( Box_ box d point
        , Vector_ vector d r
        , Num r
        , NumType box ~ r
        , NumType interval ~ r
        , Vector_ vector' d interval
        , NumType (IntervalOf Closed r) ~ r
        , HasComponents vector' vector
        , ClosedInterval_ interval r


        , vector' ~ Vector d (IntervalOf Closed r)
        ) => box -> vector
size = f . extent'
  where
    extent' :: box -> vector'
    extent' = extent

    f :: vector' -> vector
    f = over components duration


-- Given a dimension, get the width of the box in that dimension.
-- Dimensions are 0 indexed.
widthIn :: forall i box d point r. ( Box_ box d point
                                   , r ~ NumType box
                                   , i < d, KnownNat i
                                   , Num r
                                   -- , NumType (IntervalOf Closed r) ~ r
                                   ) => box -> r
widthIn = undefined -- view (component @i) . undefined -- size

-- | Get the width of a rectangle.
width :: ( Box_ box d point, 0 < d, r ~ NumType box, Num r
         ) => box -> r
width  = widthIn @0

-- | get the height of a rectangle
height :: ( Box_ box d point, 1 < d, r ~ NumType box, Num r
          ) => box -> r
height = widthIn @1
