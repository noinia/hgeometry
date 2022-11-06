{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Box.Class
  ( Box_(..)
  , HasMinPoint(..)
  , HasMaxPoint(..)

  , Rectangle_
  , width
  , height

  -- , BoxFor
  , IsBoxable(..)

  , size
  ) where

import Control.Lens
import Data.Kind
import Data.Type.Ord
import GHC.TypeLits
import HGeometry.Interval.Boxed (ClosedInterval)
import HGeometry.Interval.Class
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector

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
      , NumType box ~ NumType point
      ) => Box_ box d point | box -> d
                            , box -> point where

  -- | Get a vector with the extent of the box in each dimension. Note
  -- that the resulting vector is 0 indexed whereas one would normally
  -- count dimensions starting at zero.
  extent :: ( OptVector_ d (IntervalFor box)
            , ClosedInterval_ (IntervalFor box) r
            , NumType box ~ r
            , Num r
            ) => box -> Vector d (IntervalFor box)

-- | Rectangles are two dimensional boxes.
type Rectangle_ rectangle = Box_ rectangle 2

-- -- | The data type for boxes
-- type family BoxFor g

-- | Types for which we can compute an axis parallel boundingbox
class IsBoxable g where
  -- | Compute the axis-parallel boundingbox of the given geometry.
  boundingBox :: g -> Box (Dimension g) (Point (Dimension g) (NumType g))


--------------------------------------------------------------------------------

-- | D-dimensional boxes.
type Box         :: Nat -> Type -> Type
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

-- type instance BoxFor (Box d point) = Box d point

instance ( Box_ (Box d point) d (NumType point)
         , Point_ point d r
         , OptVector_ d r
         , Metric_ (VectorFamily' d r)
         ) => IsBoxable (Box d point) where
  boundingBox (Box p q) = Box (pointFromPoint p) (pointFromPoint q)

type instance IntervalFor (Box d point) = ClosedInterval (NumType point)

--------------------------------------------------------------------------------

-- | Get the size of the box (in all dimensions). Note that the
-- resulting vector is 0 indexed whereas one would normally count
-- dimensions starting at zero.
size :: forall box d point r.
        ( Box_ box d point
        , r ~ NumType box
        , Num r
        , ClosedInterval_ (IntervalFor box) r
        , OptVector_ d r
        , OptVector_ d (IntervalFor box)
        , HasComponents   (VectorFamily' d (IntervalFor box)) (VectorFamily' d r)
        ) => box -> Vector d r
size = over components duration . extent

-- Given a dimension, get the width of the box in that dimension.
-- Dimensions are 0 indexed.
widthIn :: forall i box d point r. ( Box_ box d point
                                   , r ~ NumType box
                                   , i < d, KnownNat i
                                   , OptVector_ d (IntervalFor box)
                                   , OptVector_ d r
                                   , HasComponents (VectorFamily' d (IntervalFor box))
                                                   (VectorFamily' d r)
                                   , ClosedInterval_ (IntervalFor box) r
                                   , Num r
                                   ) => box -> r
widthIn = view (component @i) . size



-- | Get the width of a rectangle.
width :: ( Box_ box d point
         , 0 < d
         , r ~ NumType box
         , OptVector_ d r
         , OptVector_ d (IntervalFor box)
         , HasComponents (VectorFamily' d (IntervalFor box))
                         (VectorFamily' d r)
         , ClosedInterval_ (IntervalFor box) r
         , Num r
         ) => box -> r
width  = widthIn @0

-- | get the height of a rectangle
height :: ( Box_ box d point
         , 1 < d
         , r ~ NumType box
         , OptVector_ d r
         , OptVector_ d (IntervalFor box)
         , HasComponents (VectorFamily' d (IntervalFor box))
                         (VectorFamily' d r)
         , ClosedInterval_ (IntervalFor box) r
         , Num r
         ) => box -> r
height = widthIn @1

-- -}

-- width = undefined
-- height  = undefined
