{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Box.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- d dimensional boxes
--
--------------------------------------------------------------------------------
module HGeometry.Box.Class
  ( Box_(..)
  , HasMinPoint(..)
  , HasMaxPoint(..)
  , centerPoint

  , Rectangle_
  , width
  , height


  , size
  ) where

import Control.Lens
-- import Control.Subcategory.Functor
import HGeometry.Ext
import Data.Type.Ord
import GHC.TypeLits
import HGeometry.Interval
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------

-- $setup
-- >>> let myRect = Rectangle (Point2 1 1) (Point2 10 20.0) :: Rectangle (Point 2 Double)

-- | Types that have a 'minPoint' field lens
class HasMinPoint box point | box -> point where
  -- | Lens to access the lexicographically smallest point
  minPoint :: Lens' box point

-- | Types that have a 'maxPoint' field lens
class HasMaxPoint box point | box -> point where
  -- | Lens to access the lexicographically largest point
  maxPoint :: Lens' box point

-- | d-dimensional Boxes
class ( HasMinPoint box point
      , HasMaxPoint box point
      , Point_ point (Dimension box) (NumType box)
      -- , PointFor box ~ point
      -- , NumType box ~ NumType point
      ) => Box_ box point | box -> point where

  -- | Get a vector with the extent of the box in each dimension. Note
  -- that the resulting vector is 0 indexed whereas one would normally
  -- count dimensions starting at zero.
  extent :: ( r ~ NumType box
            , d ~ Dimension box
            , Num r
            ) => box -> Vector d (ClosedInterval r)

-- | Rectangles are two dimensional boxes.
type Rectangle_ rectangle point = (Box_ rectangle point, Dimension rectangle ~ 2)

--------------------------------------------------------------------------------

-- | Get the size of the box (in all dimensions). Note that the
-- resulting vector is 0 indexed whereas one would normally count
-- dimensions starting at zero.
--
-- >>> size myRect
-- Vector2 9.0 19.0
size :: forall box d point r.
        ( Box_ box point, Point_ point d r
        , Num r
        , Functor (Vector d)
        ) => box -> Vector d r
size = fmap duration . extent


-- | Given a dimension, get the width of the box in that dimension.
-- Dimensions are 0 indexed.
widthIn :: forall i box d point r. ( Box_ box point, Point_ point d r
                                   , i <= d - 1
                                   , KnownNat i
                                   , Functor (Vector d)
                                   , Num r
                                   ) => box -> r
widthIn = view (component @i) . size


-- | Get the width of a rectangle.
width :: ( Box_ box point, Point_ point d r
         , 1 <= d
         , Functor (Vector d)
         , Num r
         ) => box -> r
width  = widthIn @0

-- | get the height of a rectangle
height :: ( Box_ box point, Point_ point d r
         , 2 <= d
         , Functor (Vector d)
         , Num r
         ) => box -> r
height = widthIn @1

-- | Get the center point of a box
--
-- >>> centerPoint myRect
-- Point2 5.5 10.5
centerPoint   :: (Box_ box point, Point_ point d r, Fractional r)
              => box -> point
centerPoint r = let v = (r^.maxPoint) .-. (r^.minPoint)
                in (r^.minPoint) .+^ ((1/2) *^ v)

--------------------------------------------------------------------------------

instance HasMinPoint box point => HasMinPoint (box :+ extra) point where
  minPoint = core.minPoint
  {-# INLINE minPoint #-}
instance HasMaxPoint box point => HasMaxPoint (box :+ extra) point where
  maxPoint = core.maxPoint
  {-# INLINE maxPoint #-}
instance Box_ box point => Box_ (box :+ extra) point where
  extent = extent . view core
  {-# INLINE extent #-}
