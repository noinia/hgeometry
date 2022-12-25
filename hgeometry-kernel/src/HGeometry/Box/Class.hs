{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
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

  , Rectangle_
  , width
  , height


  , size
  ) where

import Control.Lens
import Data.Type.Ord
import GHC.TypeLits
import HGeometry.Interval
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------

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
  extent :: ( OptVector_ d (ClosedInterval r)
            , r ~ NumType box
            , d ~ Dimension box
            , Num r
            ) => box -> Vector d (ClosedInterval r)

-- | Rectangles are two dimensional boxes.
type Rectangle_ rectangle point = (Box_ rectangle point, Dimension rectangle ~ 2)



--------------------------------------------------------------------------------

--------------------------------------------------------------------------------




-- | Get the size of the box (in all dimensions). Note that the
-- resulting vector is 0 indexed whereas one would normally count
-- dimensions starting at zero.
--
--
size :: forall box d point r.
        ( Box_ box point, Point_ point d r
        , Num r
        , OptCVector_ 2 r
        , OptVector_ d r
        , OptVector_ d (ClosedInterval r)
        , HasComponents (Vector d (ClosedInterval r)) (Vector d r)
        ) => box -> Vector d r
size = over components duration . extent


-- instance HasComponents (VectorFamily' d (Interval (EndPoint et r) r))
--                        (VectorFamily' d r) where
--   components =




-- | Given a dimension, get the width of the box in that dimension.
-- Dimensions are 0 indexed.
widthIn :: forall i box d point r. ( Box_ box point, Point_ point d r
                                   , i < d, KnownNat i
                                   , OptCVector_ 2 r
                                   , OptVector_ d (ClosedInterval r)
                                   , OptVector_ d r
                                   , HasComponents (Vector d (ClosedInterval r))
                                                   (Vector d r)
                                   , Num r
                                   ) => box -> r
widthIn = view (component @i) . size


-- | Get the width of a rectangle.
width :: ( Box_ box point, Point_ point d r
         , 0 < d
         , OptCVector_ 2 r
         , OptVector_ d r
         , OptVector_ d (ClosedInterval r)
         , HasComponents (Vector d (ClosedInterval r))
                         (Vector d r)
         , Num r
         ) => box -> r
width  = widthIn @0

-- | get the height of a rectangle
height :: ( Box_ box point, Point_ point d r
         , 1 < d
         , OptCVector_ 2 r
         , OptVector_ d r
         , OptVector_ d (ClosedInterval r)
         , HasComponents (Vector d (ClosedInterval r))
                         (Vector d r)
         , Num r
         ) => box -> r
height = widthIn @1
