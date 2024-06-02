--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Line.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A class for types that can act as lines in d-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.Line.Class
  ( Line_(..)
  , Line2_
  , lineThrough
  , verticalLine, horizontalLine
  , HasOnLine(..)
  ) where

import HGeometry.HyperPlane.Class
import HGeometry.Point.Class
import HGeometry.Point.PointF
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------

-- $setup
-- >>> import HGeometry.Point
-- >>> import HGeometry.Line

-- | Types that can act as lines in d-dimensional space.
class Line_ line d r | line -> d
                     , line -> r where
  -- | Constructs a line through a point and a vector
  fromPointAndVec ::  ( Point_ point d r
                      , Line_ line d r
                      , Num r
                      ) => point -> Vector d r -> line

-- | Construct a line through two points.
--
-- >>> lineThrough origin (Point2 4 5) :: LinePV 2 Int
-- LinePV (Point2 0 0) (Vector2 4 5)
lineThrough     :: forall line point d r.
                   ( Line_ line d r
                   , Point_ point d r
                   , Num r
                   ) => point -> point -> line
lineThrough p q = fromPointAndVec p (q .-. p)


-- | Synonym for a line-like class.
type Line2_ line r = (Line_ line 2 r, HyperPlane_ line 2 r)



-- | Vertical line with a given X-coordinate.
verticalLine   :: forall r line.
                  ( Line_ line 2 r, Num r)
               => r -> line
verticalLine x = fromPointAndVec (Point $ Vector2 x 0) (Vector2 0 1)

-- | Horizontal line with a given Y-coordinate.
horizontalLine   :: forall r line. (Line_ line 2 r, Num r)
                 => r -> line
horizontalLine y = fromPointAndVec (Point $ Vector2 0 y) (Vector2 1 0)


-- | Types that have an 'onLine' function that allows testing if a point lies on a line.
class HasOnLine line d | line -> d where
  -- | Test if the point lies on the line
  --
  -- >>> Point2 0 0 `onLine` lineThrough @(LinePV 2 Double) origin (Point2 1 0)
  -- True
  -- >>> Point2 10 10 `onLine` lineThrough @(LinePV 2 Double) origin (Point2 2 2)
  -- True
  -- >>> Point2 10 5 `onLine` lineThrough @(LinePV 2 Double) origin (Point2 2 2)
  -- False
  onLine :: ( Point_ point d r, Num r, Eq r
            , r ~ NumType line, d ~ Dimension line
            ) => point -> line -> Bool



--}

-- type family Line_ line d r where
--   Line_ line 2 r = HyperPlane_ line 2 r


  -- hyperPlaneTrough $ Vector2 p (p .+^ v)

-- direction   :: (Line_ line d r) => line -> vector
-- direction l =

-- perpendicularTo   :: (Line_ line 2 r) => line -> line
-- perpendicularTo l = undefined

-- data LineSideTest = LeftSide | OnLine | RightSide deriving (Show,Eq)
