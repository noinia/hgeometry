--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Line.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A class of types that can act as lines in d-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.Line.Class
  ( Line_(..)
  -- , fromPointAndVec
  ) where


import HGeometry.Point.Class
import HGeometry.Vector.Class

--------------------------------------------------------------------------------

-- | Types that can act as lines in d-dimensional space.
class Line_ line d r where
  -- | Constructs a line through a point and a vector
  fromPointAndVec ::  ( Point_ point d r
                      , vector ~ VectorFor point
                      , Line_ line d r
                      , Num r
                      ) => point -> vector -> line
  -- fromPointAndVec p v =



-- type family Line_ line d r where
--   Line_ line 2 r = HyperPlane_ line 2 r


  -- hyperPlaneTrough $ Vector2 p (p .+^ v)

-- direction   :: (Line_ line d r) => line -> vector
-- direction l =

-- perpendicularTo   :: (Line_ line 2 r) => line -> line
-- perpendicularTo l = undefined

-- data LineSideTest = LeftSide | OnLine | RightSide deriving (Show,Eq)
