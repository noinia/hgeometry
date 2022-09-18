module HGeometry.Line.Class
  ( Line_(..)
  -- , fromPointAndVec
  ) where


import HGeometry.Point.Class

--------------------------------------------------------------------------------


class Line_ line d r where
  -- | Constructs a line through a point and a vector
  fromPointAndVec ::  ( Point_ point d r
                      , vector ~ Diff_ point
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
