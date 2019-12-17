module Data.Geometry.Duality where

import Data.Geometry.Line
import Data.Geometry.Point
import Data.Maybe(fromJust)
--------------------------------------------------------------------------------
-- * Standard Point-Line duality in R^2

-- | Maps a line point (px,py) to a line (y=px*x - py)
dualLine              :: Num r => Point 2 r -> Line 2 r
dualLine (Point2 x y) = fromLinearFunction x (-y)

-- | Returns Nothing if the input line is vertical
-- Maps a line l: y = ax + b to a point (a,-b)
dualPoint   :: (Fractional r, Eq r) => Line 2 r -> Maybe (Point 2 r)
dualPoint l = (\(a,b) -> Point2 a (-b)) <$> toLinearFunction l

-- | Pre: the input line is not vertical
dualPoint' :: (Fractional r, Eq r) => Line 2 r -> Point 2 r
dualPoint' = fromJust . dualPoint
