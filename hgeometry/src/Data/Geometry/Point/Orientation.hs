module Data.Geometry.Point.Orientation where

import Data.Geometry.Point.Class
import Algorithms.Geometry.SoS

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

newtype StrictCCW = SCCW Sign deriving Eq

pattern CCW :: StrictCCW
pattern CCW = SCCW Negative

pattern CW  :: StrictCCW
pattern CW  = SCCW Positive
{-# COMPLETE CCW, CW #-}

instance Show StrictCCW where
  show = \case
    CCW -> "CCW"
    CW  -> "CW"


-- | Given three points p q and r determine the orientation when going from p to r via q.
ccw       :: (Ord r, Num r, AsAPoint point) => point 2 r -> point 2 r -> point 2 r -> StrictCCW
ccw p q r = undefined
