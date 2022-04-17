module Geometry.Point.Orientation where

import Algorithms.Geometry.SoS.Orientation
import Algorithms.Geometry.SoS.Sign
import Algorithms.Geometry.SoS.Index
import Geometry.Point.Class
import Geometry.Vector

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
ccw      :: ( Num r, Ord r
            , ToAPoint point 2 r
            , HasSoSIndex point
            )
         => point -> point -> point -> StrictCCW
ccw p q r = SCCW $ sideTest r (Vector2 p q)
