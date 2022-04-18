module Geometry.Point.Orientation where

import Data.Indexed
import Data.Sign
import Geometry.Point.Class
import Geometry.SoS.Orientation
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
            , HasIndex point
            )
         => point -> point -> point -> StrictCCW
ccw p q r = SCCW $ sideTest r (Vector2 p q)
