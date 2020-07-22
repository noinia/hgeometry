module Data.Geometry.Point.Orientation where

import Algorithms.Geometry.SoS.Orientation
import Algorithms.Geometry.SoS.Sign
import Data.Ext
import Data.Geometry.Point.Internal
import Data.Geometry.Vector

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
ccw       :: (Ord r, Num r, Ord i)
          => Point 2 r :+ i -> Point 2 r :+ i -> Point 2 r :+ i -> StrictCCW
ccw p q r = SCCW $ sideTest r (Vector2 p q)
