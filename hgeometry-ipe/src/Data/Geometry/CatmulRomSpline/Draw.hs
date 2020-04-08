module Data.Geometry.CatmulRomSpline.Draw where

import           Data.Ext
import           Data.Geometry.CatmulRomSpline
import           Data.Geometry.Ipe.IpeOut
import           Data.Geometry.Ipe.Types
import qualified Data.LSeq as LSeq

--------------------------------------------------------------------------------

-- | Draws a CatmulRom spline as a Path in Ipe.
draw :: Fractional r => IpeOut (Spline 2 r) Path r
draw = (:+ mempty) . Path . LSeq.fromNonEmpty . fmap CubicBezierSegment . toCubicBezier
