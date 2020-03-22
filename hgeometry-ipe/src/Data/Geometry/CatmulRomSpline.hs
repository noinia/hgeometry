module Data.Geometry.CatmulRomSpline where

import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.BezierSpline
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.LSeq (LSeq)
import qualified Data.LSeq as LSeq
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))

import Data.RealNumber.Rational

--------------------------------------------------------------------------------

newtype Spline d r = Spline (LSeq 4 (Point d r))

-- | Converts a CatmulRom Spline into a series of Cubic Bezier curves.
toCubicBezier  :: (Arity d, Fractional r) => Spline d r -> NonEmpty (CubicBezier d r)
toCubicBezier (Spline (toList -> pts)) =
    NonEmpty.fromList $ List.zipWith4 f pts (drop 1 pts) (drop 2 pts) (drop 3 pts)
    -- note that since the spline is guaranteed to contain at least four points, the
    -- drop's are safe, and so is  the conversion into a NonEmpty list.
  where
    f p1 p2 p3 p4 =
      CubicBezier p2 (p2 .+^ ((p1 .-. p3) ^/ (6*tau))) (p3 .-^ ((p4 .-. p2) ^/ (6*tau))) p3

    -- the tension
    tau = 1 / 2
  -- using: https://pomax.github.io/bezierinfo/#catmullconv


type R = RealNumber 10

test :: Spline 2 R
test = Spline . LSeq.promise . LSeq.fromList $ [origin, Point2 10 10, Point2 10 0, Point2 20 1]
