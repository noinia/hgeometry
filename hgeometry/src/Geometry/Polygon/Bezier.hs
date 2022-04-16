{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.Polygon.Bezier
  ( PathJoin(..)
  , fromBeziers
  , approximate
  , approximateSome
  ) where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.BezierSpline (BezierSpline, pattern Bezier3)
import qualified Data.Geometry.BezierSpline as Bezier
import           Data.Geometry.Point
import           Data.Geometry.PolyLine(points)
import           Data.Geometry.Polygon
import qualified Data.Vector.Circular       as CV
import qualified Data.Foldable as F

data PathJoin r
  = JoinLine
  | JoinCurve (Point 2 r) (Point 2 r)
  deriving (Show, Eq, Ord)

-- | Construct a polygon from a closed set of bezier curves. Each curve must be connected to
--   its neighbours.
fromBeziers :: (Eq r, Num r) => [BezierSpline 3 2 r] -> SimplePolygon (PathJoin r) r
fromBeziers curves
  | isCounterClockwise expanded = p
  | otherwise = p'
  where
    p = unsafeFromPoints
      [ a :+ JoinCurve b c
      | Bezier3 a b c _d <- curves ]
    p' = unsafeFromPoints
      [ d :+ JoinCurve c b
      | Bezier3 _a b c d <- reverse curves ]
    expanded = unsafeFromPoints $ concat
      [ map ext [a, b, c]
      | Bezier3 a b c _d <- curves ]

approximate :: forall t r. (Ord r, Fractional r) => r -> Polygon t (PathJoin r) r -> Polygon t () r
approximate eps p =
  case p of
    SimplePolygon{}  ->
      let vs = p^.outerBoundaryVector
      in unsafeFromCircularVector $ CV.concatMap f $ CV.zip vs (CV.rotateRight 1 vs)
    MultiPolygon v hs -> MultiPolygon (approximate eps v) (map (approximate eps) hs)
  where
    f :: (Point 2 r :+ PathJoin r, Point 2 r :+ PathJoin r) -> CV.CircularVector (Point 2 r :+ ())
    f (a :+ JoinLine, _) = CV.singleton (ext a)
    f (a :+ JoinCurve b c, d :+ _) = let poly = Bezier.approximate eps (Bezier3 a b c d)
                                     in CV.unsafeFromList . init . F.toList $ poly^.points

approximateSome :: (Ord r, Fractional r) => r -> SomePolygon (PathJoin r) r -> SomePolygon () r
approximateSome eps (Left p)  = Left $ approximate eps p
approximateSome eps (Right p) = Right $ approximate eps p
