{-# LANGUAGE UndecidableInstances #-}
module ConvexHull.MelkmanSpec where

import           Control.Lens
import qualified HGeometry.ConvexHull.DivideAndConquer as DivideAndConquer
import qualified HGeometry.ConvexHull.Melkman as Melkman
import           HGeometry.Cyclic
import           HGeometry.Instances ()
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

newtype Shift p = Shift p
  deriving newtype (Show)

instance (ShiftedEq p, Eq (ElemCyclic p)) => Eq (Shift p) where
  (Shift a) == (Shift b) = isShiftOf a b

spec = do
  prop "Melkman, convexHull of simple polygon same as convex hull of its vertices" $
    \(pg :: SimplePolygon (Point 2 Rational)) ->
      Shift (Melkman.convexHull pg)
      ===
      Shift (DivideAndConquer.convexHull $ toNonEmptyOf outerBoundary pg)
