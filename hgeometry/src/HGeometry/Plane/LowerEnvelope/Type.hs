{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Plane.LowerEnvelope.Type
  ( LowerEnvelope(..)
  , pointLocateParallel
  ) where

import           Control.Lens
import qualified Data.Vector as Vector
import           HGeometry.Algorithms.BinarySearch
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line.General
import           HGeometry.Plane.LowerEnvelope.Connected
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Sequence.Alternating (Alternating(..))


--------------------------------------------------------------------------------
-- * Data type defining a lower envelope

-- | The lower enevelope of planes in R^3. (Or rather, its minimization diagram)
data LowerEnvelope plane =
    ParallelStrips    !(Alternating Vector.Vector (VerticalOrLineEQ (NumType plane)) plane)
  | ConnectedEnvelope !(MinimizationDiagram (NumType plane) (Point 2 (NumType plane)) plane)

deriving instance (Show plane, Show (NumType plane)) => Show (LowerEnvelope plane)
deriving instance (Eq plane, Eq (NumType plane))     => Eq (LowerEnvelope plane)


--------------------------------------------------------------------------------

-- | Point locates the given point among the parralel strips.
--
-- \(O(\log h)\), where \(h\) is the number of planes on the lower envelope
pointLocateParallel      :: (Plane_ plane r, Point_ point 2 r, Ord r, Num r)
                         => point
                         -> Alternating Vector.Vector (VerticalOrLineEQ r) plane
                         -> plane
pointLocateParallel q (Alternating h0 hs) = case binarySearchIn (q `liesRightOf`) hs of
                                              AllTrue (_,h)   -> h
                                              FlipsAt _ (_,h) -> h
                                              AllFalse _      -> h0
  where
    q' `liesRightOf` (sep, _)  = case sep of
                                   VerticalLineThrough x -> q'^.xCoord > x
                                   NonVertical l         -> verticalSideTest q l == GT
