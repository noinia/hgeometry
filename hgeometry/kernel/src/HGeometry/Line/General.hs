--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Line.General
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- An algebraic data type representing a line; we explicitly model vertical lines.
--
--------------------------------------------------------------------------------
module HGeometry.Line.General
  ( VerticalOrLineEQ(..)
  ) where

import Control.DeepSeq
import Control.Lens ((^.), coerced)
import GHC.Generics
import HGeometry.HyperPlane
import HGeometry.HyperPlane.NonVertical
import HGeometry.Intersection
import HGeometry.Line.Class
import HGeometry.Line.Intersection
import HGeometry.Line.LineEQ
import HGeometry.Line.NonVertical.Class
import HGeometry.Point
import HGeometry.Properties (NumType, Dimension)
import HGeometry.Vector
import Text.Read

--------------------------------------------------------------------------------

-- | A line type that explicitly represents vertical lines.
data VerticalOrLineEQ r = VerticalLineThrough !r
                        | NonVertical !(LineEQ r)
                        deriving (Show,Eq,Ord,Generic)

type instance NumType   (VerticalOrLineEQ r) = r
type instance Dimension (VerticalOrLineEQ r) = 2

instance NFData r => NFData (VerticalOrLineEQ r)

-- fromGeneralLine :: (Line2_ line r, Num r) => line -> VerticalOrLineEQ r
-- fromGeneralLine

instance (Fractional r, Eq r) =>  Line_ (VerticalOrLineEQ r) 2 r where
  fromPointAndVec (Point2_ px py) (Vector2 vx vy)
    | vx == 0  = VerticalLineThrough px
    | otherwise = let a = vy/vx
                  in NonVertical $ LineEQ a (py-px*a)
    -- in principle we get py = a*px + b, with a = (vy/vx), and thus b=py - px*(vy/vx)

instance HasOnLine (VerticalOrLineEQ r) 2 where
  onLine q = \case
    VerticalLineThrough x -> q^.xCoord == x
    NonVertical l         -> onLine q l

instance HyperPlane_ (VerticalOrLineEQ r) 2 r where
  hyperPlaneEquation = \case
    VerticalLineThrough x -> Vector3 1 0 (-x)
    NonVertical l         -> hyperPlaneEquation l
  onHyperPlane = onLine
  onSideTest q = \case
    VerticalLineThrough x -> (q^.xCoord) `compare` x
    NonVertical l         -> onSideTest q l

instance (Fractional r, Eq r) => ConstructableHyperPlane_ (VerticalOrLineEQ r) 2 r where
  type HyperPlaneFromEquationConstraint (VerticalOrLineEQ r) 2 r = ()
  hyperPlaneFromEquation (Vector3 c a b)
    | b == 0   = VerticalLineThrough (-c)
    | otherwise = NonVertical undefined
  -- | The
  fromPointAndNormal p (Vector2 vx vy) = fromPointAndVec p $ Vector2 vy (-vx)
    -- rotate the normal vector 90 degrees clockwise
