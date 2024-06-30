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
import Control.Lens ((^.))
import GHC.Generics
import HGeometry.Ext
import HGeometry.HyperPlane
import HGeometry.Intersection
import HGeometry.Line.Class
import HGeometry.Line.Intersection
import HGeometry.Line.LineEQ
import HGeometry.Point
import HGeometry.Properties (NumType, Dimension)
import HGeometry.Vector
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
  hyperPlaneFromEquation v@(Vector3 a b c)
    | b == 0    = VerticalLineThrough ((-c)/a)
    | otherwise = NonVertical $ hyperPlaneFromEquation v


  fromPointAndNormal p (Vector2 vx vy) = fromPointAndVec p $ Vector2 vy (-vx)
    -- rotate the normal vector 90 degrees clockwise

type instance Intersection (LineEQ r) (VerticalOrLineEQ r) =
  Maybe (LineLineIntersection (LineEQ r))

instance Eq r => HasIntersectionWith (LineEQ r) (VerticalOrLineEQ r) where
  m `intersects` l = case l of
    VerticalLineThrough _ -> True
    NonVertical l'        -> m `intersects` l'

instance (Eq r, Fractional r)
         => IsIntersectableWith (LineEQ r) (VerticalOrLineEQ r) where
  m `intersect` l = case l of
    VerticalLineThrough x -> Just . Line_x_Line_Point $ Point2 x (evalAt' x m)
    NonVertical l'        -> m `intersect` l'

type instance Intersection (VerticalOrLineEQ r) (VerticalOrLineEQ r) =
  Maybe (LineLineIntersection  (VerticalOrLineEQ r))

instance Eq r => HasIntersectionWith (VerticalOrLineEQ r) (VerticalOrLineEQ r) where
  m `intersects` l = case m of
    VerticalLineThrough x -> case l of
                               VerticalLineThrough x' -> x == x'
                               NonVertical _          -> True
    NonVertical m'        -> case l of
                               VerticalLineThrough _  -> True
                               NonVertical l'         -> m' `intersects` l'

instance (Eq r, Fractional r)
         => IsIntersectableWith (VerticalOrLineEQ r) (VerticalOrLineEQ r) where
  m `intersect` l = case m of
    VerticalLineThrough x -> case l of
        VerticalLineThrough x'
          | x == x'            -> Just $ Line_x_Line_Line m
          | otherwise          -> Nothing
        NonVertical l'         -> Just . Line_x_Line_Point $ Point2 x (evalAt' x l')
    NonVertical m'        -> case l of
        VerticalLineThrough x' -> Just . Line_x_Line_Point $ Point2 x' (evalAt' x' m')
        NonVertical l'         -> fmap NonVertical <$> m' `intersect` l'


type instance Intersection (VerticalOrLineEQ r :+ extra) (VerticalOrLineEQ r :+ extra') =
  Maybe (LineLineIntersection  (VerticalOrLineEQ r :+ extra))


instance Eq r => HasIntersectionWith (VerticalOrLineEQ r :+ extra)
                                     (VerticalOrLineEQ r :+ extra') where
  m `intersects` l = (m^.core) `intersects` (l^.core)

instance (Eq r, Fractional r)
         => IsIntersectableWith (VerticalOrLineEQ r :+ extra) (VerticalOrLineEQ r :+ extra') where
  m `intersect` l = let ix = (m^.core) `intersect` (l^.core)
                    in fmap (const m) <$> ix -- if it is a line, just replace it by m
