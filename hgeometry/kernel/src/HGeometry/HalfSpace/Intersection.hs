module HGeometry.HalfSpace.Intersection
  ( HalfPlaneIntersection(..)
  ) where

import Control.Lens
import HGeometry.HalfSpace
import HGeometry.HyperPlane.Class
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.Line.LineEQ
import HGeometry.Line.PointAndVector
import HGeometry.Point
import HGeometry.Properties (NumType,Dimension)
import HGeometry.Vector

--------------------------------------------------------------------------------

data Slab' r orientedLine = Slab { _definingLine        :: orientedLine
                                 , _signedSquaredWidth  :: !r
                                 }
                          deriving stock (Show,Eq,Ord,Functor,Foldable)

type Slab orientedLine = Slab' (NumType orientedLine) orientedLine

-- | Intersection between two halfplanes
data HalfPlaneIntersection r orientedLine line =
    HalfPlane_x_HalfPlane_Line      line
  | HalfPlane_x_HalfPlane_Slab      (Slab' r orientedLine)
  | HalfPlane_x_HalfPlane_Wedge     (Vector 2 r) (Point 2 r) (Vector 2 r)
    -- ^ The first vector points into p, the second one points away from p.
    -- This way, we mean the wedge to the left of both vectors.
  | HalfPlane_x_HalfPlane_HalfPlane (HalfSpaceF line)
  deriving stock (Show,Eq)

type instance Intersection (HalfSpaceF (LineEQ r)) (HalfSpaceF line') =
  Maybe (HalfPlaneIntersection r (LinePV 2 r) (LineEQ r))


instance ( Ord r, Fractional r
         )
       => IsIntersectableWith (HalfSpaceF (LineEQ r)) (HalfSpaceF (LineEQ r)) where
  h@(HalfSpace sign l) `intersect` h'@(HalfSpace sign' l') = case l `intersect` l' of
      Nothing -> case (pointOn l `intersects` h', pointOn l' `intersects` h) of
        (False,False) -> Nothing
        (True,False)  -> Just $ HalfPlane_x_HalfPlane_HalfPlane h
        (False,True)  -> Just $ HalfPlane_x_HalfPlane_HalfPlane h'
        (True,True)   -> Just $ HalfPlane_x_HalfPlane_Slab slab

      Just (Line_x_Line_Point p) -> Just $ HalfPlane_x_HalfPlane_Wedge u p u'
        where
          LinePV _ v  = fromLineEQ l
          LinePV _ v' = fromLineEQ l'

          u  = error "not implemeted yet" -- if (p .+^ Vector2 0 1) `intersects` h  then negated v else v
          u' = error "not implemeted yet" -- if (p .+^ Vector2 0 1) `intersects` h' then v'        else negated v'
          -- FIXME: I don't think this is correct yet!!!


      Just (Line_x_Line_Line _)
          | sameSide  -> Just $ HalfPlane_x_HalfPlane_HalfPlane h
          | otherwise -> Just $ HalfPlane_x_HalfPlane_Line l
    where
      -- we take some point that is not on the bounding line; if the halfplanes both
      -- contain this point they are oriented the same way. Otherwise, they are oriented
      -- in opposite directions, and thus the halfplanes intersect in a line.
      sameSide = q `intersects` h == q `intersects` h'
      q = pointOn l .+^ Vector2 0 1 -- take some offset; this uses that l is not vertial.


      slab = let w = squaredEuclideanDistTo (pointOn l') l
             in Slab (fromLineEQ l) (if l^.intercept  > l'^.intercept then w else negate w)
