{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.HalfSpace.Intersection
  (

    -- HalfPlaneIntersection(..)
   LineHalfPlaneIntersection(..)

  , GetDirection(..)
  ) where

import Control.Lens
import HGeometry.HalfSpace.Type
import HGeometry.HyperPlane
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.HalfLine
import HGeometry.Point
import HGeometry.Slab
import HGeometry.Properties (NumType,Dimension)
import HGeometry.Vector

--------------------------------------------------------------------------------
-- | Intersection between two halfplanes
data HalfPlaneIntersection r orientedLine line =
    HalfPlane_x_HalfPlane_Line      line
  | HalfPlane_x_HalfPlane_Slab      (Slab r orientedLine)
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
      Nothing -> case (pointInteriorTo l `intersects` h', pointInteriorTo l' `intersects` h) of
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
      q = pointInteriorTo l .+^ Vector2 0 1 -- take some offset; this uses that l is not vertial.


      slab = let w = squaredEuclideanDistTo (pointInteriorTo l') l
             in Slab (fromLineEQ l) (if l^.intercept  > l'^.intercept then w else negate w)



--------------------------------------------------------------------------------

data LineHalfPlaneIntersection r line = Line_x_HalfPlane_Line     line
                                      | Line_x_HalfPlane_HalfLine (HalfLine (Point 2 r))
  deriving (Show,Eq)


type instance NumType   (LineHalfPlaneIntersection r line) = r
type instance Dimension (LineHalfPlaneIntersection r line) = 2



instance ( Num r, Ord r
         ) => HasIntersectionWith (LinePV 2 r) (HalfSpaceF (LinePV 2 r)) where
  intersects = intersectsLineHalfplane
instance ( Num r, Ord r
         ) => HasIntersectionWith (LinePV 2 r) (HalfSpaceF (LineEQ r)) where
  intersects = intersectsLineHalfplane
instance ( Num r, Ord r
         ) => HasIntersectionWith (LinePV 2 r) (HalfSpaceF (VerticalOrLineEQ r)) where
  intersects = intersectsLineHalfplane

instance ( Num r, Ord r
         ) => HasIntersectionWith (LineEQ r) (HalfSpaceF (LinePV 2 r)) where
  intersects = intersectsLineHalfplane
instance ( Num r, Ord r
         ) => HasIntersectionWith (LineEQ r) (HalfSpaceF (LineEQ r)) where
  intersects = intersectsLineHalfplane
instance ( Num r, Ord r
         ) => HasIntersectionWith (LineEQ r) (HalfSpaceF (VerticalOrLineEQ r)) where
  intersects = intersectsLineHalfplane

instance ( Num r, Ord r
         ) => HasIntersectionWith (VerticalOrLineEQ r) (HalfSpaceF (LinePV 2 r)) where
  intersects = intersectsLineHalfplane
instance ( Num r, Ord r
         ) => HasIntersectionWith (VerticalOrLineEQ r) (HalfSpaceF (LineEQ r)) where
  intersects = intersectsLineHalfplane
instance ( Num r, Ord r
         ) => HasIntersectionWith (VerticalOrLineEQ r) (HalfSpaceF (VerticalOrLineEQ r)) where
  intersects = intersectsLineHalfplane


-- | test if a line and a halfplane intersect
intersectsLineHalfplane     :: ( HalfPlane_ halfPlane r
                               , Num r, Ord r
                               , HyperPlane_ line 2 r
                               , HasPickInteriorPoint line 2 r
                               , HasIntersectionWith line (BoundingHyperPlane halfPlane 2 r)
                               , HasIntersectionWith (Point 2 r) halfPlane
                               ) => line -> halfPlane -> Bool
intersectsLineHalfplane l h = (l `intersects` (h^.boundingHyperPlane)) ||
                              (pointInteriorTo l `intersects` h)

--------------------------------------------------------------------------------

type instance Intersection (LinePV 2 r) (HalfSpaceF (LinePV 2 r)) =
  Maybe (LineHalfPlaneIntersection r (LinePV 2 r))
type instance Intersection (LinePV 2 r) (HalfSpaceF (LineEQ r)) =
  Maybe (LineHalfPlaneIntersection r (LinePV 2 r))
type instance Intersection (LinePV 2 r) (HalfSpaceF (VerticalOrLineEQ r)) =
  Maybe (LineHalfPlaneIntersection r (LinePV 2 r))

instance ( Fractional r, Ord r
         ) => IsIntersectableWith (LinePV 2 r) (HalfSpaceF (LinePV 2 r)) where
  intersect = intersectLineHalfplane
instance ( Fractional r, Ord r
         ) => IsIntersectableWith (LinePV 2 r) (HalfSpaceF (LineEQ r)) where
  intersect = intersectLineHalfplane
instance ( Fractional r, Ord r
         ) => IsIntersectableWith (LinePV 2 r) (HalfSpaceF (VerticalOrLineEQ r)) where
  intersect = intersectLineHalfplane


type instance Intersection (LineEQ r) (HalfSpaceF (LinePV 2 r)) =
  Maybe (LineHalfPlaneIntersection r (LineEQ r))
type instance Intersection (LineEQ r) (HalfSpaceF (LineEQ r)) =
  Maybe (LineHalfPlaneIntersection r (LineEQ r))
type instance Intersection (LineEQ r) (HalfSpaceF (VerticalOrLineEQ r)) =
  Maybe (LineHalfPlaneIntersection r (LineEQ r))

instance ( Fractional r, Ord r
         ) => IsIntersectableWith (LineEQ r) (HalfSpaceF (LinePV 2 r)) where
  intersect = intersectLineHalfplane
instance ( Fractional r, Ord r
         ) => IsIntersectableWith (LineEQ r) (HalfSpaceF (LineEQ r)) where
  intersect = intersectLineHalfplane
instance ( Fractional r, Ord r
         ) => IsIntersectableWith (LineEQ r) (HalfSpaceF (VerticalOrLineEQ r)) where
  intersect = intersectLineHalfplane



--------------------------------------------------------------------------------

-- | Insertsect a line and a halfplane
intersectLineHalfplane   :: ( HalfPlane_ halfPlane r
                            , Fractional r, Ord r
                            , HyperPlane_ line 2 r
                            , HasPickInteriorPoint line 2 r
                            , HyperPlane_ (BoundingHyperPlane halfPlane 2 r) 2 r
                            , IsIntersectableWith line (BoundingHyperPlane halfPlane 2 r)
                            , GetDirection line
                            , HasIntersectionWith (Point 2 r) halfPlane
                            , Intersection line (BoundingHyperPlane halfPlane 2 r)
                              ~ Maybe (LineLineIntersection line)
                            ) => line -> halfPlane -> Maybe (LineHalfPlaneIntersection r line)
intersectLineHalfplane l h = case l `intersect` (h^.boundingHyperPlane) of
    Nothing
      | pointInteriorTo l `intersects` h -> Just $ Line_x_HalfPlane_Line l
      | otherwise                        -> Nothing
    Just i                               -> Just $ case i of
      Line_x_Line_Line _  -> Line_x_HalfPlane_Line l
      Line_x_Line_Point p -> Line_x_HalfPlane_HalfLine (HalfLine p v)
        where
          v' = inLineVector l
          v = if p .+^ v' `intersects` h then v' else negated v'


--------------------------------------------------------------------------------
-- Helper class to implement line x halfplane intersection

class GetDirection line where
  -- | Get a vector v that lies in the line; i.e. given a point
  -- p that lies on the line; the point p .+^ v lies also in the line.
  inLineVector :: (r ~ NumType line, d ~ Dimension line) => line -> Vector d r

instance HasDirection line => GetDirection line where
  inLineVector = view direction
instance {-# OVERLAPPING #-} Num r => GetDirection (LineEQ r) where
  inLineVector (LineEQ a _) = Vector2 1 a
  -- not sure why it thinks this is overlapping, but whatever
instance Num r => GetDirection (VerticalOrLineEQ r) where
  inLineVector = \case
    VerticalLineThrough _ -> Vector2 0 1
    NonVertical l         -> inLineVector l
