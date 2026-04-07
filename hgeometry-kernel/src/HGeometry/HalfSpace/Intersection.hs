{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.HalfSpace.Intersection
  ( HalfPlaneIntersection(..)
  , LineHalfPlaneIntersection(..)

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
import HGeometry.Cone
import HGeometry.Properties (NumType,Dimension)
import HGeometry.Vector
import HGeometry.Ext

--------------------------------------------------------------------------------

-- | The non-empty intersection of two halfPlanes
data HalfPlaneIntersection r halfPlane =
    HalfPlane_x_HalfPlane_Line (BoundingHyperPlane halfPlane 2 r)
  | HalfPlane_x_HalfPlane_Slab (Slab r (LinePV 2 r :+ halfPlane))
  | HalfPlane_x_HalfPlane_Cone (Cone r (Point 2 r) halfPlane)
  | HalfPlane_x_HalfPlane_HalfPlane halfPlane

deriving instance (Show r, Show halfPlane, Show (BoundingHyperPlane halfPlane 2 r)
                  ) => Show (HalfPlaneIntersection r halfPlane)
deriving instance (Ord r, Num r, Eq halfPlane, Eq (BoundingHyperPlane halfPlane 2 r)
                  ) => Eq (HalfPlaneIntersection r halfPlane)

type instance NumType   (HalfPlaneIntersection r halfPlane) = r
type instance Dimension (HalfPlaneIntersection r halfPlane) = 2



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
