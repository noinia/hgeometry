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
  | HalfPlane_x_HalfPlane_Slab (Slab r halfPlane)
  | HalfPlane_x_HalfPlane_Cone (Cone r (Point 2 r) halfPlane)
  | HalfPlane_x_HalfPlane_HalfPlane halfPlane

deriving instance (Show r, Show halfPlane, Show (BoundingHyperPlane halfPlane 2 r)
                  ) => Show (HalfPlaneIntersection r halfPlane)
deriving instance (Ord r, Num r, Eq halfPlane, Eq (BoundingHyperPlane halfPlane 2 r)
                  ) => Eq (HalfPlaneIntersection r halfPlane)

type instance NumType   (HalfPlaneIntersection r halfPlane) = r
type instance Dimension (HalfPlaneIntersection r halfPlane) = 2


type instance Intersection (HalfSpaceF (LinePV 2 r)) (HalfSpaceF (LinePV 2 r)) =
  Maybe (HalfPlaneIntersection r (HalfSpaceF (LinePV 2 r)))

instance ( Fractional r, Ord r
         ) => IsIntersectableWith (HalfSpaceF (LinePV 2 r)) (HalfSpaceF (LinePV 2 r)) where
  intersect = intersectTwo

type instance Intersection (HalfSpaceF (LineEQ r)) (HalfSpaceF (LineEQ r)) =
  Maybe (HalfPlaneIntersection r (HalfSpaceF (LineEQ r)))

instance ( Fractional r, Ord r
         ) => IsIntersectableWith (HalfSpaceF (LineEQ r)) (HalfSpaceF (LineEQ r)) where
  intersect = intersectTwo

-- | move to LinePV or so
instance (Num r) => HasSupportingLine (LineEQ r) where
  supportingLine = fromLineEQ

--------------------------------------------------------------------------------


-- | Computes the intersection of two halfplanes
intersectTwo :: forall halfPlane r.
                ( HalfPlane_ halfPlane r
                , Fractional r, Ord r
                , HyperPlane_ (BoundingHyperPlane halfPlane 2 r) 2 r
                , HasIntersectionWith (Point 2 r) halfPlane
                , HasPickInteriorPoint (BoundingHyperPlane halfPlane 2 r) 2 r
                , GetDirection (BoundingHyperPlane halfPlane 2 r)
                , IsIntersectableWith (BoundingHyperPlane halfPlane 2 r) (BoundingHyperPlane halfPlane 2 r)
                , HasSupportingLine (BoundingHyperPlane halfPlane 2 r)
                , Intersection (BoundingHyperPlane halfPlane 2 r)
                  (BoundingHyperPlane halfPlane 2 r)
                  ~ Maybe (LineLineIntersection (BoundingHyperPlane halfPlane 2 r))
                ) => halfPlane -> halfPlane -> Maybe (HalfPlaneIntersection r halfPlane)
intersectTwo h1 h2 = case l1 `intersect` l2 of
    Nothing -> case (pointInteriorTo l1 `intersects` h2, pointInteriorTo l2 `intersects` h1) of
      (False,False) -> Nothing
      (True, False) -> Just $ HalfPlane_x_HalfPlane_HalfPlane h1
      (False, True) -> Just $ HalfPlane_x_HalfPlane_HalfPlane h2
      (True,True)   -> Just $ HalfPlane_x_HalfPlane_Slab (fromParalelHalfplanes h1 h2)
    Just i  -> Just $ case i of
      Line_x_Line_Line l  -> let n = normalVector l
                                 q = pointInteriorTo l .+^ n
                             in if q `intersects` h1 == q `intersects` h2
                                then HalfPlane_x_HalfPlane_HalfPlane h1 -- same halfplane
                                else HalfPlane_x_HalfPlane_Line l -- oppositive halfplanes
      Line_x_Line_Point a -> HalfPlane_x_HalfPlane_Cone $ Cone a left right
        where
          v1 = inLineVector l1
          v2 = inLineVector l2
          (left,right)
            | isLeftHalfPlane v1 h1 = case ccw (origin :: Point 2 r) (Point v1) (Point v2) of
                CCW | isLeftHalfPlane v2 h2 -> (negated v1 :+ h1, v2 :+ h2)
                    | otherwise             -> (v2 :+ h2,         v1 :+ h1)
                CW  | isLeftHalfPlane v2 h2 -> (negated v2 :+ h2, v1 :+ h1)
                    | otherwise             -> (negated v1 :+ h1, negated v2 :+ h2)
                CoLinear -> error "absurd"
            | otherwise            = case ccw (origin :: Point 2 r) (Point v1) (Point v2) of
                CCW | isLeftHalfPlane v2 h2 -> (negated v2 :+ h2, negated v1 :+ h1)
                    | otherwise             -> (v1 :+ h1,         negated v2 :+ h2)
                CW  | isLeftHalfPlane v2 h2 -> (v1 :+ h1,         v2 :+ h2)
                    | otherwise             -> (v2 :+ h2,         negated v1 :+ h1)
                CoLinear -> error "absurd"

          isLeftHalfPlane (Vector2 x y) h = let w = Vector2 (-y) x
                                                -- perpendicular to v; pointing left
                                            in (a .+^ w) `intersects` h
  where
    l1 = h1^.boundingHyperPlane
    l2 = h2^.boundingHyperPlane

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
