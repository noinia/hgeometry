{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Triangle
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data types to represent Triangles
--
--------------------------------------------------------------------------------
module HGeometry.Triangle
  ( Triangle(Triangle)
  , module HGeometry.Triangle.Class


  , LineTriangleIntersection(..)
  ) where

import Control.Lens
import Data.Foldable1
import GHC.Generics (Generic)
import HGeometry.Box.Boxable
import HGeometry.Ext
import HGeometry.HalfLine
import HGeometry.Intersection
import HGeometry.Line.PointAndVector
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Transformation
import HGeometry.Triangle.Class
import HGeometry.Vector
import Hiraffe.Graph
import Text.Read

--------------------------------------------------------------------------------

-- | Triangles in d-dimensional space
newtype Triangle point = MkTriangle (Vector 3 point)
  deriving (Generic)
  deriving newtype (Functor,Foldable)

-- | Construct a triangle from its three points
pattern Triangle       :: point -> point -> point -> Triangle point
pattern Triangle a b c = MkTriangle (Vector3 a b c)
{-# COMPLETE Triangle #-}

instance Traversable Triangle where
  traverse f (MkTriangle v) = MkTriangle <$> traverse f v

instance Foldable1 Triangle where
  foldMap1 f (MkTriangle v) = foldMap1 f v

instance Traversable1 Triangle where
  traverse1 f (MkTriangle v) = MkTriangle <$> traverse1 f v


deriving instance Eq  (Vector 3 point) => Eq (Triangle point)
deriving instance Ord (Vector 3 point) => Ord (Triangle point)

type instance Dimension (Triangle point) = Dimension point
type instance NumType   (Triangle point) = NumType point

-- | Iso between a triangle and a vector of three points
_TriangleVector :: Iso (Triangle point) (Triangle point') (Vector 3 point) (Vector 3 point')
_TriangleVector = iso (\(MkTriangle v) -> v) MkTriangle

instance ( Point_ point (Dimension point) (NumType point)
         ) => Triangle_ (Triangle point) point where
  mkTriangle = Triangle
  corners = _TriangleVector

instance HasVertices' (Triangle point) where
  type Vertex   (Triangle point) = point
  type VertexIx (Triangle point) = Int -- make this an AtMostThree?
  vertexAt i = _TriangleVector.iix i
  numVertices = const 3

instance HasVertices (Triangle point) (Triangle point') where
  vertices = cloneIndexedTraversal1 (_TriangleVector.components)

-- data AtMostThree = One | Two | Three
--   deriving (Show,Read,Eq,Ord,Enum,Bounded)

-- instance HasEdges' (Triangle point) where
--   type Edge  (Triangle point)  = (point, point)
--   type EdgeIx (Triangle point) = AtMostThree

-- -- p a (f b) -> s -> f t
--   edgeAt i =
--   (Triangle a b c) = case i of
--     One   ->
--     Two   ->
--     Three ->

--   numEdges = const 3


instance HasPoints (Triangle point) (Triangle point') point point' where
  allPoints = _TriangleVector.components

instance ( DefaultTransformByConstraints (Triangle point) d r
         , Point_ point d r
         ) => IsTransformable (Triangle point)

instance (Show point) => Show (Triangle point) where
  showsPrec k (Triangle a b c ) = showParen (k > appPrec) $
                                    showString "Triangle "
                                    . showsPrec (appPrec+1) a
                                    . showChar ' '
                                    . showsPrec (appPrec+1) b
                                    . showChar ' '
                                    . showsPrec (appPrec+1) c

appPrec :: Int
appPrec = 10

instance (Read point) => Read (Triangle point) where
  readPrec = parens (prec appPrec $ do
                          Ident "Triangle" <- lexP
                          b <- step readPrec
                          a <- step readPrec
                          c <- step readPrec
                          return (Triangle a b c))

instance ( Point_ point d r
         , Ord (Vector d r)
         ) => IsBoxable (Triangle point)

instance ( Point_ point 2 r
         , Num r, Ord r
         ) => HasIntersectionWith (Point 2 r) (Triangle point) where
  q `intersects` t = allOf components (q `intersects`) $ intersectingHalfPlanes t


--------------------------------------------------------------------------------


-- | Data type desciribn the intersection between an oriented line in R^3 and a triangle
data LineTriangleIntersection point seg =
    Line_x_Triangle_Point point
  | Line_x_Triangle_LineSegment seg
  deriving (Show,Eq,Ord,Functor)

instance Bifunctor LineTriangleIntersection where
  bimap f g = \case
    Line_x_Triangle_Point p       -> Line_x_Triangle_Point (f p)
    Line_x_Triangle_LineSegment s -> Line_x_Triangle_LineSegment (g s)

-- | The extra value is the parameter t so that the intersection point
-- is line^.anchor + t*line^.direction
type instance Intersection (LinePV 3 r) (Triangle point') =
    Maybe (LineTriangleIntersection (Point 3 r :+ r) (ClosedLineSegment (Point 3 r :+ r)))

-- | Same here, we also return the parameter at which the ray intersects the point
type instance Intersection (HalfLine point) (Triangle point') =
    Maybe (LineTriangleIntersection
            (Point 3 (NumType point) :+ NumType point)
            (ClosedLineSegment (Point 3 (NumType point) :+ NumType point))
          )

instance ( Point_ point  3 r
         , Fractional r, Ord r
         ) => HasIntersectionWith (LinePV 3 r) (Triangle point)

instance ( Point_ point  3 r
         , Fractional r, Ord r
         ) => IsIntersectableWith (LinePV 3 r) (Triangle point) where
  l `intersect` tri = directedLineTriangleIntersect l (view asPoint <$> tri)

-- | Computes the intersection between an oriented line and a triangle.
-- If the result is an intersection point p; it also returns the parameter t
-- describing the intersection point along the line. I.e. so that p = o + t*v
-- where o is the anchorPoint of the line, and v is its direciton vector.
--
-- see https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
-- for the implementation details.
-- as well as
-- https://www.tandfonline.com/doi/abs/10.1080/10867651.1997.10487468
directedLineTriangleIntersect :: (Ord r, Fractional r)
                              => LinePV 3 r -> Triangle (Point 3 r)
                              -> Maybe (LineTriangleIntersection
                                            (Point 3 r :+ r)
                                            (ClosedLineSegment (Point 3 r :+ r))
                                       )
directedLineTriangleIntersect (LinePV o vec) (Triangle a b c)
    | lineAndTriangleParallel = error "(Half)Line x Triangle colinear not implemented yet"
        -- FIXME might we not intersect in a line segment here?
    | lineIntersectsTriangle  = Just . Line_x_Triangle_Point $ (o .+^ (t *^ vec)) :+ t
    | otherwise               = Nothing
  where
    lineAndTriangleParallel = det == 0

    e1 = b .-. a
    e2 = c .-. a

    rayCrossE2 = vec `cross` e2

    det = e1 `dot` rayCrossE2
    invDet = 1 / det

    s = o .-. a
    u = invDet * (s `dot` rayCrossE2)

    sCrossE1 = s `cross` e1
    v = invDet * (vec `dot` sCrossE1)

    lineIntersectsTriangle = 0 <= u && u       <= 1
                          && 0 <= v && (u + v) <= 1
    -- the u <= 1 is slightly suplurfous, but may speed up the equation.

    -- the parameter along the line at which the line intersects the triangle
    t = invDet * (e2 `dot` sCrossE1)


instance ( Point_ point  3 r
         , Point_ point' 3 r
         , Fractional r, Ord r
         ) => HasIntersectionWith (HalfLine point) (Triangle point')

instance ( Point_ point  3 r
         , Point_ point' 3 r
         , Fractional r, Ord r
         ) => IsIntersectableWith (HalfLine point) (Triangle point') where
  ray `intersect` tri = case directedLineTriangleIntersect (toLine ray) (view asPoint <$> tri) of
      Just (Line_x_Triangle_Point p) | p^.extra >= 0 -> Just $ Line_x_Triangle_Point p
      Just (Line_x_Triangle_LineSegment _)           ->
        error "HalfLine x LineSegment not fully implmeneted yet"
        -- possibly clip the segment here!
      _                                              -> Nothing
    where
      toLine (HalfLine o v) = LinePV (o^.asPoint) v
