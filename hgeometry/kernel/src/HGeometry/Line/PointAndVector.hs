{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Line.PointAndLine
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional lines.
--
--------------------------------------------------------------------------------
module HGeometry.Line.PointAndVector
  ( LinePV(..)
  , isIdenticalTo
  , HasSupportingLine(..)
  , fromLinearFunction
  , toLinearFunction
  , SideTestUpDown(..), OnSideUpDownTest(..)
  , liesAbove, liesBelow
  , SideTest(..), onSide

  , bisector
  , perpendicularTo, isPerpendicularTo

  , cmpSlope
  ) where

import           Control.DeepSeq
import           Control.Lens
import qualified Data.Foldable as F
import           Data.Ord (comparing)
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line.Class
import           HGeometry.Line.Intersection
import           HGeometry.Point
-- import           HGeometry.Point.EuclideanDistance
-- import           HGeometry.Point.Orientation.Degenerate
import           HGeometry.Properties (NumType, Dimension)
import           HGeometry.Vector
import           Text.Read

--------------------------------------------------------------------------------
-- * d-dimensional Lines

-- | A line is given by an anchor point and a vector indicating the
-- direction.
data LinePV d r = LinePV { _anchorPoint :: !(Point  d r)
                         , _direction   :: !(Vector d r)
                         } deriving Generic

type instance Dimension (LinePV d r) = d
type instance NumType   (LinePV d r) = r

instance ( Has_ Metric_ d r ) => Line_ (LinePV d r) d r where
  fromPointAndVec p v = LinePV (p^.asPoint) (v^._Vector)


instance ( Eq r, Num r
         ) => ConstructableHyperPlane_ (LinePV 2 r) 2 r where

  type HyperPlaneFromEquationConstraint (LinePV 2 r) 2 r = Fractional r

  -- equation: line equation is: c + ax + by = 0
  -- pre: not all of a b and c are zero
  hyperPlaneFromEquation (Vector3 c a b)
    | b == 0    = LinePV (Point2 (-c/a) 0)      (Vector2 0 1) -- if b=0 we are vertical
    | otherwise = LinePV (Point2 0      (-c/b)) (Vector2 c (-a))

  fromPointAndNormal p (Vector2 vx vy) = LinePV (p^.asPoint) (Vector2 (-vy) vx)


instance Num r => HyperPlaneFromPoints (LinePV 2 r) where
  hyperPlaneThrough (Vector2 p q) = LinePV (p^.asPoint) (q .-. p)


instance ( Eq r, Num r
         ) => HyperPlane_ (LinePV 2 r) 2 r where
  hyperPlaneEquation (LinePV (Point2 px py) (Vector2 vx vy)) = Vector3 a0 vx vy
    where
      a0 = if vx == 0 then -px else -vx*px - vy*py



{- HLINT ignore toLinearFunction -}
-- | get values a,b s.t. the input line is described by y = ax + b.
-- returns Nothing if the line is vertical
toLinearFunction                               :: forall r.
                                                  ( Fractional r, Ord r
                                                  )
                                               => LinePV 2 r -> Maybe (r,r)
toLinearFunction l@(LinePV _ ~(Vector2 vx vy)) =
  case l `intersect` verticalLine @r @(LinePV 2 r) 0 of
  Nothing                               -> Nothing -- l is vertical
  Just (Line_x_Line_Point (Point2 _ b)) -> Just (vy / vx,b)
  Just (Line_x_Line_Line _)             -> Nothing -- l is a vertical line (through x=0)


instance ( Show r, KnownNat d
         , Has_ Additive_ d r
         -- , OptVector_ d r, OptMetric_ d r
         ) => Show (LinePV d r) where
  showsPrec k (LinePV p v) = showParen (k > appPrec) $
                             showString "LinePV "
                             . showsPrec (appPrec+1) p
                             . showChar ' '
                             . showsPrec (appPrec+1) v

appPrec :: Int
appPrec = 10

instance (Read r
         , Has_ Additive_ d r
         -- , OptVector_ d r, OptMetric_ d r
         , KnownNat d
         ) => Read (LinePV d r) where
  readPrec = parens (prec appPrec $ do
                          Ident "LinePV" <- lexP
                          p <- step readPrec
                          v <- step readPrec
                          return (LinePV p v))

instance (NFData (Vector d r)) => NFData (LinePV d r)

-- deriving instance Arity d             => Functor       (Line d)
-- deriving instance Arity d             => F.Foldable    (Line d)
-- deriving instance Arity d             => T.Traversable (Line d)

-- deriving instance (Arity d, Eq r)     => Eq (LinePV d r)


instance {-# OVERLAPPING #-} (Ord r, Num r) => Eq (LinePV 2 r) where
  l@(LinePV p _) == l' = l `isParallelTo2` l' && p `onLine2` l'

-- instance (Eq r, Fractional r) => Eq (LinePV d r) where
--  l@(LinePV p _) == m = l `isParallelTo` m && p `onLine` m

instance (Has_ Metric_ d r, Fractional r, Eq (Vector d r)) => HasOnLine (LinePV d r) d where
  onLine = onLineD

-- | Test if point q lies on line l
onLineD :: ( Point_ point d r, Has_ Metric_ d r
           , Fractional r, Eq r, Eq (Vector d r)
           ) => point -> LinePV d r -> Bool
onLineD q (LinePV p v) = let q' = q^.asPoint
                        in p == q' || (q' .-. p) `isScalarMultipleOf` v


-- instance (Arbitrary r, Arity d, Num r, Eq r) => Arbitrary (LinePV d r) where
--   arbitrary = do p <- arbitrary @(Point d r)
--                  q <- suchThat arbitrary (/= p)
--                  return $ lineThrough p q


-- ** Functions on lines



-- | Test if two lines are identical, meaning; if they have exactly the same
-- anchor point and directional vector.
isIdenticalTo                             :: Eq (Vector d r)
                                          => LinePV d r -> LinePV d r -> Bool
(LinePV p u) `isIdenticalTo` (LinePV q v) = (p,u) == (q,v)


-- {-# RULES
-- "isParallelTo/isParallelTo2" [3]
--      forall (l1 :: forall r. LinePV 2 r) l2. isParallelTo l1 l2 = isParallelTo2 l1 l2
-- #-}

-- | Check whether two lines are parallel
isParallelTo2                            :: (Eq r, Num r) => LinePV 2 r -> LinePV 2 r -> Bool
isParallelTo2 (LinePV _ (Vector2 ux uy))
              (LinePV _ (Vector2 vx vy)) = denom == 0
    where
      denom = vy * ux - vx * uy

instance {-# OVERLAPPING #-} Ord r => HasOnLine (LinePV 2 r) 2 where
  onLine = onLine2

-- | Specific 2d version of testing if apoint lies on a line.
onLine2                  :: (Ord r, Num r, Point_ point 2 r) => point -> LinePV 2 r -> Bool
q `onLine2` (LinePV p v) = ccw (q^.asPoint) p (p .+^ v) == CoLinear

-- | The intersection of two lines is either: NoIntersection, a point or a line.
type instance Intersection (LinePV 2 r) (LinePV 2 r) =
  Maybe (LineLineIntersection (LinePV 2 r))

instance ( Ord r, Num r
         ) => LinePV 2 r `HasIntersectionWith` LinePV 2 r where
  l1 `intersects` l2@(LinePV q _) =
    not (l1 `isParallelTo2` l2) || q `onLine2` l1


instance ( Ord r
         , Fractional r
         -- , OptCVector_ 2 r, OptCVector_ 3 r
         -- , OptMetric_ 2 r, Eq (VectorFamily 2 r)
         ) => LinePV 2 r `IsIntersectableWith` LinePV 2 r where
  l@(LinePV p ~(Vector2 ux uy)) `intersect` (LinePV q ~v@(Vector2 vx vy))
      | areParallel = if q `onLine2` l then Just $ Line_x_Line_Line l
                                       else Nothing
      | otherwise   = Just $ Line_x_Line_Point r
    where
      r = q .+^ (alpha *^ v)

      denom       = vy * ux - vx * uy
      areParallel = denom == 0
      -- Instead of using areParallel, we can also use the generic 'isParallelTo' function
      -- for lines of arbitrary dimension, but this is a bit more efficient.

      alpha        = (ux * (py - qy) + uy * (qx - px)) / denom

      Point2 px py = p
      Point2 qx qy = q

--------------------------------------------------------------------------------
-- * Supporting Lines

-- | Types for which we can compute a supporting line, i.e. a line
-- that contains the thing of type t.
class HasSupportingLine t where
  supportingLine :: t -> LinePV (Dimension t) (NumType t)

instance HasSupportingLine (LinePV d r) where
  supportingLine = id

--------------------------------------------------------------------------------
-- * Convenience functions on Two dimensional lines

-- | Create a line from the linear function ax + b
fromLinearFunction     :: (Num r) => r -> r -> LinePV 2 r
fromLinearFunction a b = LinePV (Point2 0 b) (Vector2 1 a)

instance (Fractional r, Has_ Metric_ d r
         ) => HasSquaredEuclideanDistance (LinePV d r) where
  pointClosestTo (view asPoint -> p) (LinePV a m) = a .+^ (t0 *^ m)
    where
      -- see https://monkeyproofsolutions.nl/wordpress/how-to-calculate-the-shortest-distance-between-a-point-and-a-line/
      t0 = numerator / divisor
      numerator = (p .-. a) `dot` m
      divisor  = m `dot` m


-- | Result of a side test
data SideTestUpDown = Below | On | Above deriving (Show,Read,Eq,Ord)

-- | Class for types that support testing on which side (above, on,
-- below) a particular point is.
class OnSideUpDownTest t where
  -- | Given a point q and a hyperplane h, compute to which side of h q lies. For
  -- vertical hyperplanes the left side of the hyperplane is interpeted as below.
  onSideUpDown :: (d ~ Dimension t, r ~ NumType t, Ord r, Num r, Point_ point d r)
               => point -> t -> SideTestUpDown

instance OnSideUpDownTest (LinePV 2 r) where
  -- | Given a point q and a line l, compute to which side of l q lies. For
  -- vertical lines the left side of the line is interpeted as below.
  --
  -- >>> Point2 10 10 `onSideUpDown` (lineThrough origin $ Point2 10 5)
  -- Above
  -- >>> Point2 10 10 `onSideUpDown` (lineThrough origin $ Point2 (-10) 5)
  -- Above
  -- >>> Point2 5 5 `onSideUpDown` (verticalLine 10)
  -- Below
  -- >>> Point2 5 5 `onSideUpDown` (lineThrough origin $ Point2 (-3) (-3))
  -- On
  q `onSideUpDown` (LinePV p v) = let r    =  p .+^ v
                                      f z         = (z^.xCoord, -z^.yCoord)
                                      minBy g a b = F.minimumBy (comparing g) [a,b]
                                      maxBy g a b = F.maximumBy (comparing g) [a,b]
                                  in case ccw (minBy f p r) (maxBy f p r) (q^.asPoint) of
                                       CCW      -> Above
                                       CW       -> Below
                                       CoLinear -> On

-- | Result of a side test
data SideTest = LeftSide | OnLine | RightSide deriving (Show,Read,Eq,Ord)

-- | Given a point q and a line l, compute to which side of l q lies. For
-- vertical lines the left side of the line is interpeted as below.
--
-- >>> Point2 10 10 `onSide` (lineThrough origin $ Point2 10 5)
-- LeftSide
-- >>> Point2 10 10 `onSide` (lineThrough origin $ Point2 (-10) 5)
-- RightSide
-- >>> Point2 5 5 `onSide` (verticalLine 10)
-- LeftSide
-- >>> Point2 5 5 `onSide` (lineThrough origin $ Point2 (-3) (-3))
-- OnLine
onSide                :: ( Ord r, Num r
                         , Point_ point 2 r
                         -- , OptCVector_ 2 r, OptMetric_ 2 r
                         ) => point -> LinePV 2 r -> SideTest
q `onSide` (LinePV p v) = let r    =  p .+^ v
                            -- f z         = (z^.xCoord, -z^.yCoord)
                            -- minBy g a b = F.minimumBy (comparing g) [a,b]
                            -- maxBy g a b = F.maximumBy (comparing g) [a,b]
                          in case ccw p r (q^.asPoint) of
                            CCW      -> LeftSide
                            CW       -> RightSide
                            CoLinear -> OnLine

-- | Test if the query point q lies (strictly) above line l
liesAbove       :: ( Ord r, Num r
                   , Point_ point 2 r
                   -- , OptCVector_ 2 r, OptMetric_ 2 r
                   ) => point -> LinePV 2 r -> Bool
q `liesAbove` l = q `onSideUpDown` l == Above

-- | Test if the query point q lies (strictly) above line l
liesBelow      :: (Ord r, Num r, Point_ point 2 r
                  -- , OptCVector_ 2 r, OptMetric_ 2 r
                  ) => point -> LinePV 2 r -> Bool
q `liesBelow` l = q `onSideUpDown` l == Below

-- | Get the bisector between two points
bisector     :: (Fractional r, Point_ point 2 r
                -- , OptCVector_ 2 r, OptMetric_ 2 r
                ) => point -> point -> LinePV 2 r
bisector p q = let v = q .-. p
                   h = view asPoint $ p .+^ (v ^/ 2)
               in perpendicularTo (LinePV h v)

-- | Given a line l with anchor point p and vector v, get the line
-- perpendicular to l that also goes through p. The resulting line m is
-- oriented such that v points into the left halfplane of m.
--
-- >>> perpendicularTo $ LinePV (Point2 3 4) (Vector2 (-1) 2)
-- LinePV (Point2 3 4) (Vector2 (-2) (-1))
perpendicularTo                             :: Num r => LinePV 2 r -> LinePV 2 r
perpendicularTo (LinePV p ~(Vector2 vx vy)) = LinePV p (Vector2 (-vy) vx)

-- | Test if a vector is perpendicular to the line.
isPerpendicularTo :: (Num r, Eq r
                     ) => Vector 2 r -> LinePV 2 r -> Bool
v `isPerpendicularTo` (LinePV _ u) = v `dot` u == 0


-- | Compares the lines on slope. Vertical lines are considered larger than
-- anything else.
--
-- >>> (LinePV origin (Vector2 5 1)) `cmpSlope` (LinePV origin (Vector2 3 3))
-- LT
-- >>> (LinePV origin (Vector2 5 1)) `cmpSlope` (LinePV origin (Vector2 (-3) 3))
-- GT
-- >>> (LinePV origin (Vector2 5 1)) `cmpSlope` (LinePV origin (Vector2 0 1))
-- LT
cmpSlope :: forall r. (Num r, Ord r
                      ) => LinePV 2 r -> LinePV 2 r -> Ordering
(LinePV _ u) `cmpSlope` (LinePV _ v) = case ccw (origin :: Point 2 r) (f u) (f v) of
                                         CCW      -> LT
                                         CW       -> GT
                                         CoLinear -> EQ
  where
    f w@(Vector2 x y) = Point $ case (x `compare` 0, y >= 0) of
                                  (GT,_)    -> w
                                  (EQ,True) -> w
                                  _         -> (-1) *^ w
                                  -- x < 0, or (x==0 and y <0 ; i.e. a vertical line)



--------------------------------------------------------------------------------

{-
-- | Lines are transformable, via line segments
instance ( Fractional r
         , TransformationConstraints d r
         -- , OptVector_ d r, OptMetric_ d r
         ) => IsTransformable (LinePV d r) where
  transformBy t (LinePV p v) = lineThrough p' q'
    where
      p' = transformBy t p
      q' = transformBy t (p .+^ v)

-}
