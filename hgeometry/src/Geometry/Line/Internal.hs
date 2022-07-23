{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Line.Internal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional lines.
--
--------------------------------------------------------------------------------
module Geometry.Line.Internal where

import           Control.DeepSeq
import           Control.Lens
import qualified Data.Foldable as F
import           Data.Ord (comparing)
import qualified Data.Traversable as T
import           Data.Vinyl
import           Data.Vinyl.CoRec
import           GHC.Generics (Generic)
import           Geometry.Line.Class
import           Geometry.Point.Class
import           Geometry.Point.EuclideanDistance
import           Geometry.Point.Internal
import           Geometry.Point.Orientation.Degenerate
import           Geometry.Properties
import           Geometry.Vector
import           Test.QuickCheck

--------------------------------------------------------------------------------
-- * d-dimensional Lines

-- | A line is given by an anchor point and a vector indicating the
-- direction.
data Line d r = Line { _anchorPoint :: !(Point  d r)
                     , _direction   :: !(Vector d r)
                     } deriving Generic

instance Line_ Line d r where
  mkLine (fromGenericPoint -> p) = Line p
  anchorPoint = lens _anchorPoint (\line pt -> line{_anchorPoint=pt})
  direction = lens _direction (\line dir -> line{_direction=dir})

instance (Show r, Arity d) => Show (Line d r) where
  show (Line p v) = concat [ "Line (", show p, ") (", show v, ")" ]

-- -- TODO:
-- instance (Read r, Arity d)   => Read (Line d r) where


deriving instance (NFData r, Arity d) => NFData        (Line d r)
deriving instance Arity d             => Functor       (Line d)
deriving instance Arity d             => F.Foldable    (Line d)
deriving instance Arity d             => T.Traversable (Line d)

instance (Arity d, Eq r, Fractional r) => Eq (Line d r) where
  l@(Line p _) == m = l `isParallelTo` m && p `onLine` m



instance (Arbitrary r, Arity d, Num r, Eq r) => Arbitrary (Line d r) where
  arbitrary = do p <- arbitrary @(Point d r)
                 q <- suchThat arbitrary (/= p)
                 return $ lineThrough p q

type instance Dimension (Line d r) = d
type instance NumType   (Line d r) = r

-- ** Functions on lines


-- | Test if two lines are identical, meaning; if they have exactly the same
-- anchor point and directional vector.
isIdenticalTo                         :: (Eq r, Arity d) => Line d r -> Line d r -> Bool
(Line p u) `isIdenticalTo` (Line q v) = (p,u) == (q,v)


{-# RULES
"isParallelTo/isParallelTo2" [3]
     forall (l1 :: forall r. Line 2 r) l2. isParallelTo l1 l2 = isParallelTo2 l1 l2
#-}

-- | Check whether two lines are parallel
isParallelTo2 :: (Eq r, Num r) => Line 2 r -> Line 2 r -> Bool
isParallelTo2 (Line _ (Vector2 ux uy)) (Line _ (Vector2 vx vy)) = denom == 0
    where
      denom       = vy * ux - vx * uy

-- | Specific 2d version of testing if apoint lies on a line.
onLine2 :: (Ord r, Num r, Point_ point 2 r) => point 2 r -> Line 2 r -> Bool
p `onLine2` (Line q v) = ccw (fromGenericPoint p) q (q .+^ v) == CoLinear

-- | The intersection of two lines is either: NoIntersection, a point or a line.
type instance IntersectionOf (Line 2 r) (Line 2 r) = [ NoIntersection
                                                     , Point 2 r
                                                     , Line 2 r
                                                     ]

instance (Ord r, Num r) => Line 2 r `HasIntersectionWith` Line 2 r where
  l1 `intersects` l2@(Line q _) = not (l1 `isParallelTo2` l2) || q `onLine2` l1

instance (Ord r, Fractional r) => Line 2 r `IsIntersectableWith` Line 2 r where
  nonEmptyIntersection = defaultNonEmptyIntersection
  l@(Line p ~(Vector2 ux uy)) `intersect` (Line q ~v@(Vector2 vx vy))
      | areParallel = if q `onLine2` l then coRec l
                                       else coRec NoIntersection
      | otherwise   = coRec r
    where
      r = q .+^ alpha *^ v

      denom       = vy * ux - vx * uy
      areParallel = denom == 0
      -- Instead of using areParallel, we can also use the generic 'isParallelTo' function
      -- for lines of arbitrary dimension, but this is a bit more efficient.

      alpha        = (ux * (py - qy) + uy * (qx - px)) / denom

      Point2 px py = p
      Point2 qx qy = q

--------------------------------------------------------------------------------
-- * Supporting Lines

-- | Types for which we can compute a supporting line, i.e. a line that contains the thing of type t.
class HasSupportingLine t where
  supportingLine :: t -> Line (Dimension t) (NumType t)

instance HasSupportingLine (Line d r) where
  supportingLine = id

--------------------------------------------------------------------------------
-- * Convenience functions on Two dimensional lines

-- | Create a line from the linear function ax + b
fromLinearFunction     :: Num r => r -> r -> Line 2 r
fromLinearFunction a b = Line (Point2 0 b) (Vector2 1 a)

{- HLINT ignore toLinearFunction -}
-- | get values a,b s.t. the input line is described by y = ax + b.
-- returns Nothing if the line is vertical
toLinearFunction                             :: forall r. (Fractional r, Ord r)
                                             => Line 2 r -> Maybe (r,r)
toLinearFunction l@(Line _ ~(Vector2 vx vy)) = match (l `intersect`
                                                       verticalLine @Line @r 0) $
       (H $ \NoIntersection -> Nothing)    -- l is a vertical line
    :& (H $ \(Point2 _ b)   -> Just (vy / vx,b))
    :& (H $ \_              -> Nothing)    -- l is a vertical line (through x=0)
    :& RNil


instance (Fractional r, Arity d) => HasSquaredEuclideanDistance (Line d r) where
  pointClosestTo (fromGenericPoint -> p) (Line a m) = fromGenericPoint $ a .+^ (t0 *^ m)
    where
      -- see https://monkeyproofsolutions.nl/wordpress/how-to-calculate-the-shortest-distance-between-a-point-and-a-line/
      t0 = numerator / divisor
      numerator = (p .-. a) `dot` m
      divisor  = m `dot` m


-- | Result of a side test
data SideTestUpDown = Below | On | Above deriving (Show,Read,Eq,Ord)

class OnSideUpDownTest t where
  -- | Given a point q and a hyperplane h, compute to which side of h q lies. For
  -- vertical hyperplanes the left side of the hyperplane is interpeted as below.
  onSideUpDown :: (d ~ Dimension t, r ~ NumType t, Ord r, Num r, Point_ point d r)
               => point d r -> t -> SideTestUpDown

instance OnSideUpDownTest (Line 2 r) where
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
  q `onSideUpDown` (Line p v) = let r    =  p .+^ v
                                    f z         = (z^.xCoord, -z^.yCoord)
                                    minBy g a b = F.minimumBy (comparing g) [a,b]
                                    maxBy g a b = F.maximumBy (comparing g) [a,b]
                                in case ccw (minBy f p r) (maxBy f p r) (fromGenericPoint q) of
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
                         ) => point 2 r -> Line 2 r -> SideTest
q `onSide` (Line p v) = let r    =  p .+^ v
                            -- f z         = (z^.xCoord, -z^.yCoord)
                            -- minBy g a b = F.minimumBy (comparing g) [a,b]
                            -- maxBy g a b = F.maximumBy (comparing g) [a,b]
                        in case ccw p r (fromGenericPoint q) of
                          CCW      -> LeftSide
                          CW       -> RightSide
                          CoLinear -> OnLine

-- | Test if the query point q lies (strictly) above line l
liesAbove       :: (Ord r, Num r, Point_ point 2 r) => point 2 r -> Line 2 r -> Bool
q `liesAbove` l = q `onSideUpDown` l == Above

-- | Test if the query point q lies (strictly) above line l
liesBelow      :: (Ord r, Num r, Point_ point 2 r) => point 2 r -> Line 2 r -> Bool
q `liesBelow` l = q `onSideUpDown` l == Below

-- | Get the bisector between two points
bisector     :: (Fractional r, Point_ point 2 r) => point 2 r -> point 2 r -> Line 2 r
bisector p q = let v = q .-. p
                   h = fromGenericPoint $ p .+^ (v ^/ 2)
               in perpendicularTo (Line h v)


-- | Compares the lines on slope. Vertical lines are considered larger than
-- anything else.
--
-- >>> (Line origin (Vector2 5 1)) `cmpSlope` (Line origin (Vector2 3 3))
-- LT
-- >>> (Line origin (Vector2 5 1)) `cmpSlope` (Line origin (Vector2 (-3) 3))
-- GT
-- >>> (Line origin (Vector2 5 1)) `cmpSlope` (Line origin (Vector2 0 1))
-- LT
cmpSlope :: forall r. (Num r, Ord r) => Line 2 r -> Line 2 r -> Ordering
(Line _ u) `cmpSlope` (Line _ v) = case ccw (origin :: Point 2 r) (f u) (f v) of
                                     CCW      -> LT
                                     CW       -> GT
                                     CoLinear -> EQ
  where
    f w@(Vector2 x y) = Point $ case (x `compare` 0, y >= 0) of
                                  (GT,_)    -> w
                                  (EQ,True) -> w
                                  _         -> (-1) *^ w
                                  -- x < 0, or (x==0 and y <0 ; i.e. a vertical line)
