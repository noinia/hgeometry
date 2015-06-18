{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Line.Internal where

import           Control.Applicative
import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import           Data.Geometry.Box
import           Data.Geometry.Interval
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import           Linear.Affine(Affine(..))
import           Linear.Vector((*^))


--------------------------------------------------------------------------------
-- * d-dimensional Lines

-- | A line is given by an anchor point and a vector indicating the
-- direction.
data Line d r = Line { _anchorPoint :: Point  d r
                     , _direction   :: Vector d r
                     }
makeLenses ''Line

deriving instance (Show r, Arity d) => Show    (Line d r)
deriving instance Arity d           => Functor (Line d)

instance Arity d => F.Foldable (Line d) where
  foldMap = T.foldMapDefault

instance Arity d => T.Traversable (Line d) where
  traverse f (Line p v) = Line <$> T.traverse f p
                               <*> T.traverse f v


type instance Dimension (Line d r) = d
type instance NumType   (Line d r) = r

-- ** Functions on lines

-- | A line may be constructed from two points.
lineThrough     :: (Num r, Arity d) => Point d r -> Point d r -> Line d r
lineThrough p q = Line p (q .-. p)

verticalLine   :: Num r => r -> Line 2 r
verticalLine x = Line (point2 x 0) (v2 0 1)

horizontalLine   :: Num r => r -> Line 2 r
horizontalLine y = Line (point2 0 y) (v2 1 0)

perpendicularTo                          :: Num r => Line 2 r -> Line 2 r
perpendicularTo (Line p (Vector2 vx vy)) = Line p (v2 (-vy) vx)





-- | Test if two lines are identical, meaning; if they have exactly the same
-- anchor point and directional vector.
isIdenticalTo                         :: (Eq r, Arity d) => Line d r -> Line d r -> Bool
(Line p u) `isIdenticalTo` (Line q v) = (p,u) == (q,v)


-- | Test if the two lines are parallel.
--
-- >>> lineThrough origin (point2 1 0) `isParallelTo` lineThrough (point2 1 1) (point2 2 1)
-- True
-- >>> lineThrough origin (point2 1 0) `isParallelTo` lineThrough (point2 1 1) (point2 2 2)
-- False
isParallelTo                         :: (Eq r, Fractional r, Arity d)
                                     => Line d r -> Line d r -> Bool
(Line _ u) `isParallelTo` (Line _ v) = u `isScalarMultipleOf` v
  -- TODO: Maybe use a specialize pragma for 2D (see intersect instance for two lines.)


-- | Test if point p lies on line l
--
-- >>> origin `onLine` lineThrough origin (point2 1 0)
-- True
-- >>> point2 10 10 `onLine` lineThrough origin (point2 2 2)
-- True
-- >>> point2 10 5 `onLine` lineThrough origin (point2 2 2)
-- False
onLine                :: (Eq r, Fractional r, Arity d) => Point d r -> Line d r -> Bool
p `onLine` (Line q v) = p == q || (p .-. q) `isScalarMultipleOf` v
  -- TODO: Maybe use a specialize pragma for 2D with an implementation using ccw


-- | The intersection of two lines is either: NoIntersection, a point or a line.
type instance IntersectionOf (Line 2 r) (Line 2 r) = [ NoIntersection
                                                     , Point 2 r
                                                     , Line 2 r
                                                     ]

instance (Eq r, Fractional r) => (Line 2 r) `IsIntersectableWith` (Line 2 r) where


  nonEmptyIntersection = defaultNonEmptyIntersection

  l@(Line p (Vector2 ux uy)) `intersect` m@(Line q v@(Vector2 vx vy))
      | areParallel = if q `onLine` l then coRec l
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
