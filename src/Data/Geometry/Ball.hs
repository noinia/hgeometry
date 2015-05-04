{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Ball where

import Data.Ext
import Control.Lens hiding (only)
import qualified Data.List as L
import Data.Geometry.Line
import Data.Geometry.LineSegment
import Data.Geometry.Point
import Data.Geometry.Properties
import Data.Geometry.Vector
import GHC.TypeLits
import Linear.Affine(qdA, (.-.), (.+^))
import Linear.Vector((^/),(*^),(^+^))

--------------------------------------------------------------------------------
-- * A d-dimensional ball

data Ball d p r = Ball { _center        :: Point d r :+ p
                       , _squaredRadius :: r
                       }
makeLenses ''Ball

deriving instance (Show r, Show p, Arity d) => Show (Ball d p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq (Ball d p r)
deriving instance Arity d                   => Functor (Ball d p)

type instance NumType   (Ball d p r) = r
type instance Dimension (Ball d p r) = d

-- * Constructing Balls

-- | Given two points on the diameter of the ball, construct a ball.
fromDiameter     :: (Arity d, Fractional r) => Point d r -> Point d r -> Ball d () r
fromDiameter p q = let c = p .+^ ((q .-. p) ^/ 2) in Ball (only c) (qdA c p)

-- | Construct a ball given the center point and a point p on the boundary.
fromCenterAndPoint     :: (Arity d, Num r) => Point d r :+ p -> Point d r :+ p -> Ball d p r
fromCenterAndPoint c p = Ball c $ qdA (c^.core) (p^.core)

-- | A d dimensional unit ball centered at the origin.
unitBall :: (Arity d, Num r) => Ball d () r
unitBall = Ball (only origin) 1

-- * Querying if a point lies in a ball

-- | Result of a inBall query
data PointBallQueryResult = Inside | On | Outside deriving (Show,Read,Eq)

inBall                 :: (Arity d, Ord r, Num r)
                       => Point d r -> Ball d p r -> PointBallQueryResult
p `inBall` (Ball c sr) = case qdA p (c^.core) `compare` sr of
                           LT -> Inside
                           EQ -> On
                           GT -> Outside

-- | Test if a point lies strictly inside a ball
--
-- >>> (point2 0.5 0) `insideBall` unitBall
-- True
-- >>> (point2 1 0) `insideBall` unitBall
-- False
-- >>> (point2 2 0) `insideBall` unitBall
-- False
insideBall       :: (Arity d, Ord r, Num r)
                 => Point d r -> Ball d p r -> Bool
p `insideBall` b = p `inBall` b == Inside

-- | Test if a point lies in or on the ball
--
inClosedBall       :: (Arity d, Ord r, Num r)
                    => Point d r -> Ball d p r -> Bool
p `inClosedBall` b = p `inBall` b /= Outside

-- TODO: Add test cases

-- | Test if a point lies on the boundary of a ball.
--
-- >>> (point2 1 0) `onBall` unitBall
-- True
-- >>> (point3 1 1 0) `onBall` unitBall
-- False
onBall       :: (Arity d, Ord r, Num r)
             => Point d r -> Ball d p r -> Bool
p `onBall` b = p `inBall` b == On


--------------------------------------------------------------------------------
-- * Circles, aka 2-dimensional Balls

type Circle = Ball 2

-- | Given three points, get the circle through the three points. If the three
-- input points are colinear we return Nothing
--
-- >>> circle (point2 0 10) (point2 10 0) (point2 (-10) 0)
-- Just (Ball {_center = Point {toVec = Vector {_unV = fromList [0.0,0.0]}} :+ (), _squaredRadius = 100.0})
circle       :: (Eq r, Fractional r)
             => Point 2 r -> Point 2 r -> Point 2 r -> Maybe (Circle () r)
circle p q r = case f p `intersect` f q of
                 LineLineIntersection c -> Just $ Ball (only c) (qdA c p)
                 _                      -> Nothing -- The two lines f p and f q are
                                                   -- parallel, that means the three
                                                   -- input points where colinear.
  where
    -- Given a point p', get the line perpendicular, and through the midpoint
    -- of the line segment p'r
    f p' = let v        = r .-. p'
               midPoint = p' .+^ (v ^/ 2)
           in perpendicularTo (Line midPoint v)


instance (Ord r, Floating r) => (Line 2 r) `IsIntersectableWith` (Circle p r) where

  data Intersection (Line 2 r) (Circle p r) = NoLineCircleIntersection
                                            | LineTouchesCircle        (Point 2 r)
                                            | LineCircleIntersection   (Point 2 r) (Point 2 r)
                                              deriving (Show,Eq)

  nonEmptyIntersection NoLineCircleIntersection = False
  nonEmptyIntersection _                        = True

  (Line p' v) `intersect` (Ball (c :+ _) r) = case discr `compare` 0 of
                                                LT -> NoLineCircleIntersection
                                                EQ -> LineTouchesCircle $ q' (lambda (+))
                                                GT -> let [l1,l2] = L.sort [lambda (-), lambda (+)]
                                                      in LineCircleIntersection (q' l1) (q' l2)
    where
      (Vector2 vx vy)   = v
      -- (px, py) is the vector/point after translating the circle s.t. it is centered at the
      -- origin
      pv@(Vector2 px py) = p' .-. c

      -- q alpha is a point on the translated line
      q alpha = Point $ pv ^+^ alpha *^ v
      -- a point q alpha after translating it back in the situation where c is the center of the circle.
      q' alpha = q alpha .+^ toVec c

      -- let q lambda be the intersection point. We solve the following equation
      -- solving the equation (q_x)^2 + (q_y)^2 = r^2 then yields the equation
      -- L^2(vx^2 + vy^2) + L2(px*vx + py*vy) + px^2 + py^2 = 0
      -- where L = \lambda
      aa                   = vx^2 + vy^2
      bb                   = 2 * (px * vx + py * vy)
      cc                   = px^2 + py^2 - r^2
      discr                = bb^2 - 4*aa*cc
      discr'               = sqrt discr
      -- This thus gives us the following value(s) for lambda
      lambda (|+-|)        = (-bb |+-| discr') / (2*aa)


instance (Ord r, Floating r) => (LineSegment 2 p r) `IsIntersectableWith` (Circle q r) where

  data Intersection (LineSegment 2 p r) (Circle q r) = NoLineSegmentCircleIntersection
                                                     | LineSegmentTouchesCircle    (Point 2 r)
                                                     | LineSegmentIntersectsCircle (Point 2 r)
                                                     | LineSegmentCrossesCircle    (Point 2 r) (Point 2 r)
                                                     deriving (Show,Eq)

  nonEmptyIntersection NoLineSegmentCircleIntersection = False
  nonEmptyIntersection _                               = True

  s `intersect` c = case supportingLine s `intersect` c of
    NoLineCircleIntersection    -> NoLineSegmentCircleIntersection
    LineTouchesCircle p         -> if p `onSegment` s then LineSegmentTouchesCircle p
                                                      else NoLineSegmentCircleIntersection
    LineCircleIntersection p q -> case (p `onSegment` s, q `onSegment` s) of
                                    (False,False) -> NoLineSegmentCircleIntersection
                                    (False,True)  -> LineSegmentIntersectsCircle q
                                    (True, False) -> LineSegmentIntersectsCircle p
                                    (True, True)  -> LineSegmentCrossesCircle p q
