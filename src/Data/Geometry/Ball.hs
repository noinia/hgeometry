{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Ball where

import Control.Lens

import Data.Geometry.Line
import Data.Geometry.Point
import Data.Geometry.Properties
import Data.Geometry.Vector
import GHC.TypeLits
import Linear.Affine(qdA, (.-.), (.+^))
import Linear.Vector((^/))

--------------------------------------------------------------------------------

data Ball d r = Ball { _center        :: Point d r
                     , _squaredRadius :: r
                     }
makeLenses ''Ball

deriving instance (Show r, Arity d) => Show (Ball d r)
deriving instance (Eq r, Arity d)   => Eq (Ball d r)
deriving instance Arity d           => Functor (Ball d)

type instance NumType   (Ball d r) = r
type instance Dimension (Ball d r) = d


-- | Given two points on the diameter of the ball, construct a ball.
fromDiameter     :: (Arity d, Fractional r) => Point d r -> Point d r -> Ball d r
fromDiameter p q = let c = p .+^ (q .-. p ^/ 2) in Ball c (qdA c p)


data PointBallQueryResult = Inside | On | Outside deriving (Show,Read,Eq)

inBall                 :: (Arity d, Ord r, Num r)
                       => Point d r -> Ball d r -> PointBallQueryResult
p `inBall` (Ball c sr) = case qdA p c `compare` sr of
                           LT -> Inside
                           EQ -> On
                           GT -> Outside

p `insideBall` b = p `inBall` b == Inside


p `inClosedBall` b = p `inBall` b /= Outside


--------------------------------------------------------------------------------

type Circle = Ball 2

-- | Given three points, get the circle through the three points. If the three
-- input points are colinear we return Nothing
--
-- >>> circle (point2 0 10) (point2 10 0) (point2 (-10) 0)
-- Just (Ball {_center = Point {toVec = Vector {_unV = fromList [0.0,0.0]}}, _squaredRadius = 100.0})
circle       :: (Eq r, Fractional r)
             => Point 2 r -> Point 2 r -> Point 2 r -> Maybe (Circle r)
circle p q r = case f p `intersect` f q of
                 LineLineIntersection c -> Just $ Ball c (qdA c p)
                 _                      -> Nothing -- The two lines f p and f q are
                                                   -- parallel, that means the three
                                                   -- input points where colinear.
  where
    -- Given a point p', get the line perpendicular, and through the midpoint
    -- of the line segment p'r
    f p' = let v        = r .-. p'
               midPoint = p' .+^ (v ^/ 2)
           in perpendicularTo (Line midPoint v)
