{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.HalfSpace
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional HalfSpaces
--
--------------------------------------------------------------------------------
module Data.Geometry.HalfSpace where

import Control.Lens
import Data.Geometry.HalfLine
import Data.Geometry.HyperPlane
import Data.Geometry.Line
import Data.Geometry.Point
import Data.Geometry.Properties
import Data.Geometry.Transformation
import Data.Geometry.Boundary
import Data.Geometry.Vector
import Data.Vinyl.CoRec
import Data.Vinyl.Core
import GHC.Generics (Generic)
import GHC.TypeLits

--------------------------------------------------------------------------------

-- $setup
-- >>> :{
-- let myVector :: Vector 3 Int
--     myVector = Vector3 1 2 3
--     myPoint = Point myVector
-- :}


--------------------------------------------------------------------------------

-- | A Halfspace in \(d\) dimensions. Note that the intended side of
-- the halfspace is already indicated by the normal vector of the
-- bounding plane.
newtype HalfSpace d r = HalfSpace { _boundingPlane :: HyperPlane d  r }
                       deriving Generic
makeLenses ''HalfSpace

deriving instance (Arity d, Show r)   => Show    (HalfSpace d r)
-- deriving instance (NFData r, Arity d) => NFData  (HalfSpace d r)
deriving instance Arity d => Functor     (HalfSpace d)
deriving instance Arity d => Foldable    (HalfSpace d)
deriving instance Arity d => Traversable (HalfSpace d)

type instance NumType (HalfSpace d r)   = r
type instance Dimension (HalfSpace d r) = d

deriving instance (Arity d, Arity (d + 1), Fractional r) => IsTransformable (HalfSpace d r)

instance (Arity d, Eq r, Fractional r) => Eq (HalfSpace d r) where
  (HalfSpace h) == (HalfSpace h') = let u = h^.normalVec
                                        v = h'^.normalVec
                                        d = quadrance (u ^+^ v) - (quadrance u)
                                    in h == h' && signum d == 1

--------------------------------------------------------------------------------

type HalfPlane = HalfSpace 2



-- | Get the halfplane left of a line (i.e. "above") a line
--
-- >>> leftOf $ horizontalLine 4
-- HalfSpace {_boundingPlane = HyperPlane {_inPlane = Point2 [0,4], _normalVec = Vector2 [0,1]}}
leftOf   :: Num r => Line 2 r -> HalfPlane r
leftOf l = (rightOf l)&boundingPlane.normalVec %~ ((-1) *^)

-- | Get the halfplane right of a line (i.e. "below") a line
--
-- >>> rightOf $ horizontalLine 4
-- HalfSpace {_boundingPlane = HyperPlane {_inPlane = Point2 [0,4], _normalVec = Vector2 [0,-1]}}
rightOf   :: Num r => Line 2 r -> HalfPlane r
rightOf l = HalfSpace $ l^.re _asLine

above :: Num r => Line 2 r -> HalfPlane r
above = leftOf

below :: Num r => Line 2 r -> HalfPlane r
below = rightOf

--------------------------------------------------------------------------------

-- type HalfPlane r = GHalfSpace (Line 2 r)

-- type HalfSpace d r = GHalfSpace (HyperPlane d r)

-- TODO: Property test that in 2d this is the same as CCW

type instance IntersectionOf (Point d r) (HalfSpace d r) = [NoIntersection, Point d r]

instance (Num r, Ord r, Arity d) => Point d r `IsIntersectableWith` HalfSpace d r where
  nonEmptyIntersection = defaultNonEmptyIntersection

  q `intersects` h = q `inHalfSpace` h /= Outside

  q `intersect` h | q `intersects` h = coRec q
                  | otherwise        = coRec NoIntersection



type instance IntersectionOf (Line d r) (HalfSpace d r) =
    [NoIntersection, HalfLine d r, Line d r]


instance (Fractional r, Ord r) => Line 2 r `IsIntersectableWith` HalfSpace 2 r where
  nonEmptyIntersection = defaultNonEmptyIntersection

  l@(Line o v) `intersect` h = match (l `intersect` m) $
         (H $ \NoIntersection -> if o `intersects` h
                                   then coRec l
                                   else coRec NoIntersection)
      :& (H $ \p              -> if (p .+^ v) `intersects` h
                                   then coRec $ HalfLine p v
                                   else coRec $ HalfLine p ((-1) *^ v))
      :& (H $ \_l             -> coRec l)
      :& RNil
    where
      m = h^.boundingPlane._asLine


-- | Test if a point lies in a halfspace
inHalfSpace                                  :: (Num r, Ord r, Arity d)
                                             => Point d r -> HalfSpace d r
                                             -> PointLocationResult
q `inHalfSpace` (HalfSpace (HyperPlane p n)) = case n `dot` (q .-. p) `compare` 0 of
                                                 LT -> Outside
                                                 EQ -> OnBoundary
                                                 GT -> Inside
