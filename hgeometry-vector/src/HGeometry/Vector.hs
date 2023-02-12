{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional vectors.
--
--------------------------------------------------------------------------------
module HGeometry.Vector(
    module HGeometry.Vector.Class
  , module HGeometry.Vector.Optimal
  , isScalarMultipleOf
  , scalarMultiple
  , sameDirection

  , Optimize(..), Choose
  ) where

import           Control.Lens
import           HGeometry.Vector.Boxed (Optimize(..), Choose)
import           HGeometry.Vector.Class
import qualified HGeometry.Vector.List as ListVector
import           HGeometry.Vector.Optimal


-- | Given two colinar vectors, u and v, test if they point in the same direction, i.e.
-- iff scalarMultiple' u v == Just lambda, with lambda > 0
--
-- pre: u and v are colinear, u and v are non-zero
sameDirection     :: forall vector vector' d r. ( Vector_ vector  d r, Vector_ vector' d r
                     , Num r, Eq r
                     ) => vector -> vector' -> Bool
sameDirection u v = andOf components $
    vZipWith @_ @_ @(ListVector.ListVector d Bool) (\ux vx -> signum ux == signum vx) u v

-- | 'isScalarmultipleof u v' test if v is a scalar multiple of u.
--
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 10 10
-- True
-- >>> Vector3 1 1 2 `isScalarMultipleOf` Vector3 10 10 20
-- True
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 10 1
-- False
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 (-1) (-1)
-- True
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 11.1 11.1
-- True
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 11.1 11.2
-- False
-- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 11.1 11.2
-- False
-- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 4 2
-- True
-- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 4 0
-- False
-- >>> Vector3 2 1 0 `isScalarMultipleOf` Vector3 4 0 5
-- False
-- >>> Vector3 0 0 0 `isScalarMultipleOf` Vector3 4 0 5
-- True
isScalarMultipleOf       :: (Eq r, Fractional r
                            , Vector_ vector d r, Metric_ vector
                            )
                         => vector -> vector  -> Bool
u `isScalarMultipleOf` v = let d = u `dot` v
                               num = quadrance u * quadrance v
                           in num == 0 || num == d*d
-- u `isScalarMultipleOf` v = isJust $ scalarMultiple u v


-- | scalarMultiple u v computes the scalar labmda s.t. v = lambda * u (if it exists)
scalarMultiple     :: (Eq r, Fractional r, Vector_ vector d r)
                   => vector -> vector -> Maybe r
scalarMultiple u v
      | allZero u || allZero v = Just 0
      | otherwise              = scalarMultiple' u v

allZero :: (Eq r, Num r, Vector_ vector d r) => vector -> Bool
allZero = allOf components (== 0)

data ScalarMultiple r = No | Maybe | Yes r deriving (Eq,Show)

instance Eq r => Semigroup (ScalarMultiple r) where
  No      <> _       = No
  _       <> No      = No
  Maybe   <> x       = x
  x       <> Maybe   = x
  (Yes x) <> (Yes y)
     | x == y               = Yes x
     | otherwise            = No


instance Eq r => Monoid (ScalarMultiple r) where
  mempty = Maybe
  mappend = (<>)

-- | Actual implementation of scalarMultiple
scalarMultiple'      :: forall vector d r. (Eq r, Fractional r, Vector_ vector d r)
                     => vector -> vector -> Maybe r
scalarMultiple' u v = g . foldOf components
                    $ vZipWith @_ @_ @(ListVector.ListVector d (ScalarMultiple r)) f u v
  where
    f 0  0  = Maybe -- we don't know lambda yet, but it may still be a scalar mult.
    f _  0  = No      -- Not a scalar multiple
    f ui vi = Yes $ ui / vi -- can still be a scalar multiple

    g No      = Nothing
    g Maybe   = error "scalarMultiple': found a Maybe, which means the vectors either have length zero, or one of them is all Zero!"
    g (Yes x) = Just x

-- {-# SPECIALIZE
--     scalarMultiple' :: (Eq r, Fractional r) => Vector 2 r -> Vector 2 r -> Maybe r #-}
