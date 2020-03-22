{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Vector
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional vectors.
--
--------------------------------------------------------------------------------
module Data.Geometry.Vector( module Data.Geometry.Vector.VectorFamily
                           , module LV
                           , C(..)
                           , Affine(..)
                           , quadrance, qdA, distanceA
                           , dot, norm, signorm
                           , isScalarMultipleOf
                           , scalarMultiple
                           -- reexports
                           , FV.replicate
                           , FV.imap
                           , xComponent, yComponent, zComponent
                           ) where

import           Control.Applicative (liftA2)
import           Control.Lens (Lens')
import           Control.Monad.State
import qualified Data.Foldable as F
import           Data.Geometry.Properties
import           Data.Geometry.Vector.VectorFamily
import           Data.Geometry.Vector.VectorFixed (C(..))
import qualified Data.Vector.Fixed as FV
import           GHC.TypeLits
import           Linear.Affine (Affine(..), qdA, distanceA)
import           Linear.Metric (dot,norm,signorm,quadrance)
import           Linear.Vector as LV
import           System.Random (Random(..))
import           Test.QuickCheck (Arbitrary(..),infiniteList)

--------------------------------------------------------------------------------

type instance Dimension (Vector d r) = d
type instance NumType   (Vector d r) = r

instance (Arbitrary r, Arity d) => Arbitrary (Vector d r) where
  arbitrary = vectorFromListUnsafe <$> infiniteList

instance (Random r, Arity d) => Random (Vector d r) where
  randomR (lows,highs) g0 = flip runState g0 $
                            FV.zipWithM (\l h -> state $ randomR (l,h)) lows highs
  random g0 = flip runState g0 $ FV.replicateM (state random)

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
isScalarMultipleOf       :: (Eq r, Fractional r, Arity d)
                         => Vector d r -> Vector d r -> Bool
u `isScalarMultipleOf` v = let d = u `dot` v
                               num = quadrance u * quadrance v
                           in num == 0 || 1 == d*d / num
-- u `isScalarMultipleOf` v = isJust $ scalarMultiple u v
{-# SPECIALIZE
    isScalarMultipleOf :: (Eq r, Fractional r) => Vector 2 r -> Vector 2 r -> Bool  #-}
{-# SPECIALIZE
    isScalarMultipleOf :: (Eq r, Fractional r) => Vector 3 r -> Vector 3 r -> Bool  #-}

-- | scalarMultiple u v computes the scalar labmda s.t. v = lambda * u (if it exists)
scalarMultiple     :: (Eq r, Fractional r, Arity d)
                   => Vector d r -> Vector d r -> Maybe r
scalarMultiple u v
      | allZero u || allZero v = Just 0
      | otherwise              = scalarMultiple' u v
{-# SPECIALIZE
    scalarMultiple :: (Eq r, Fractional r) => Vector 2 r -> Vector 2 r -> Maybe r #-}


-- -- | Helper function for computing the scalar multiple. The result is a pair
-- -- (b,mm), where b indicates if v is a scalar multiple of u, and mm is a Maybe
-- -- scalar multiple. If the result is Nothing, the scalar multiple is zero.
-- scalarMultiple'     :: (Eq r, Fractional r, GV.Arity d)
--                     => Vector d r -> Vector d r -> (Bool,Maybe r)
-- scalarMultiple' u v = F.foldr allLambda (True,Nothing) $ FV.zipWith f u v
--   where
--     f ui vi = (ui == 0 && vi == 0, ui / vi)
--     allLambda (True,_)      x               = x
--     allLambda (_, myLambda) (b,Nothing)     = (b,Just myLambda) -- no lambda yet
--     allLambda (_, myLambda) (b,Just lambda) = (myLambda == lambda && b, Just lambda)


allZero :: (Arity d, Eq r, Num r) => Vector d r -> Bool
allZero = F.all (== 0)
{-# SPECIALIZE allZero :: (Eq r, Num r) => Vector 2 r -> Bool #-}


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
scalarMultiple'      :: (Eq r, Fractional r, Arity d)
                     => Vector d r -> Vector d r -> Maybe r
scalarMultiple' u v = g . F.foldr mappend mempty $ liftA2 f u v
  where
    f 0  0  = Maybe -- we don't know lambda yet, but it may still be a scalar mult.
    f _  0  = No      -- Not a scalar multiple
    f ui vi = Yes $ ui / vi -- can still be a scalar multiple

    g No      = Nothing
    g Maybe   = error "scalarMultiple': found a Maybe, which means the vectors either have length zero, or one of them is all Zero!"
    g (Yes x) = Just x
{-# SPECIALIZE
    scalarMultiple' :: (Eq r, Fractional r) => Vector 2 r -> Vector 2 r -> Maybe r #-}


--------------------------------------------------------------------------------
-- * Helper functions specific to two and three dimensional vectors

xComponent :: (1 <= d, Arity d) => Lens' (Vector d r) r
xComponent = element (C :: C 0)
{-# INLINABLE xComponent #-}

yComponent :: (2 <= d, Arity d) => Lens' (Vector d r) r
yComponent = element (C :: C 1)
{-# INLINABLE yComponent #-}

zComponent :: (3 <= d, Arity d) => Lens' (Vector d r) r
zComponent = element (C :: C 2)
{-# INLINABLE zComponent #-}
