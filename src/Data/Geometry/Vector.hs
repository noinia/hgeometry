module Data.Geometry.Vector( module Data.Geometry.Vector.VectorFixed
                           , module FV
                           , module LV
                           , Affine(..)
                           , isScalarMultipleOf
                           , scalarMultiple
                           ) where

import qualified Data.Foldable                    as F
import           Data.Geometry.Vector.VectorFixed
import           Data.Geometry.Vector.VectorFixed as GV
import           Data.Maybe
import           Data.Monoid
import qualified Data.Vector.Fixed                as FV
import           Linear.Affine(Affine(..))
import           Linear.Vector as LV


-- | Test if v is a scalar multiple of u.
--
-- >>> v2 1 1 `isScalarMultipleOf` v2 10 10
-- True
-- >>> v2 1 1 `isScalarMultipleOf` v2 10 1
-- False
-- >>> v2 1 1 `isScalarMultipleOf` v2 11.1 11.1
-- True
-- >>> v2 1 1 `isScalarMultipleOf` v2 11.1 11.2
-- False
-- >>> v2 2 1 `isScalarMultipleOf` v2 11.1 11.2
-- False
-- >>> v2 2 1 `isScalarMultipleOf` v2 4 2
-- True
-- >>> v2 2 1 `isScalarMultipleOf` v2 4 0
-- False
isScalarMultipleOf       :: (Eq r, Fractional r, GV.Arity d)
                         => Vector d r -> Vector d r -> Bool
u `isScalarMultipleOf` v = isJust $ scalarMultiple u v

-- | Get the scalar labmda s.t. v = lambda * u (if it exists)
scalarMultiple     :: (Eq r, Fractional r, GV.Arity d)
                   => Vector d r -> Vector d r -> Maybe r
scalarMultiple u v
      | allZero u || allZero v = Just 0
      | otherwise              = scalarMultiple' u v


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


allZero :: (GV.Arity d, Eq r, Num r) => Vector d r -> Bool
allZero = F.all (== 0)


data ScalarMultiple r = No | Maybe | Yes r deriving (Eq,Show)

instance Eq r => Monoid (ScalarMultiple r) where
  mempty = Maybe

  No      `mappend` _       = No
  _       `mappend` No      = No
  Maybe   `mappend` x       = x
  x       `mappend` Maybe   = x
  (Yes x) `mappend` (Yes y)
     | x == y               = Yes x
     | otherwise            = No


scalarMultiple'      :: (Eq r, Fractional r, GV.Arity d)
                     => Vector d r -> Vector d r -> Maybe r
scalarMultiple' u v = g . F.foldr mappend mempty $ FV.zipWith f u v
  where
    f 0  0  = Maybe -- we don't know lambda yet, but it may still be a scalar mult.
    f ui 0  = No      -- Not a scalar multiple
    f ui vi = Yes $ ui / vi -- can still be a scalar multiple

    g No      = Nothing
    g Maybe   = error "scalarMultiple': found a Maybe, which means the vectors either have length zero, or one of them is all Zero!"
    g (Yes x) = Just x
