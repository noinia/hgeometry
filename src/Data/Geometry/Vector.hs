module Data.Geometry.Vector( module GV
                           , module FV
                           , isScalarMultipleOf
                           ) where

import qualified Data.Vector.Fixed as FV
import qualified Data.Foldable as F
import Data.Geometry.Vector.VectorFixed as GV


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
isScalarMultipleOf       :: (Eq r, Fractional r, GV.Arity d)
                         => Vector d r -> Vector d r -> Bool
u `isScalarMultipleOf` v = fst . F.foldr allLambda (True,Nothing) $ FV.zipWith f u v
  where
    f ui vi = (ui == 0 && vi == 0, ui / vi)
    allLambda (True,_)      x               = x
    allLambda (_, myLambda) (b,Nothing)     = (b,Just myLambda) -- no lambda yet
    allLambda (_, myLambda) (b,Just lambda) = (myLambda == lambda && b, Just lambda)
