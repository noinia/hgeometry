{-# OPTIONS_GHC -Wno-orphans #-}
module V2
  ( Vector
  , generateA
  , components
  , component'
  , liftU2
  , liftI2A
  ) where

import           Control.Lens
import qualified Data.Functor.Apply as Apply
-- import           HGeometry.Properties
import           HGeometry.Vector.Class
import           Linear.V2 (V2(..))
import           R
--------------------------------------------------------------------------------
type D = 2
type Vector = V2 R
instance Vector_ Vector D R
-- type instance Dimension Vector = 2
-- type instance NumType Vector = R


-- | Generates a vector from an Applicative operation (that takes the
-- index)
generateA   :: Applicative f => (Int -> f R) -> f Vector
generateA f = V2 <$> f 0 <*> f 1
{-# INLINE generateA #-}

-- | Traversal over the components of the vector
--
-- >>> myVector2^..components
-- [5,11]
components :: IndexedTraversal1' Int Vector R
components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'  :: Apply.Apply f => (r -> f r) -> V2 r -> f (V2 r)
      traverse' f (V2 x y)  = V2 <$> f x Apply.<.> f y
      itraverse' :: Apply.Apply f => (Int -> r -> f r) -> V2 r -> f (V2 r)
      itraverse' f (V2 x y) = V2 <$> f 0 x Apply.<.> f 1 y
{-# INLINE components #-}

-- | Lens to access the i^th coordinate.
component'                :: Int -> IndexedTraversal' Int Vector R
component' i f v@(V2 x y) = case (i :: Int) of
                              0 -> (\x' -> V2 x' y) <$> indexed f i x
                              1 -> (\y' -> V2 x y') <$> indexed f i y
                              _ -> pure v
{-# INLINE component' #-}

-- | Apply a function to merge the 'non-zero' components of two
-- vectors, unioning the rest of the values.
liftU2 :: (R -> R -> R) -> Vector -> Vector -> Vector
liftU2 f (V2 x y) (V2 x' y') = V2 (f x x') (f y y')
{-# INLINE liftU2 #-}

-- | Apply an Applicative function to the components of two vectors.
liftI2A :: Apply.Apply f => (R -> R -> f R) -> Vector -> Vector -> f Vector
liftI2A f (V2 x y) (V2 x' y') = V2 <$> f x x' Apply.<.> f y y'
{-# INLINE liftI2A #-}
