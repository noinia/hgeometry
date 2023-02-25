{-# OPTIONS_GHC -Wno-orphans #-}
module V1
  ( Vector
  , generateA
  , components
  , component'
  , liftU2
  , liftI2A
  ) where

import           Control.Lens
import qualified Data.Functor.Apply as Apply
import           HGeometry.Properties
import           HGeometry.Vector.Class
import           Linear.V1 (V1(..))
import           R

--------------------------------------------------------------------------------

type D = 1
type Vector = V1 R
instance V_ Vector D R
type instance Dimension Vector = 1
type instance NumType Vector = R

-- | Generates a vector from an Applicative operation (that takes the
-- index)
generateA   :: Applicative f => (Int -> f R) -> f Vector
generateA f = V1 <$> f 0
{-# INLINE generateA #-}

-- | Traversal over the components of the vector
--
-- >>> myVector^..components
-- [5]
components :: IndexedTraversal1' Int Vector R
components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'  :: Apply.Apply f => (r -> f r) -> V1 r -> f (V1 r)
      traverse' f (V1 x)  = V1 <$> f x
      itraverse' :: Apply.Apply f => (Int -> r -> f r) -> V1 r -> f (V1 r)
      itraverse' f (V1 x) = V1 <$> f 0 x
{-# INLINE components #-}

-- | Lens to access the i^th coordinate.
component'                :: Int -> IndexedTraversal' Int Vector R
component' i f v@(V1 x) = case (i :: Int) of
                            0 -> V1 <$> indexed f i x
                            _ -> pure v
{-# INLINE component' #-}

-- | Apply a function to merge the 'non-zero' components of two
-- vectors, unioning the rest of the values.
liftU2                  :: (R -> R -> R) -> Vector -> Vector -> Vector
liftU2 f (V1 x) (V1 x') = V1 (f x x')
{-# INLINE liftU2 #-}

-- | Apply an Applicative function to the components of two vectors.
liftI2A :: Apply.Apply f => (R -> R -> f R) -> Vector -> Vector -> f Vector
liftI2A f (V1 x) (V1 x') = V1 <$> f x x'
{-# INLINE liftI2A #-}
