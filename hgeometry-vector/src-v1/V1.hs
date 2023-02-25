--------------------------------------------------------------------------------
-- |
-- Module      :  V1
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of a one dimensional vector
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module V1
  ( Vector(..)
  , generateA
  , components
  , component'
  , liftU2
  , liftI2A
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Functor.Apply
import qualified Data.Functor.Apply as Apply
import           GHC.Generics (Generic)
import           HGeometry.Vector.Class
-- import           HGeometry.Properties
import           R

--------------------------------------------------------------------------------

-- | The Vector type
newtype Vector = Vector1 R
  deriving stock (Generic)

-- type instance Dimension Vector = 1
-- type instance NumType Vector = R
instance V_ Vector 1 R

-- type Dim = 1

-- deriving stock instance Show R => Show Vector
-- deriving stock instance Read R => Read Vector

deriving newtype instance Eq R     => Eq Vector
deriving newtype instance Ord R    => Ord Vector
deriving newtype instance NFData R => NFData Vector


type instance IxValue   Vector = R

generateA   :: Applicative f => (Int -> f R) -> f Vector
generateA f = Vector1 <$> f 0
{-# INLINE generateA #-}

components :: IndexedTraversal1' Int Vector R
components = conjoined traverse' (itraverse' . indexed)
  where
    -- traverse'            :: Apply f => (R -> f R) -> Vector -> f Vec
    traverse' f (Vector1 x)  = Vector1 <$> f x
    itraverse'              :: Apply f => (Int -> R -> f R) -> Vector -> f Vector
    itraverse' f (Vector1 x) = Vector1 <$> f 0 x
{-# INLINE components #-}

-- | Lens to access the i^th coordinate.
component' :: Int -> IndexedTraversal' Int Vector R
component' i f s@(Vector1 x) = case i of
    0 -> Vector1 <$> indexed f (0 :: Int) x
    _ -> pure s
{-# INLINE component' #-}

-- | Apply a function to merge the 'non-zero' components of two
-- vectors, unioning the rest of the values.
liftU2  :: (R -> R -> R) -> Vector -> Vector -> Vector
liftU2 f (Vector1 x) (Vector1 x') = Vector1 (f x x')
{-# INLINE liftU2 #-}

-- | Apply an Applicative function to the components of two vectors.
liftI2A :: Apply.Apply f => (R -> R -> f R) -> Vector -> Vector -> f Vector
liftI2A f (Vector1 x) (Vector1 x') = Vector1 <$> f x x'
{-# INLINE liftI2A #-}
