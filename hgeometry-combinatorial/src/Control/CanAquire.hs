{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Control.CanAquire
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Control.CanAquire(
      runAcquire
    , CanAquire(..)

    , replaceByIndex, labelWithIndex
    , I
    ) where

import           Control.Monad.ST.Strict
import           Control.Monad.State.Strict
import           Data.Reflection
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

-- | Run a computation on something that can aquire i's.
runAcquire         :: forall t a b. Traversable t
                   => (forall s. CanAquire (I s a) => t (I s a) -> b)
                   -> t a -> b
runAcquire alg pts = reify v $ \px -> alg (coerceTS px ts)
  where
    (v,ts) = replaceByIndex pts

    coerceTS   :: proxy s -> t Int -> t (I s a)
    coerceTS _ = unsafeCoerce -- fmap I
      -- Ideally this would just be a coerce. But GHC doesn't want to do that.

class CanAquire i where
  type AquiredVal i
  -- | A value of type i can obtain something of type 'a'
  aquire  :: i -> AquiredVal i

--------------------------------------------------------------------------------

-- | Replaces every element by an index. Returns the new traversable
-- containing only these indices, as well as a vector with the
-- values. (such that indexing in this value gives the original
-- value).
replaceByIndex     :: forall t a. Traversable t => t a -> (V.Vector a, t Int)
replaceByIndex ts' = runST $ do
                               v <- MV.new n
                               t <- traverse (lbl v) ts
                               (,t) <$> V.unsafeFreeze v
  where
    (ts, n) = labelWithIndex ts'

    lbl         :: MV.MVector s' a -> (Int,a) -> ST s' Int
    lbl v (i,x) = MV.write v i x >> pure i

-- | Label each element with its index. Returns the new collection as
-- well as its size.
labelWithIndex :: Traversable t => t a -> (t (Int, a), Int)
labelWithIndex = flip runState 0 . traverse lbl
  where
    lbl   :: a -> State Int (Int,a)
    lbl x = do i <- get
               put $ i+1
               pure (i,x)


--------------------------------------------------------------------------------

-- | A type that can act as an Index/Pointer
newtype I (s :: *) a = I Int deriving (Eq, Ord, Enum)

instance Show (I s a) where
  showsPrec i (I j) = showsPrec i j

instance Reifies s (V.Vector a) => CanAquire (I s a) where
  type AquiredVal (I s a) = a
  aquire (I i) = let v = reflect @s undefined in v V.! i
