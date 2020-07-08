{-# LANGUAGE FunctionalDependencies #-}
module Control.CanAquire(
      runAcquire
    , CanAquire(..)
    , HasIndex(..)

    , replaceByIndex, labelWithIndex
    , I
    ) where

import           Control.Monad.ST.Strict
import           Control.Monad.State.Strict
import           Data.Reflection
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

--------------------------------------------------------------------------------

-- | Run a computation on something that can aquire i's.
runAcquire         :: forall t a b. Traversable t
                   => (forall i. CanAquire i a => t i -> b)
                   -> t a -> b
runAcquire alg pts = reify v $ \px -> alg (coerceTS px ts)
  where
    (v,ts) = replaceByIndex pts

    coerceTS   :: proxy s -> t Int -> t (I s a)
    coerceTS _ = fmap I
      -- Ideally this would just be a coerce. But GHC doesn't want to do that.

class HasIndex i Int => CanAquire i a where
  -- | A value of type i can obtain something of type 'a'
  aquire  :: i -> a

class HasIndex t i | t -> i where
  -- | Types that have an instance of this class can act as indices.
  indexOf :: t -> i

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

-- | A type that can act as an Index.
newtype I s a = I Int deriving (Eq, Ord, Enum)

instance Show (I s a) where
  showsPrec i (I j) = showsPrec i j

instance HasIndex (I s a) Int where
  indexOf (I i) = i

instance Reifies s (V.Vector a) => (I s a) `CanAquire` a where
  aquire (I i) = let v = reflect @s undefined in v V.! i
