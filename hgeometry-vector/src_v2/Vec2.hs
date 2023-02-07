{-# LANGUAGE AllowAmbiguousTypes #-}
module Vec2 where

import           Control.DeepSeq
import           Control.Lens
import qualified Data.Functor.Apply as Apply
import           Data.Proxy
import           Data.Type.Ord
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           HGeometry.Properties
import           HGeometry.Sigs.R

--------------------------------------------------------------------------------

myVec :: Vector
myVec = Vector2 5 11



type D = 2

-- | 2-dimensional vector
data Vector = Vector2 {-# UNPACK #-}!R
                      {-# UNPACK #-}!R
  deriving stock (Show,Eq,Ord,Generic)

instance NFData Vector

type instance NumType   Vector = R
type instance Dimension Vector = D

--------------------------------------------------------------------------------
-- * HGeometry.Sigs.Vector


components :: IndexedTraversal1' Int Vector R
components = conjoined trav (itrav.indexed)
  where
    trav                 :: Apply.Apply f => (R -> f R) -> Vector -> f Vector
    trav f (Vector2 x y) = Vector2 <$> f x Apply.<.> f y

    itrav                 :: Apply.Apply f => (Int -> R -> f R) -> Vector -> f Vector
    itrav f (Vector2 x y) = Vector2 <$> f 0 x Apply.<.> f 1 y
{-# INLINE components #-}

----------------------------------------

type instance Index   Vector = Int
type instance IxValue Vector = R

instance Ixed Vector where
  ix i f v@(Vector2 x y) = case i of
                             0 -> flip Vector2 y <$> f x
                             1 -> Vector2 x      <$> f y
                             _ -> pure v
  {-# INLINE ix #-}


component :: forall i. (i <= D, KnownNat i) => IndexedLens' Int Vector R
component = let i = fromInteger @Int (natVal $ Proxy @i)
        in case i of
             0 -> ilens (\(Vector2 x _) -> (i,x)) (\(Vector2 _ y) x -> Vector2 x y)
             1 -> ilens (\(Vector2 _ y) -> (i,y)) (\(Vector2 x _) y -> Vector2 x y)
             _ -> error "coord: absurd"
{-# INLINE component #-}


--------------------------------------------------------------------------------
-- * HGeometry.Sigs.Vector.Additive

zero :: Vector
zero = Vector2 0 0

liftU2                                 :: (R -> R -> R) -> Vector -> Vector -> Vector
liftU2 f (Vector2 x y) (Vector2 x' y') = Vector2 (f x x') (f y y')

liftI2                                 :: (R -> R -> R) -> Vector -> Vector -> Vector
liftI2 f (Vector2 x y) (Vector2 x' y') = Vector2 (f x x') (f y y')

--------------------------------------------------------------------------------
-- * HGeometry.Sigs.Vector.Metric

-- nothing to implement.
