{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.Vector where

import Control.Applicative

import Data.List(genericReplicate)

import Data.AdditiveGroup
import Data.Basis
import Data.VectorSpace


import Data.Vinyl
import Data.Vinyl.Unicode


import GHC.TypeLits

newtype Vec (d :: Nat) r = Vec [r]
                           deriving (Show,Eq,Ord)


pure'   :: forall a d. SingI (d :: Nat) => a -> Vec d a
pure' x = Vec $ genericReplicate (fromSing (sing :: Sing (d :: Nat))) x

vZipWith                     :: forall a b c d.
                                (a -> b -> c) -> Vec d a -> Vec d b -> Vec d c
vZipWith f (Vec xs) (Vec ys) = Vec $ zipWith f xs ys

app' :: forall a b d . Vec d (a -> b) -> Vec d a -> Vec d b
app' = vZipWith ($)


instance Functor (Vec d) where
  fmap f (Vec xs) = Vec $ fmap f xs

instance SingI (d :: Nat) => Applicative (Vec d) where
  pure  = pure'
  (<*>) = app'


instance (SingI (d :: Nat), Num r) => AdditiveGroup (Vec d r) where
  zeroV = pure 0
  (^+^) = vZipWith (+)
  negateV = fmap negate

instance (SingI (d :: Nat), Num r) => VectorSpace (Vec d r) where
  type Scalar (Vec d r) = r
  s *^ v = fmap (s*) v

instance (SingI (d :: Nat), AdditiveGroup r, Num r) => InnerSpace (Vec d r) where
  v <.> v' = let (Vec r) = vZipWith (*) v v' in
             sum r



test :: Vec 3 Int
test = zeroV

foo :: Vec 3 Int
foo = Vec [1,2,3]


-- data Vec (d :: Nat) (a :: *) where
--   Nil  ::                 Vec 0       a
--   (:.) :: a -> Vec d a -> Vec (d + 1) a


-- -- myVec :: Vec 2 Int
-- myVec = 1 :. (2 :. Nil)


-- vReplicate     :: Sing (n :: Nat) -> a -> Vec n a
-- vReplicate s x = case fromSing s of
--                    0 -> Nil
--                    _ -> x :. vReplicate (sing') x
--   where
--     sing' = sing :: Sing ((n - 1) :: Nat)


-- vZipWith                       :: (a -> b -> c) -> Vec d a -> Vec d b -> Vec d c
-- vZipWith f Nil       _         = Nil
-- -- vZipWith f (x :. xs) (y :. ys) = f x y :. vZipWith f xs ys

-- vReplicate :: (Num r, SingE (n:: Nat)) => Vec n r
-- vReplicate = case fromSing (sing :: Nat) of
--                0 -> Nil
--                m ->
