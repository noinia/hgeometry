{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.Vector where

import Data.AdditiveGroup
import Data.VectorSpace

import Data.Vinyl
import Data.Vinyl.Unicode


import GHC.TypeLits


data Vec (d :: Nat) (a :: *) where
  Nil  :: Vec 0 a
  (:.) :: a -> (Vec d a) -> Vec (d + 1) a


vZipWith                       :: forall a b c d .
                                  (a -> b -> c) -> Vec d a -> Vec d b -> Vec d c
vZipWith f Nil       _         = Nil



vZipWith' ::
                                  (a -> b -> c) -> Vec (1 + d) a -> Vec (1 + d) b -> Vec (1 + d) c
vZipWith' f (x :. xs) (y :. ys) = (f x y) :. (vZipWith f xs ys :: Vec d c)








-- instance Num r => AdditiveGroup (V2 r) where
--   zeroV = v2 (0,0)
--   v ^+^ v' = let (x1,y1) = unV2 v
--                  (x2,y2) = unV2 v'
--              in v2 (x1+x2, y1+y2)
--   negateV (unV2 -> (x',y')) = v2 (negate x', negate y')
