--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Vector2.V2
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Two dimensional vectors, using the implementation from Linear.V2
--
--------------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes #-}
module HGeometry.Vector.Vector2.V2
  ( Vector(..), pattern Vector2
  , components, component
  , zero, liftU2, liftI2
  , D
  ) where

import           Control.DeepSeq
import           Control.Lens
import qualified Data.Functor.Apply as Apply
import           Data.Proxy
import           Data.Type.Ord
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           HGeometry.Properties
import           Linear.Vector
import           Linear.V2
import           R

--------------------------------------------------------------------------------

type D = 2

type Vector = V2 R

pattern Vector2     :: r -> r -> V2 r
pattern Vector2 x y = V2 x y
{-# COMPLETE Vector2 #-}

type instance NumType   Vector = R
type instance Dimension Vector = D


components :: IndexedTraversal1' Int Vector R
components = conjoined trav (itrav.indexed)
  where
    trav            :: Apply.Apply f => (R -> f R) -> Vector -> f Vector
    trav f (V2 x y) = V2 <$> f x Apply.<.> f y

    itrav            :: Apply.Apply f => (Int -> R -> f R) -> Vector -> f Vector
    itrav f (V2 x y) = V2 <$> f 0 x Apply.<.> f 1 y
{-# INLINE components #-}


component :: forall i. (i < D, KnownNat i) => IndexedLens' Int Vector R
component = let i = fromInteger @Int (natVal $ Proxy @i)
        in case i of
             0 -> ilens (\(V2 x _) -> (i,x)) (\(V2 _ y) x -> V2 x y)
             1 -> ilens (\(V2 _ y) -> (i,y)) (\(V2 x _) y -> V2 x y)
             _ -> error "coord: absurd"
{-# INLINE component #-}
