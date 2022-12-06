{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.List
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of d-dimensional vectors using basic Lists
--
--------------------------------------------------------------------------------
module HGeometry.Vector.List
  ( ListVector(..)
  ) where

import Control.Applicative (liftA2)
import Control.Lens
-- import qualified Data.List as List
import Data.Proxy
import GHC.Generics
import GHC.TypeNats
import HGeometry.Properties
import HGeometry.Vector.Class

--------------------------------------------------------------------------------

-- | Implementation of a vector by a List.
newtype ListVector d r = ListVector [r]
                       deriving ( Show,Eq,Ord,Functor,Foldable,Traversable
                                , FunctorWithIndex Int
                                , FoldableWithIndex Int
                                , Generic
                                )

instance KnownNat d => Applicative (ListVector d) where
  pure x = let d = fromIntegral . natVal $ Proxy @d
           in ListVector $ replicate d x
  (ListVector fs) <*> (ListVector xs) = ListVector $ zipWith ($) fs xs


instance Wrapped (ListVector d r)
instance Rewrapped (ListVector d r) (ListVector d s)

instance Ixed (ListVector d r)  where
  ix i = _Wrapped.ix i



instance TraversableWithIndex Int (ListVector d) where
  itraverse f (ListVector l) = ListVector <$> itraverse f l

type instance Dimension (ListVector d r) = d
type instance NumType   (ListVector d r) = r

type instance Index (ListVector d r) = Int
type instance IxValue (ListVector d r) = r

instance HasComponents (ListVector d r) (ListVector d s) where
  components = itraversed

instance KnownNat d => Additive_ (ListVector d r) where
  zero  = pure 0
  liftU2 = liftA2

  -- (ListVector v) (ListVector w) = ListVector $ zipWith f v w
  liftI2 = liftA2 -- f (ListVector v) (ListVector w) = ListVector $ zipWith f v w

instance KnownNat d => Metric_ (ListVector d r)

instance Vector_ (ListVector d r) d r where
  vectorFromList = Just . ListVector
  -- xs = let d = natVal $ Proxy @d
  --                     in if List.genericLength xs == d then Just (ListVector xs) else Nothing


-- type family ArityX d :: Constraint where
--   ArityX 0 = ()
--   ArityX d = (KnownNat d, ArityX (d-1))

-- construct     :: forall d r. ArityX d
--               => Proxy d -> Proxy r -> ConstructVector (ListVector d r) d
-- construct d _ = go [] d
--   where
--     go       :: forall i. ArityX i
--              => [r]
--              -> Proxy i
--              -> ConstructVector (ListVector i r) i
--     go acc i = case i `cmpNat` Proxy @0 of
--                  LTI -> error "absurd"
--                  EQI -> ListVector (reverse acc)
--                  GTI -> undefined -- \x -> go (x:acc) (Proxy @(i-1))
