--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Ext
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A pair-like data type to represent a 'core' type that has extra information
-- as well.
--
--------------------------------------------------------------------------------
module Data.Ext.Multi where

import Control.DeepSeq
import Control.Lens
import Data.Coerce
import Data.Vinyl
import GHC.Generics (Generic)
import Test.QuickCheck

--------------------------------------------------------------------------------

data family core :+ (extras :: [*]) :: *

newtype instance core :+ '[]    = Only core deriving (Eq,Ord,NFData,Arbitrary,Generic,Show)
data    instance core :+ (t:ts) = WithExtra !core (HList (t:ts))


infixr 1 :+

pattern (:+)   :: c -> HList (e:extras) -> c :+ (e:extras)
pattern c :+ r = WithExtra c r
{-# COMPLETE (:+) #-}

ext :: c -> c :+ '[]
ext = Only

--------------------------------------------------------------------------------

class HasCore extras where
  core :: Lens (core :+ extras) (core' :+ extras) core core'

instance HasCore '[] where
  core = lens coerce (const coerce)
instance HasCore (t:ts) where
  core = lens (\(c :+ _) -> c) (\(_ :+ r) c -> c :+ r)

--------------------------------------------------------------------------------

class HasExtras extras extras' where
  extra :: Lens (core :+ extras) (core :+ extras') (HList extras) (HList extras')

instance HasExtras '[] '[] where
  extra = lens (const RNil) const
instance HasExtras '[] (t:ts) where
  extra = lens (const RNil) (\(Only c) r -> c :+ r)
instance HasExtras (t:ts) '[] where
  extra = lens (\(_ :+ r) -> r) (\(c :+ _) _ -> Only c)
instance HasExtras (t:ts) (a:as) where
  extra = lens (\(_ :+ r) -> r) (\(c :+ _) r' -> c :+ r')

--------------------------------------------------------------------------------
