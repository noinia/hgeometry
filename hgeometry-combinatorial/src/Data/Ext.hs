{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings  #-}
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
module Data.Ext where

import Control.DeepSeq
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor.Apply
import Data.Bitraversable
import Data.Functor.Apply (liftF2)
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import GHC.Generics (Generic)
import Test.QuickCheck

--------------------------------------------------------------------------------

-- | Our Ext type that represents the core datatype core extended with extra
-- information of type 'extra'.
data core :+ extra = core :+ extra deriving (Show,Read,Eq,Ord,Bounded,Generic,NFData)
infixr 1 :+


instance Bifunctor (:+) where
  bimap f g (c :+ e) = f c :+ g e

instance Biapply (:+) where
  (f :+ g) <<.>> (c :+ e) = f c :+ g e

instance Biapplicative (:+) where
  bipure = (:+)
  (f :+ g) <<*>> (c :+ e) = f c :+ g e

instance Bifoldable (:+) where
  bifoldMap f g (c :+ e) = f c `mappend` g e

instance Bitraversable (:+) where
  bitraverse f g (c :+ e) = (:+) <$> f c <*> g e

instance Bifoldable1 (:+)

instance Bitraversable1 (:+) where
  bitraverse1 f g (c :+ e) = liftF2 (:+) (f c) (g e)

instance (Semigroup core, Semigroup extra) => Semigroup (core :+ extra) where
  (c :+ e) <> (c' :+ e') = c <> c' :+ e <> e'


instance (ToJSON core, ToJSON extra) => ToJSON (core :+ extra) where
  -- toJSON     (c :+ e) = toJSON     (c,e)
  -- toEncoding (c :+ e) = toEncoding (c,e)
  toJSON     (c :+ e) = object ["core" .= c, "extra" .= e]
  toEncoding (c :+ e) = pairs  ("core" .= c <> "extra" .= e)

instance (FromJSON core, FromJSON extra) => FromJSON (core :+ extra) where
  -- parseJSON = fmap (\(c,e) -> c :+ e) . parseJSON
  parseJSON (Object v) = (:+) <$> v .: "core" <*> v .: "extra"
  parseJSON invalid    = typeMismatch "Ext (:+)" invalid

instance (Arbitrary c, Arbitrary e) => Arbitrary (c :+ e) where
  arbitrary = (:+) <$> arbitrary <*> arbitrary

_core :: (core :+ extra) -> core
_core (c :+ _) = c
{-# INLINABLE _core #-}

_extra :: (core :+ extra) -> extra
_extra (_ :+ e) = e
{-# INLINABLE _extra #-}

core :: Lens (core :+ extra) (core' :+ extra) core core'
core = lens _core (\(_ :+ e) c -> (c :+ e))
{-# INLINABLE core #-}

extra :: Lens (core :+ extra) (core :+ extra') extra extra'
extra = lens _extra (\(c :+ _) e -> (c :+ e))
{-# INLINABLE extra #-}

ext   :: a -> a :+ ()
ext x = x :+ ()
{-# INLINABLE ext #-}
