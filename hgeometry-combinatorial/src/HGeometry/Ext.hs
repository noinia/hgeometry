{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Ext
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A pair-like data type to represent a 'core' type that has extra information
-- as well.
--
--------------------------------------------------------------------------------
module HGeometry.Ext where

import Control.DeepSeq
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor.Apply
import Data.Bitraversable
-- import Data.Default
import Data.Functor.Apply (liftF2)
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import GHC.Generics (Generic)
-- import Test.QuickCheck
import System.Random.Stateful (Uniform(..), UniformRange(..))

--------------------------------------------------------------------------------

-- | Our Ext type that represents the core datatype core extended with extra
-- information of type 'extra'.
data core :+ extra = core :+ extra deriving (Show,Read,Eq,Ord,Bounded,Generic,NFData)
infixr 1 :+

instance Functor ((:+) c) where
  fmap f (c :+ e) = c :+ f e

instance Foldable ((:+) c) where
  foldMap f (_ :+ e) = f e

instance Traversable ((:+) c) where
  traverse f (c :+ e) = (:+) c <$> f e

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

instance Bifoldable1 (:+) where
  bifoldMap1 f g (c :+ e) = f c <> g e

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

instance (Uniform core, Uniform extra) => Uniform (core :+ extra)

instance (UniformRange core, UniformRange extra) => UniformRange (core :+ extra) where
  uniformRM (lc :+ le, hc :+ he) g = (:+) <$> uniformRM (lc, hc) g <*> uniformRM (le,he) g

-- instance (Default a, Default b) => Default (a :+ b) where
--   def = def :+ def

-- | Access the core of an extended value.
_core :: (core :+ extra) -> core
_core (c :+ _) = c
{-# INLINABLE _core #-}

-- | Access the extra part of an extended value.
_extra :: (core :+ extra) -> extra
_extra (_ :+ e) = e
{-# INLINABLE _extra #-}

-- | Lens access to the core of an extended value.
core :: Lens (core :+ extra) (core' :+ extra) core core'
core = lens _core (\(_ :+ e) c -> c :+ e)
{-# INLINABLE core #-}

-- | Lens access to the extra part of an extended value.
extra :: Lens (core :+ extra) (core :+ extra') extra extra'
extra = lens _extra (\(c :+ _) e -> c :+ e)
{-# INLINABLE extra #-}

-- | Tag a value with the unit type.
ext   :: a -> a :+ ()
ext x = x :+ ()
{-# INLINABLE ext #-}


--------------------------------------------------------------------------------

-- | A class for types that can behave as a c. Mostly for types t that
-- "extend" a 'core' type c.
class AsA t c | t -> c where
  -- | Get the core from the t.
  asCore :: t -> c

-- | infifx shorthand for AsA
type t :~ c = t `AsA` c

-- | Pattern to get the core.
pattern AsA  :: t :~ c => c -> t
pattern AsA c <- (asCore -> c)

-- | Everything can act as itself
instance (t ~ c)          => AsA t        c where
  asCore = id
  {-# INLINABLE asCore #-}

-- | An Ext can act as its core.
instance {-# OVERLAPPING #-} AsA (c :+ e) c where
  asCore = view core
  {-# INLINABLE asCore #-}

--------------------------------------------------------------------------------

-- | Types that can be decomposed into an Ext
class AsExt t where
  type CoreOf t
  type ExtraOf t
  -- | Convert between this type and an Ext
  _Ext :: Iso' t (CoreOf t :+ ExtraOf t)

instance AsExt (c :+ e) where
  type CoreOf (c :+ e) = c
  type ExtraOf (c :+ e) = e
  _Ext = iso id id
  {-# INLINE _Ext #-}

-- newtype CoreOnly core = CoreOnly core
--                       deriving newtype (Show,Read,Eq,Ord)

-- instance AsExt (CoreOnly core) where
--   type CoreOf (CoreOnly core) = core
--   type ExtraOf (CoreOnly core) = ()
--   _Ext = iso (\(CoreOnly c) -> ext c) (CoreOnly . view core)

--------------------------------------------------------------------------------
