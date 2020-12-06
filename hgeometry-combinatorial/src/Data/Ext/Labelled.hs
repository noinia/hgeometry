{-# LANGUAGE TypeFamilyDependencies #-}

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
module Data.Ext.Labelled where

import Control.DeepSeq
import Control.Lens
import Data.Coerce
import Data.Vinyl
import GHC.Generics (Generic)
import GHC.TypeLits
import Test.QuickCheck

--------------------------------------------------------------------------------
-- * The implementation

data family (core :: *) :+ (extra :: Maybe *)

newtype instance core :+ Nothing      = Only core
  deriving (Eq,Ord,NFData,Arbitrary,Generic,Bounded,Enum)
newtype instance core :+ (Just extra) = WithExtra (core, extra)
  deriving (Eq,Ord,NFData,Generic,Arbitrary,Bounded)

instance Show core => Show (core :+ Nothing) where
  showsPrec d (Only c) = showsPrec d (c :+ ())
instance (Show core, Show extra) => Show (core :+ Just extra) where
  showsPrec d (c :+ e) = showParen (d > up_prec) $
                           showsPrec (up_prec+1) c .
                           showString " :+ "       .
                           showsPrec (up_prec+1) e
    where up_prec = 1

infixr 1 :+, :+., :+|

pattern (:+)          :: core -> extra -> core :+ Just extra
pattern core :+ extra = WithExtra (core,extra)
{-# COMPLETE (:+) #-}

type core :+. extra = core :+ Single extra
type core :+| extra = core :+ Many   extra

--------------------------------------------------------------------------------

type family Extra m where
  Extra Nothing  = ()
  Extra (Just e) = e

type family ExtraArg (t :: *) (ls :: [(Symbol,*)]) where
  ExtraArg t '[]     = t
  ExtraArg t  (l:ls) = FieldRec (l:ls)

type None     = Nothing
type Single t = Just (ExtraArg t  '[])
type Many ls  = Just (ExtraArg () ls)

--------------------------------------------------------------------------------

ext :: c -> c :+ None
ext = Only

--------------------------------------------------------------------------------

class HasCore extra where
  core :: Lens (core :+ extra) (core' :+ extra) core core'

instance HasCore Nothing where
  core = lens coerce (const coerce)

instance HasCore (Just extra) where
  core = lens (\(c :+ _) -> c) (\(_ :+ r) c -> c :+ r)

--------------------------------------------------------------------------------

class HasExtras (extras) extras' where
  extra :: Lens (core :+ extras) (core :+ extras') (Extra extras) (Extra extras')

instance HasExtras Nothing Nothing where
  extra = lens (const ()) const
instance HasExtras Nothing (Just e) where
  extra = lens (const ()) (\(Only c) e -> c :+ e)
instance HasExtras (Just e) Nothing  where
  extra = lens (\(_ :+ e) -> e) (\(c :+ _) _ -> Only c)
instance HasExtras (Just e) (Just e')  where
  extra = lens (\(_ :+ e) -> e) (\(c :+ _) e' -> c :+ e')

--------------------------------------------------------------------------------
