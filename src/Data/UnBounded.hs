{-# LANGUAGE TemplateHaskell   #-}
module Data.UnBounded( Top, topToMaybe
                     , pattern ValT, pattern Top

                     , Bottom, bottomToMaybe
                     , pattern Bottom, pattern ValB

                     , UnBounded(..)
                     , unUnBounded
                     , unBoundedToMaybe
                     ) where

import           Control.Applicative
import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.Traversable as T


--------------------------------------------------------------------------------
-- * Top and Bottom

-- | `Top a` represents the type a, together with a 'Top' element, i.e. an element
-- that is greater than any other element. We can think of `Top a` being defined as:
--
-- >>> data Top a = ValT a | Top
newtype Top a = GTop { topToMaybe :: Maybe a }
                deriving (Eq,Functor,F.Foldable,T.Traversable,Applicative,Monad)

pattern ValT x = GTop (Just x)
pattern Top    = GTop Nothing

instance Ord a => Ord (Top a) where
  Top      `compare` Top      = EQ
  _        `compare` Top      = LT
  Top      `compare` _        = GT
  (ValT x) `compare` (ValT y) = x `compare` y

instance Show a => Show (Top a) where
  show (ValT x) = "ValT " ++ show x
  show Top      = "Top"

--------------------------------------------------------------------------------

-- | `Bottom a` represents the type a, together with a 'Bottom' element,
-- i.e. an element that is smaller than any other element. We can think of
-- `Bottom a` being defined as:
--
-- >>> data Bottom a = ValB
newtype Bottom a = GBottom { bottomToMaybe :: Maybe a }
                 deriving (Eq,Functor,F.Foldable,T.Traversable,Applicative,Monad)

pattern Bottom = GBottom Nothing
pattern ValB x = GBottom (Just x)

instance Show a => Show (Bottom a) where
  show Bottom   = "Bottom"
  show (ValB x) = "ValB " ++ show x

--------------------------------------------------------------------------------

-- | `UnBounded a` represents the type a, together with an element
-- `MaxInfinity` larger than any other element, and an element `MinInfinity`,
-- smaller than any other element.
data UnBounded a = MinInfinity | Val { _unUnBounded :: a }  | MaxInfinity
                 deriving (Eq,Ord,Functor,F.Foldable,T.Traversable)

makeLenses ''UnBounded

instance Show a => Show (UnBounded a) where
  show MinInfinity = "MinInfinity"
  show (Val x)     = "Val " ++ show x
  show MaxInfinity = "MaxInfinity"

instance Num a => Num (UnBounded a) where
  MinInfinity + _           = MinInfinity
  (Val x)     + (Val y)     = Val $ x + y
  _           + MaxInfinity = MaxInfinity

  MinInfinity * _           = MinInfinity
  (Val x)     * (Val y)     = Val $ x * y
  _           * MaxInfinity = MaxInfinity

  abs MinInfinity = MinInfinity
  abs (Val x)     = Val $ abs x
  abs MaxInfinity = MaxInfinity

  signum MinInfinity = -1
  signum (Val x)     = Val $ signum x
  signum MaxInfinity = 1

  fromInteger = Val . fromInteger

  negate MinInfinity = MaxInfinity
  negate (Val x)     = Val $ negate x
  negate MaxInfinity = MinInfinity

instance Fractional a => Fractional (UnBounded a) where
  MinInfinity / _       = MinInfinity
  (Val x)     / (Val y) = Val $ x / y
  (Val _)     / _       = 0
  MaxInfinity / _       = MaxInfinity

  fromRational = Val . fromRational


unBoundedToMaybe         :: UnBounded a -> Maybe a
unBoundedToMaybe (Val x) = Just x
unBoundedToMaybe _       = Nothing
