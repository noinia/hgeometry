{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.UnBounded
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Add an unbounded/infintity element to a data type. Essentially,
-- t'Bottom' adds \(-\infty\) (and is pretty much identical to Maybe),
-- whereas t'Top' adds \(\infty\). The 'UnBounded' type adds both.
--
--------------------------------------------------------------------------------
module Data.UnBounded( Top( ValT, Top), topToMaybe
                     , _ValT, _Top, _TopMaybe

                     , Bottom(Bottom, ValB), bottomToMaybe
                     , _ValB, _Bottom, _BottomMaybe

                     , UnBounded(..)
                     , _Val
                     , unBoundedToMaybe
                     ) where

import           Control.Lens
import qualified Data.Foldable        as F
import           Data.Functor.Classes
import qualified Data.Traversable     as T

--------------------------------------------------------------------------------
-- * Top and Bottom

-- | @Top a@ represents the type a, together with a v'Top' element, i.e. an element
-- that is greater than any other element. We can think of `Top a` being defined as:
--
-- >>> data Top a = ValT a | Top
newtype Top a = GTop (Maybe a)
                deriving (Eq,Functor,F.Foldable,T.Traversable,Applicative,Monad,Eq1
                         ,Semigroup,Monoid)




-- | @Top a@ values are isomorphing to @Maybe a@ values.
topToMaybe :: Top a -> Maybe a
topToMaybe (GTop mb) = mb

pattern ValT  :: a -> Top a
pattern ValT x = GTop (Just x)

pattern Top    :: Top a
pattern Top    = GTop Nothing

{-# COMPLETE ValT, Top #-}


instance Ord1 Top where
  liftCompare _   Top      Top       = EQ
  liftCompare _   _        Top       = LT
  liftCompare _   Top      _         = GT
  liftCompare cmp (ValT x) (ValT y) = x `cmp` y

instance Ord a => Ord (Top a) where
  compare = compare1

instance Show a => Show (Top a) where
  show Top      = "Top"
  show (ValT x) = "ValT " ++ show x

-- | 'ValT' prism. Can be used to access the non-bottom element if it exists:
--
-- >>> ValT True & _ValT %~ not
-- ValT False
--
-- >>> Top & _ValT %~ not
-- Top
_ValT :: Prism (Top a) (Top b) a b
_ValT = prism ValT (\case Top -> Left Top ; ValT x -> Right x)

-- | t'Top' prism.
_Top :: Prism' (Top a) ()
_Top = prism' (const Top) (\case Top -> Just () ; ValT _ -> Nothing)

-- | Iso between a @Top a@ and a @Maybe a@, interpreting a v'Top' as a
-- 'Nothing' and vice versa. Note that this reverses the ordering of
-- the elements.
--
-- >>> ValT 5 ^. _TopMaybe
-- Just 5
-- >>> Just 5 ^.re _TopMaybe
-- ValT 5
-- >>> Top ^. _TopMaybe
-- Nothing
-- >>> Nothing ^.re _TopMaybe
-- Top
_TopMaybe :: Iso' (Top a) (Maybe a)
_TopMaybe = iso topToMaybe GTop

--------------------------------------------------------------------------------

-- | @`Bottom a`@ represents the type a, together with a v'Bottom' element,
-- i.e. an element that is smaller than any other element. We can think of
-- @`Bottom a`@ being defined as:
--
-- >>> data Bottom a = Bottom | ValB a
newtype Bottom a = GBottom (Maybe a)
                 deriving (Eq,Ord,Functor,F.Foldable,T.Traversable,Applicative,Monad,Eq1,Ord1
                          ,Semigroup,Monoid)

-- | `Bottom a` values are isomorphing to `Maybe a` values.
bottomToMaybe :: Bottom a -> Maybe a
bottomToMaybe (GBottom mb) = mb

pattern Bottom :: Bottom a
pattern Bottom = GBottom Nothing

pattern ValB   :: a -> Bottom a
pattern ValB x = GBottom (Just x)

{-# COMPLETE Bottom, ValB #-}

instance Show a => Show (Bottom a) where
  show Bottom   = "Bottom"
  show (ValB x) = "ValB " ++ show x

-- | 'ValB' prism. Can be used to access the non-bottom element if it exists:
--
-- >>> ValB True & _ValB %~ not
-- ValB False
--
-- >>> Bottom & _ValB %~ not
-- Bottom
_ValB :: Prism (Bottom a) (Bottom b) a b
_ValB = prism ValB (\case Bottom -> Left Bottom ; ValB x -> Right x)

-- | t'Bottom' prism.
_Bottom :: Prism' (Bottom a) ()
_Bottom = prism' (const Bottom) (\case Bottom -> Just () ; ValB _ -> Nothing)

-- | Iso between a 'Bottom a' and a 'Maybe a', interpreting a Bottom as a
-- Nothing and vice versa.
--
-- >>> ValB 5 ^. _BottomMaybe
-- Just 5
-- >>> Just 5 ^.re _BottomMaybe
-- ValB 5
-- >>> Bottom ^. _BottomMaybe
-- Nothing
-- >>> Nothing ^.re _BottomMaybe
-- Bottom
_BottomMaybe :: Iso' (Bottom a) (Maybe a)
_BottomMaybe = iso bottomToMaybe GBottom

--------------------------------------------------------------------------------

-- | `UnBounded a` represents the type a, together with an element
-- `MaxInfinity` larger than any other element, and an element `MinInfinity`,
-- smaller than any other element.
data UnBounded a = MinInfinity | Val { _unUnBounded :: a }  | MaxInfinity
                 deriving (Eq,Ord,Functor,F.Foldable,T.Traversable)

-- | Prism to access unbounded value if it exists.
--
-- >>> Val True ^? _Val
-- Just True
--
-- >>> MinInfinity ^? _Val :: Maybe Bool
-- Nothing
--
-- >>> Val True & _Val %~ not
-- Val False
--
-- >>> MaxInfinity & _Val %~ not
-- MaxInfinity
_Val :: Prism (UnBounded a) (UnBounded b) a b
_Val = prism Val fromUnBounded
  where
    fromUnBounded MinInfinity = Left MinInfinity
    fromUnBounded MaxInfinity = Left MaxInfinity
    fromUnBounded (Val v)     = Right v

instance Show a => Show (UnBounded a) where
  show MinInfinity = "MinInfinity"
  show (Val x)     = "Val " ++ show x
  show MaxInfinity = "MaxInfinity"

instance Num a => Num (UnBounded a) where
  MinInfinity + _           = MinInfinity
  _           + MinInfinity = MinInfinity
  (Val x)     + (Val y)     = Val $ x + y
  _           + MaxInfinity = MaxInfinity
  MaxInfinity + _           = MaxInfinity


  MinInfinity * _           = MinInfinity
  _           * MinInfinity = MinInfinity

  (Val x)     * (Val y)     = Val $ x * y
  _           * MaxInfinity = MaxInfinity
  MaxInfinity * _           = MaxInfinity

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

-- | Test if an Unbounded is actually bounded.
--
-- >>> unBoundedToMaybe (Val 5)
-- Just 5
-- >>> unBoundedToMaybe MinInfinity
-- Nothing
-- >>> unBoundedToMaybe MaxInfinity
-- Nothing
unBoundedToMaybe         :: UnBounded a -> Maybe a
unBoundedToMaybe (Val x) = Just x
unBoundedToMaybe _       = Nothing
