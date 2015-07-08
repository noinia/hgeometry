{-# LANGUAGE TemplateHaskell   #-}
module Data.UnBounded where

import           Control.Applicative
import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.Traversable as T


--------------------------------------------------------------------------------
-- * Top and Bottom


data Top a = Val { _unTop :: a }  | Top
           deriving (Show,Read,Eq,Ord,Functor,F.Foldable,T.Traversable)

instance Applicative Top where
  pure = Val
  (Val f) <*> (Val x) = Val $ f x
  _        <*> _        = Top

instance Monad Top where
  return = Val
  (Val m) >>= k = k m
  Top      >>= _ = Top

toMaybe         :: Top a -> Maybe a
toMaybe (Val x) = Just x
toMaybe Top     = Nothing

type Bottom a = Maybe a

data UnBounded a = BottomU | ValU { _unUnBounded :: a }  | TopU
                 deriving (Eq,Ord,Functor,F.Foldable,T.Traversable)

instance Show a => Show (UnBounded a) where
  show BottomU  = "BottomU"
  show (ValU x) = "ValU (" ++ show x ++ ")"
  show TopU     = "TopU"
makeLenses ''UnBounded

instance Num a => Num (UnBounded a) where
  BottomU  + _        = BottomU
  (ValU x) + (ValU y) = ValU $ x + y
  _        + TopU     = TopU

  BottomU  * _        = BottomU
  (ValU x) * (ValU y) = ValU $ x * y
  _        * TopU     = TopU

  abs BottomU  = BottomU
  abs (ValU x) = ValU $ abs x
  abs TopU     = TopU

  signum BottomU  = -1
  signum (ValU x) = ValU $ signum x
  signum TopU     = 1

  fromInteger = ValU . fromInteger

  negate BottomU  = TopU
  negate (ValU x) = ValU $ negate x
  negate TopU     = BottomU

instance Fractional a => Fractional (UnBounded a) where
  BottomU  / _        = BottomU
  (ValU x) / (ValU y) = ValU $ x / y
  (ValU _) / _        = 0
  TopU     / _        = TopU

  fromRational = ValU . fromRational
