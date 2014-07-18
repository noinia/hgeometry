{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vinyl.Show where

import Data.Proxy

import Data.Vinyl
import Data.Vinyl.Idiom.Identity
import Data.Vinyl.Universe.Geometry

import Data.Vinyl.Universe.Const


import GHC.TypeLits

--------------------------------------------------------------------------------

type StringRec = PlainRec (Const String)


class ShowableField (t :: *) where
  showField :: Proxy t -> String


instance Implicit (StringRec '[]) where
  implicitly = RNil

instance ( ShowableField x
         , Implicit (StringRec xs)
         ) =>
         Implicit (StringRec (x ': xs)) where
  implicitly = (Identity $ showField (Proxy :: Proxy x)) :& implicitly




instance KnownSymbol sy => ShowableField (sy :~> t) where
  showField _ = symbolVal (Proxy :: Proxy sy)

instance KnownNat n => ShowableField (DField n) where
  showField _ = "axis_" ++ (show $ natVal (Proxy :: Proxy n))
