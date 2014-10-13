{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Open where

import Data.Proxy
import Data.Vinyl

import GHC.TypeLits


data (sy :: Symbol) ::: (t :: *)

data SField tt where
  SField :: SField (sy ::: t)

type family OpenElF fld where
  OpenElF (sy ::: t) = t

newtype OpenAttr fld = OpenAttr { _unOpenAttr :: OpenElF fld }

instance (KnownSymbol sy, Show t) => Show (OpenAttr (sy ::: t)) where
  show (OpenAttr x) = symbolVal (Proxy :: Proxy sy) ++ ": " ++ show x


str :: OpenElF ("sy" ::: String)
str = "foo"

(=:) :: sing fld -> OpenElF fld -> OpenAttr fld
_ =: x = OpenAttr x


foo :: SField ("foo" ::: String)
foo = SField

bar :: Num a => SField ("bar" ::: a)
bar = SField

frank =  (foo =: "foo")
      :& (bar =: 5)
      :& RNil
