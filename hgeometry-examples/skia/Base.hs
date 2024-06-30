{-# LANGUAGE OverloadedStrings          #-}
module Base
  ( R
  ) where

import GHC.TypeNats
import HGeometry.Miso.OrphanInstances ()
import HGeometry.Number.Real.Rational
import Miso.String (MisoString,ToMisoString(..), ms)

--------------------------------------------------------------------------------

type R = RealNumber 5

instance KnownNat p => ToMisoString (RealNumber p) where
  toMisoString = toMisoString . toFixed
