{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.HyperPlane.Test where

import           Data.Type.Ord
import           GHC.TypeLits

--------------------------------------------------------------------------------

newtype NonVerticalHyperPlane d = NonVerticalHyperPlane ()
--------------------------------------------------------------------------------

instance ( 1 + (d-1) ~ d
         -- , 2 <= d
         ) => NonVerticalHyperPlane_ (NonVerticalHyperPlane d) d where


--------------------------------------------------------------------------------
-- | Non-vertical hyperplanes.
class NonVerticalHyperPlane_ hyperPlane d where
