--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Helpers
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Helper functions to easily implement operations on data types that
-- can act as vectors.
--
--------------------------------------------------------------------------------
module HGeometry.Vector.Helper where

import           Control.Lens
import           Control.Monad (replicateM)
import           Data.Functor.Classes
import qualified Data.List as List
import           Data.Proxy
import           GHC.TypeLits
import           HGeometry.Vector.Class
import           Text.Read (Read (..), ReadPrec)

--------------------------------------------------------------------------------

-- | Show implementation for vectors
showsPrecVec      :: forall vector d r. (Vector_ vector d r, KnownNat d, Show r)
                  => Int -> vector -> ShowS
showsPrecVec k v = showParen (k > app_prec) $
                     showString constr . showChar ' ' .
                     unwordsS (map (showsPrec 11) (v^..components))
    where
      app_prec = 10
      constr   = "Vector" <> show (fromIntegral (natVal @d Proxy))
      unwordsS = foldr (.) id . List.intersperse (showChar ' ')

-- | Implementation for read on vectors.
readPrecVec :: forall vector d r. (Vector_ vector d r, KnownNat d, Read r)
            => ReadPrec vector
readPrecVec = readData $
      readUnaryWith (replicateM d readPrec) constr $ \rs ->
        case vectorFromList rs of
          Just p -> p
          _      -> error "internal error in HGeometry.Vector read instance."
    where
      d        = fromIntegral (natVal @d Proxy)
      constr   = "Vector" <> show d
