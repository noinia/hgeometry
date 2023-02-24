{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module ShowRead
  (
  ) where


import           Control.Lens
import           Control.Monad.State
import           D
import qualified Data.Foldable as F
import           Data.Functor.Classes (readData, readUnaryWith)
import qualified Data.List as List
import           Data.Proxy
import           Data.Semigroup
import           Data.Type.Ord
import           GHC.TypeLits (natVal, KnownNat)
import qualified HGeometry.Number.Radical as Radical
import           Impl
import           R
import           Text.Read (Read (..))
import           Vector

--------------------------------------------------------------------------------

instance Show R => Show Vector where
  -- | Show implementation for vectors
  showsPrec k v = showParen (k > app_prec) $
                     showString constr . showChar ' ' .
                     unwordsS (map (showsPrec 11) (v^..components))
    where
      app_prec = 10
      constr   = "Vector" <> show (fromIntegral (natVal @D Proxy))
      unwordsS = foldr (.) id . List.intersperse (showChar ' ')

instance Read R => Read Vector where
  readPrec = readData $
      readUnaryWith (replicateM d readPrec) constr $ \rs ->
        case vectorFromList rs of
          Just p -> p
          _      -> error "internal error in HGeometry.Vector read instance."
    where
      d        = fromIntegral (natVal @D Proxy)
      constr   = "Vector" <> show d
