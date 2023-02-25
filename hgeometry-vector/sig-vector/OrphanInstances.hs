{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module OrphanInstances
  (
  ) where

import           Control.Lens
import           Control.Monad.State
import           D
import qualified Data.Functor.Apply as Apply
import           Data.Functor.Classes (readData, readUnaryWith)
import qualified Data.List as List
import           Data.Proxy
import           GHC.TypeLits (natVal)
import           Impl
import           R
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))
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

instance UniformRange R => UniformRange Vector where
  uniformRM (lows,highs) gen = Apply.unwrapApplicative $
      liftI2A (\l h -> Apply.WrapApplicative $ uniformRM (l,h) gen) lows highs

instance Uniform R => Uniform Vector where
  uniformM gen = generateA (const $ uniformM gen)
instance (Uniform R, UniformRange R) => Random Vector
