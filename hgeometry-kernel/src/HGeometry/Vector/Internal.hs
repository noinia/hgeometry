{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Vector.Internal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional vectors.
--
--------------------------------------------------------------------------------
module HGeometry.Vector.Internal(
    module HGeometry.Vector.VectorFamily
                    -- reexports
  , FV.replicate
  ) where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Vector.Fixed as FV
import qualified Data.Vector.Fixed as V
import           HGeometry.Properties
import           HGeometry.Vector.Class
import           HGeometry.Vector.VectorFamily
import qualified Linear.Vector as Linear
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))
-- import           Test.QuickCheck (Arbitrary (..), Arbitrary1 (..), infiniteList,
--                                    infiniteListOf)

--------------------------------------------------------------------------------

-- $setup
-- >>> import Control.Lens

type instance Dimension (Vector d r) = d
type instance NumType   (Vector d r) = r


instance Arity d => HasComponents (Vector d r) (Vector d s) where
  components = itraversed

instance Arity d => Additive_ (Vector d r) where
  zero = pure 0
  liftU2 = Linear.liftU2
  liftI2 = Linear.liftI2

instance Arity d => Metric_ (Vector d r)

instance Arity d => Vector_ (Vector d r) d r where
  vectorFromList = V.fromListM



-- instance (Arbitrary r, Arity d) => Arbitrary (Vector d r) where
--   arbitrary = vectorFromListUnsafe <$> infiniteList

-- instance (Arity d) => Arbitrary1 (Vector d) where
--   liftArbitrary gen = vectorFromListUnsafe <$> infiniteListOf gen

instance (Random r, Arity d) => Random (Vector d r) where
  randomR (lows,highs) g0 = flip runState g0 $
                            FV.zipWithM (\l h -> state $ randomR (l,h)) lows highs
  random g0 = flip runState g0 $ FV.replicateM (state random)

instance (UniformRange r, Arity d) => UniformRange (Vector d r) where
  uniformRM (lows,highs) gen = FV.zipWithM (\l h -> uniformRM (l,h) gen) lows highs

instance (Uniform r, Arity d) => Uniform (Vector d r) where
  uniformM gen = FV.replicateM (uniformM gen)

instance (Bounded r, Arity d) => Bounded (Vector d r) where
  minBound = pure minBound
  maxBound = pure maxBound



-- -- | Helper function for computing the scalar multiple. The result is a pair
-- -- (b,mm), where b indicates if v is a scalar multiple of u, and mm is a Maybe
-- -- scalar multiple. If the result is Nothing, the scalar multiple is zero.
-- scalarMultiple'     :: (Eq r, Fractional r, GV.Arity d)
--                     => Vector d r -> Vector d r -> (Bool,Maybe r)
-- scalarMultiple' u v = F.foldr allLambda (True,Nothing) $ FV.zipWith f u v
--   where
--     f ui vi = (ui == 0 && vi == 0, ui / vi)
--     allLambda (True,_)      x               = x
--     allLambda (_, myLambda) (b,Nothing)     = (b,Just myLambda) -- no lambda yet
--     allLambda (_, myLambda) (b,Just lambda) = (myLambda == lambda && b, Just lambda)
