module HGeometry.Plane.LowerEnvelope.EpsApproximation
  ( epsilonNet
  ) where

-- import           Control.Monad.State.Class
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
-- import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
-- import           System.Random.Stateful
-- import           Witherable

--------------------------------------------------------------------------------

-- | Given a value r, and a set of planes H, we construct a 1/r-net A of H w.r.t
-- the shallow downward ranges.
epsilonNet    :: ( Plane_ plane r
                 , Ord r, Fractional r, Foldable1 f, Functor f, Ord plane
                 )
              => Int -- ^ the parameter r
             -> f plane -> NonEmpty plane
epsilonNet r = NonEmpty.fromList . NonEmpty.take size . toNonEmpty
  where
    size = r * lg r
  -- FIXME: this currently does not really construct not eps net

-- | Compute something like log base 2 (this is a bit silly)
lg   :: Int -> Int
lg n = case n `compare` 2 of
         LT -> 0
         EQ -> 1
         GT -> 1 + lg (n `div` 2)

{-

-- | Given a value r, and a set of planes H, we construct a 1/r-approximation A of H w.r.t
-- the shallow downward ranges.
epsApproximation :: ( Plane_ plane r
                    , Ord r, Fractional r, Foldable f, Functor f, Ord plane
                    )
                 => Int -- ^ the parameter r
                 -> f plane -> f plane
epsApproximation = undefined

-- | Given a value r, and a set of planes H, we construct a 1/r-approximation A of H w.r.t
-- the shallow downward ranges.
epsApproximation'      :: ( Plane_ plane r
                          , Ord r, Fractional r, Foldable f, Functor f, Ord plane
                          , RandomGen gen, MonadState gen m
                          , Show plane, Show r
                          )
                       => Int -- ^ the parameter r
                       -> f plane -> m (f plane)
epsApproximation' r hs = undefined

-}
