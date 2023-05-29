module HGeometry.LowerEnvelope.DivideAndConquer
  ( lowerEnvelope
  , lowerEnvelopeWith
  ) where


-- import Control.Monad.Random
import           HGeometry.LowerEnvelope.Type
import           Witherable
import           System.Random.Stateful
import           Control.Monad.State.Class
import           Control.Lens
import           Data.Word
import qualified HGeometry.LowerEnvelope.Naive as Naive
import           HGeometry.LowerEnvelope.Sample

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | below this value we use the naive algorithm.
nZero :: Int
nZero = 10

eps :: Rational
eps = 1/8 -- TODO figure out what this had to be again.

--------------------------------------------------------------------------------

-- | divide and conquer algorithm
--
-- expected running time: \(O(n \log n)\)
lowerEnvelope    :: ( Foldable f, Ord r, Fractional r
                    , RandomGen gen, MonadState gen m
                    )
                  => f (Plane r) -> m (LowerEnvelope [] Boxed.Vector r)
lowerEnvelope hs
  | n <= nZero = pure $ Naive.lowerEnvelope hs
  | otherwise  = do ss            <- sample p hs
                    (env, prisms) <- computePrisms hs ss
                    subEnvs       <- mapM (over conflictList (SubEnv . lowerEnvelope)) prisms
                    merge env subEnvs
  where
    n = length hs
    s = n ^^^ (1-eps)
    p = probability s n

data SubEnv plane = SubEnv (LowerEnvelope [] Boxed.Vector (NumType plane))

-- | Run the divide and conquer algorithm with a given generator.
--
-- expected running time: \(O(n \log n)\)
lowerEnvelopeWith     :: ( Foldable f, Ord r, Fractional r
                         , RandomGen gen
                         )
                      => gen -> f (Plane r) -> m (LowerEnvelope [] Boxed.Vector r)
lowerEnvelopeWith gen = runStateGen gen . lowerEnvelope

--------------------------------------------------------------------------------

-- | represents the result after computing the lower envelope on the conflict lists
data SubEnv plane = SubEnv (LowerEnvelope [] Boxed.Vector (NumType plane))

--------------------------------------------------------------------------------

-- | Given the gloal envelope, and the envelopes of of the prisms,
-- merge them into one big structure
merge              :: LowerEnvelope [] Boxed.Vector r
                   -> f (Prism SubEnv r)
                   -> LowerEnvelope [] Boxed.Vector r
merge env _subEnvs = pure env -- TODO

--------------------------------------------------------------------------------
