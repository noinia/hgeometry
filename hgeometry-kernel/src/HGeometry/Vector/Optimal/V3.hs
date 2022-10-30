{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Vector.Optimal.V3
  ( V3(Vector3)
  , HasV3(..)
  ) where

import           Control.Arrow ((&&&))
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.State
import           Data.Aeson
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.Generics
import           HGeometry.Properties
import           HGeometry.Vector.Class
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))

--------------------------------------------------------------------------------

-- | Two dimensional vectors with an optimized representation
data family V3 r

-- | Class specifying that we have a specialized v2 implementation
class ( Field1 (V3 r) (V3 r) r r
      , Field2 (V3 r) (V3 r) r r
      , Field3 (V3 r) (V3 r) r r
      ) => HasV3 r where
  -- | Constructs a V3
  mkV3 :: r -> r -> r -> V3 r

-- | Constant sized vector with 2 elements.
pattern Vector3       :: HasV3 r => r -> r -> r -> V3 r
pattern Vector3 x y z <- (view _1 &&& view _2 &&& view _3-> (x,(y,z)) )
  where
    Vector3 x y z = mkV3 x y z
{-# INLINE Vector3 #-}
{-# COMPLETE Vector3 #-}

type instance Dimension (V3 r) = 3
type instance NumType (V3 r)   = r
type instance IxValue (V3 r)   = r
type instance Index   (V3 r)   = Int



--------------------------------------------------------------------------------

instance (NFData r, HasV3 r) => NFData (V3 r) where
  rnf (Vector3 x y z) = rnf x `seq` rnf y `seq` rnf z

instance ( Field1 (V3 r) (V3 r) r r
         , Field2 (V3 r) (V3 r) r r
         , Field3 (V3 r) (V3 r) r r
         ) => Ixed (V3 r) where
  ix i f v = case i of
               0 -> _1 f v
               1 -> _2 f v
               2 -> _3 f v
               _ -> pure v
  {-# INLINE ix #-}

instance ( HasV3 r, Vector_ v 3 (NumType v)
         ) => HasComponents (V3 r) v where
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'      :: Applicative f => (r -> f (NumType v)) -> V3 r -> f v
      traverse' f v  = Vector3_ <$> f (v^._1)   <*> f   (v^._2) <*> f (v^._3)
      itraverse'     :: Applicative f => (Int -> r -> f (NumType v)) -> V3 r -> f v
      itraverse' f v = Vector3_ <$> f 0 (v^._1) <*> f 1 (v^._2) <*> f 2 (v^._3)
  {-# INLINE components #-}

instance HasV3 r => Vector_ (V3 r) 3 r where
  vectorFromList = \case
    [x,y,z] -> Just $ mkV3 x y z
    _       -> Nothing
  {-# INLINE vectorFromList #-}
  -- mkVector = mkV3
  -- {-# INLINE mkVector #-}

instance HasV3 r => Additive_ (V3 r) where
  zero   = mkV3 0 0 0
  {-# INLINE zero #-}
  liftU2 f v v' = mkV3 (f (v^._1) (v'^._1)) (f (v^._2) (v'^._2)) (f (v^._3) (v'^._3))
  {-# INLINE liftU2 #-}
  liftI2 f v v' = mkV3 (f (v^._1) (v'^._1)) (f (v^._2) (v'^._2)) (f (v^._3) (v'^._3))
  {-# INLINE liftI2 #-}

instance HasV3 r => Metric_ (V3 r)
