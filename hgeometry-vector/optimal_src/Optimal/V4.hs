{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Optimal.V4
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Optimal implementation using data families of 4 dimensional vectors.
--
--------------------------------------------------------------------------------
module HGeometry.Vector.Optimal.V4
  ( V4(Vector4)
  , HasV4(..)
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
data family V4 r

-- | Class specifying that we have a specialized v2 implementation
class ( Field1 (V4 r) (V4 r) r r
      , Field2 (V4 r) (V4 r) r r
      , Field3 (V4 r) (V4 r) r r
      , Field4 (V4 r) (V4 r) r r
      ) => HasV4 r where
  -- | Constructs a V4
  mkV4 :: r -> r -> r -> r -> V4 r

-- | Constant sized vector with 2 elements.
pattern Vector4         :: HasV4 r => r -> r -> r -> r -> V4 r
pattern Vector4 x y z w <- (view _1 &&& view _2 &&& view _3 &&& view _4 -> (x,(y,(z,w))) )
  where
    Vector4 x y z w = mkV4 x y z w
{-# INLINE Vector4 #-}
{-# COMPLETE Vector4 #-}

type instance Dimension (V4 r) = 4
type instance NumType (V4 r)   = r
type instance IxValue (V4 r)   = r
type instance Index   (V4 r)   = Int



--------------------------------------------------------------------------------

instance (NFData r, HasV4 r) => NFData (V4 r) where
  rnf (Vector4 x y z w) = rnf x `seq` rnf y `seq` rnf z `seq` rnf w

instance ( Field1 (V4 r) (V4 r) r r
         , Field2 (V4 r) (V4 r) r r
         , Field3 (V4 r) (V4 r) r r
         , Field4 (V4 r) (V4 r) r r
         ) => Ixed (V4 r) where
  ix i f v = case i of
               0 -> _1 f v
               1 -> _2 f v
               2 -> _3 f v
               3 -> _4 f v
               _ -> pure v
  {-# INLINE ix #-}

instance ( HasV4 r, Vector_ v 4 (NumType v)
         ) => HasComponents (V4 r) v where
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'      :: Applicative f => (r -> f (NumType v)) -> V4 r -> f v
      traverse' f v  = Vector4_ <$> f (v^._1)   <*> f   (v^._2) <*> f (v^._3) <*> f (v^._4)
      itraverse'     :: Applicative f => (Int -> r -> f (NumType v)) -> V4 r -> f v
      itraverse' f v = Vector4_ <$> f 0 (v^._1) <*> f 1 (v^._2) <*> f 2 (v^._3) <*> f 3 (v^._4)
  {-# INLINE components #-}

instance HasV4 r => Vector_ (V4 r) 4 r where
  vectorFromList = \case
    [x,y,z,w] -> Just $ mkV4 x y z w
    _         -> Nothing
  {-# INLINE vectorFromList #-}
  -- mkVector = mkV4
  -- {-# INLINE mkVector #-}

instance HasV4 r => Additive_ (V4 r) where
  zero   = mkV4 0 0 0 0
  {-# INLINE zero #-}
  liftU2 f v v' = mkV4 (f (v^._1) (v'^._1)) (f (v^._2) (v'^._2)) (f (v^._3) (v'^._3)) (f (v^._4) (v'^._4))
  {-# INLINE liftU2 #-}
  liftI2 f v v' = mkV4 (f (v^._1) (v'^._1)) (f (v^._2) (v'^._2)) (f (v^._3) (v'^._3)) (f (v^._4) (v'^._4))
  {-# INLINE liftI2 #-}

instance HasV4 r => Metric_ (V4 r)
