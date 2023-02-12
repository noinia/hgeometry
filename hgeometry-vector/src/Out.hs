{-# LANGUAGE UndecidableInstances #-}
module Out where

import           Control.Lens ( IxValue, conjoined, indexed, reindexed, Indexed(..)
                              , ilens, (&), (^.), (.~)
                              )
import           Data.Functor.Apply
import           GHC.Generics (Generic)
import           HGeometry.Vector.Class
import qualified In
import           R
--------------------------------------------------------------------------------

-- | Our vector type
data Vec = Cons {-# UNPACK #-}!R {-# UNPACK #-}!In.Vec
  deriving stock (Eq,Ord,Generic)

type instance IxValue Vec = R

instance (IxValue In.Vec ~ R) => VectorLike_ Vec where
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'               :: Apply f => (R -> f R) -> Vec -> f Vec
      traverse' f (Cons x r)  = Cons <$> f x <.> components f r
      itraverse'              :: Apply f => (Int -> R -> f R) -> Vec -> f Vec
      itraverse' f (Cons x r) = Cons <$> f 0 x <.> reindexed (+1) components (Indexed f) r
  {-# INLINE components #-}

  unsafeComponent i = case i of
                        0 -> ilens (\(Cons x _) -> (i, x))
                                   (\(Cons _ r) x -> Cons x r)
                        _ -> ilens (\(Cons _ r) -> (i, r^.unsafeComponent (i-1)))
                                   (\(Cons x r) y -> Cons x $ r&unsafeComponent (i-1) .~ y)
  {-# INLINE unsafeComponent  #-}
  -- not sure this will be all that efficient
