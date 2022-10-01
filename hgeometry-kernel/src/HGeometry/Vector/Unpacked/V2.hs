{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Vector.Unpacked.V2
  ( Vec2(..)
  ) where

import Control.Lens
import Control.Monad.State
import Data.Aeson
import GHC.Generics (Generic)
import HGeometry.Properties
import HGeometry.Vector.Class
import System.Random (Random (..))
import System.Random.Stateful (UniformRange(..), Uniform(..), uniformListM)

import HGeometry.Point.PointF
import System.Random.Stateful

--------------------------------------------------------------------------------

type R = Int

--------------------------------------------------------------------------------

type Point2 = PointF Vec2

randomPoints   :: (Uniform point

                  ) => Int -> gen -> IO [point]
randomPoints n = replicateM n . uniformM

-- test :: IO [Point2]
-- test = getStdGen >>= uniformListM 10

--------------------------------------------------------------------------------

-- | Unboxed 2D vectors
data Vec2 = Vec2 {-# UNPACK #-} !R
                 {-# UNPACK #-} !R
            deriving (Show,Eq,Ord,Generic)

type instance Dimension Vec2 = 2

type instance NumType   Vec2 = R
type instance IxValue   Vec2 = R
type instance Index     Vec2 = Int

instance Random Vec2 where
  randomR (lows,highs) g0 = flip runState g0 $
                            zipWithM' (\l h -> state $ randomR (l,h)) lows highs
  random g0 = flip runState g0 $ replicateM' (state random)

instance UniformRange Vec2 where
  uniformRM (lows,highs) gen = zipWithM' (\l h -> uniformRM (l,h) gen) lows highs

zipWithM' :: Applicative m => (R -> R -> m R) -> Vec2 -> Vec2 -> m Vec2
zipWithM' f (Vec2 x y) (Vec2 x' y') = Vec2 <$> f x x' <*> f y y'

replicateM'   :: Applicative f => f R -> f Vec2
replicateM' x = Vec2 <$> x <*> x

instance Uniform Vec2 where
  uniformM gen = replicateM' (uniformM gen)

instance FromJSON Vec2
instance ToJSON Vec2


instance Ixed Vec2 where
  ix i f v@(Vec2 x y) = case i of
                          0 -> flip Vec2 y <$> f x
                          1 -> Vec2 x      <$> f y
                          _ -> pure v
  {-# INLINE ix #-}

instance Vector_ v 2 (NumType v) => HasComponents Vec2 v where
  {-# SPECIALIZE instance HasComponents Vec2 Vec2 #-}
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'               :: Applicative f => (R -> f (NumType v)) -> Vec2 -> f v
      traverse' f (Vec2 x y)  = Vector2_ <$> f x <*> f y
      itraverse'              :: Applicative f => (Int -> R -> f (NumType v)) -> Vec2 -> f v
      itraverse' f (Vec2 x y) = Vector2_ <$> f 0 x <*> f 1 y
  {-# INLINE components #-}


instance Vector_ Vec2 2 R where
  vectorFromList = \case
    [x,y] -> Just $ Vec2 x y
    _     -> Nothing
  {-# INLINE vectorFromList #-}

instance Additive_ Vec2 where
  zero   = Vec2 0 0
  liftU2 f (Vec2 x y) (Vec2 x' y') = Vec2 (f x x') (f y y')
  liftI2 f (Vec2 x y) (Vec2 x' y') = Vec2 (f x x') (f y y')

instance Metric_ Vec2
