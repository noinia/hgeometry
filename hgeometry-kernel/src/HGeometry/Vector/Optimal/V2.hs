{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Vector.Optimal.V2
  ( V2(Vector2)
  , HasV2(..)
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
-- import qualified HGeometry.Vector.Unboxed.V2 as V2Int
import qualified Linear.V2 as L2
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))

--------------------------------------------------------------------------------

-- | Two dimensional vectors with an optimized representation
data family V2 r

-- | Class specifying that we have a specialized v2 implementation
class ( Field1 (V2 r) (V2 r) r r
      , Field2 (V2 r) (V2 r) r r
      ) => HasV2 r where
  -- | Constructs a V2
  mkV2 :: r -> r -> V2 r

-- | Constant sized vector with 2 elements.
pattern Vector2     :: HasV2 r => r -> r -> V2 r
pattern Vector2 x y <- (view _1 &&& view _2 -> (x,y))
  where
    Vector2 x y = mkV2 x y
{-# INLINE Vector2 #-}
{-# COMPLETE Vector2 #-}

type instance Dimension (V2 r) = 2
type instance NumType (V2 r)   = r
type instance IxValue (V2 r)   = r
type instance Index   (V2 r)   = Int

--------------------------------------------------------------------------------

instance (NFData r, HasV2 r) => NFData (V2 r) where
  rnf (Vector2 x y) = rnf x `seq` rnf y

instance (Random r, HasV2 r) => Random (V2 r) where
  randomR (lows,highs) g0 = flip runState g0 $
                            zipWithM' (\l h -> state $ randomR (l,h)) lows highs
  random g0 = flip runState g0 $ replicateM' (state random)

instance (UniformRange r, HasV2 r) => UniformRange (V2 r) where
  uniformRM (lows,highs) gen = zipWithM' (\l h -> uniformRM (l,h) gen) lows highs

zipWithM'                                  :: (Applicative m, HasV2 r)
                                           => (r -> r -> m r) -> V2 r -> V2 r -> m (V2 r)
zipWithM' f (Vector2 x y) (Vector2 x' y') = Vector2 <$> f x x' <*> f y y'

replicateM'   :: (Applicative f, HasV2 r) => f r -> f (V2 r)
replicateM' x = Vector2 <$> x <*> x

instance (Uniform r, HasV2 r) => Uniform (V2 r) where
  uniformM gen = replicateM' (uniformM gen)


--------------------------------------------------------------------------------

-- instance FromJSON (V2 r) where

-- instance ToJSON (V2 r) where

--------------------------------------------------------------------------------

instance ( Field1 (V2 r) (V2 r) r r
         , Field2 (V2 r) (V2 r) r r
         ) => Ixed (V2 r) where
  ix i f v = case i of
               0 -> _1 f v
               1 -> _2 f v
               _ -> pure v
  {-# INLINE ix #-}

instance ( HasV2 r, Vector_ v 2 (NumType v)
         ) => HasComponents (V2 r) v where
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'      :: Applicative f => (r -> f (NumType v)) -> V2 r -> f v
      traverse' f v  = Vector2_ <$> f (v^._1)   <*> f   (v^._2)
      itraverse'     :: Applicative f => (Int -> r -> f (NumType v)) -> V2 r -> f v
      itraverse' f v = Vector2_ <$> f 0 (v^._1) <*> f 1 (v^._2)
  {-# INLINE components #-}

instance (HasV2 r) => Vector_ (V2 r) 2 r where
  vectorFromList = \case
    [x,y] -> Just $ mkV2 x y
    _     -> Nothing
  {-# INLINE vectorFromList #-}
  -- mkVector = mkV2
  -- {-# INLINE mkVector #-}

instance HasV2 r => Additive_ (V2 r) where
  zero   = mkV2 0 0
  {-# INLINE zero #-}
  liftU2 f v v' = mkV2 (f (v^._1) (v'^._1)) (f (v^._2) (v'^._2))
  {-# INLINE liftU2 #-}
  liftI2 f v v' = mkV2 (f (v^._1) (v'^._1)) (f (v^._2) (v'^._2))
  {-# INLINE liftI2 #-}

instance HasV2 r => Metric_ (V2 r)



--------------------------------------------------------------------------------


-- | elements of the vector are stored consecutively
newtype instance UMV.MVector s (V2 r) = MV_Vec2 (UMV.MVector s r)
newtype instance UV.Vector     (V2 r) = V_Vec2  (UV.Vector     r)

instance ( HasV2 r
         , GMV.MVector UMV.MVector r
         ) => GMV.MVector UMV.MVector (V2 r) where
  basicLength (MV_Vec2 v) = GMV.basicLength v `div` 2
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_Vec2 v) = MV_Vec2 $ GMV.basicUnsafeSlice (2*s) (2*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_Vec2 v) (MV_Vec2 v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Vec2 <$> GMV.basicUnsafeNew (2*n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Vec2 v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_Vec2 v) i = do x <- GMV.basicUnsafeRead v  (2*i)
                                     y <- GMV.basicUnsafeRead v (2*i+1)
                                     pure $ Vector2 x y
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Vec2 v) i (Vector2 x y) = do GMV.basicUnsafeWrite v (2*i)   x
                                                    GMV.basicUnsafeWrite v (2*i+1) y
  {-# INLINE basicUnsafeWrite #-}


instance (HasV2 r, GV.Vector UV.Vector r)
          => GV.Vector UV.Vector (V2 r) where

  basicUnsafeFreeze (MV_Vec2 mv) = V_Vec2 <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Vec2 v) = MV_Vec2 <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Vec2 v) = GV.basicLength v `div` 2
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_Vec2 v) = V_Vec2 $ GV.basicUnsafeSlice (2*s) (2*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Vec2 v) i = Vector2 <$> GV.basicUnsafeIndexM v (2*i)
                                           <*> GV.basicUnsafeIndexM v (2*i+1)
  {-# INLINE basicUnsafeIndexM #-}

instance ( HasV2 r
         , UMV.Unbox r
         ) => UV.Unbox (V2 r)


--------------------------------------------------------------------------------

data instance V2 Int = V2Int {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                     deriving (Show,Eq,Ord,Generic)

instance Field1 (V2 Int) (V2 Int) Int Int where
  _1 = lens (\(V2Int x _) -> x) (\(V2Int _ y) x -> V2Int x y)
  {-# INLINE _1 #-}
instance Field2 (V2 Int) (V2 Int) Int Int where
  _2 = lens (\(V2Int _ y) -> y) (\(V2Int x _) y -> V2Int x y)
  {-# INLINE _2 #-}
instance HasV2 Int where
  mkV2 = V2Int
  {-# INLINE mkV2 #-}

--------------------------------------------------------------------------------

newtype Boxed r = Boxed r
  deriving newtype (Show,Eq,Ord)

newtype instance V2 (Boxed r) = V2Boxed (L2.V2 r)

instance Field1 (V2 (Boxed r)) (V2 (Boxed r)) (Boxed r) (Boxed r) where
  _1 = lens (\(V2Boxed v) -> Boxed $ view _1 v)
            (\(V2Boxed v) (Boxed x) -> V2Boxed $ set _1 x v)
  {-# INLINE _1 #-}

instance Field2 (V2 (Boxed r)) (V2 (Boxed r)) (Boxed r) (Boxed r) where
  _2 = lens (\(V2Boxed v ) -> Boxed $ view _2 v)
            (\(V2Boxed v) (Boxed x) -> V2Boxed $ set _2 x v)
  {-# INLINE _2 #-}

instance HasV2 (Boxed r) where
  mkV2 (Boxed x) (Boxed y) = V2Boxed $ L2.V2 x y
  {-# INLINE mkV2 #-}
