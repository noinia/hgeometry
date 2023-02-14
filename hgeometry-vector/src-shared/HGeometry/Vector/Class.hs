--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Clas
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- typeclass that expresses that we can essentially add vectors
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
module HGeometry.Vector.Class
  ( Vector
  , VectorLike_(..)
  , component, xComponent, yComponent, zComponent, wComponent
  , vectorFromList
  , Additive_(..), negated, (*^), (^*), (^/), sumV, basis, unit
  , foldMapZip
  , sameDirection
  , scalarMultiple
  , Metric_(..)
  , isScalarMultipleOf
  ) where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Foldable as F
import qualified Data.Functor.Apply as Apply
import           Data.Functor.Classes (readData, readUnaryWith)
import           Data.Kind (Type)
import qualified Data.List as List
import           Data.Proxy
import           Data.Semigroup
import           Data.Type.Ord
import           GHC.TypeLits (Nat, KnownNat, natVal)
-- import           GHC.Generics
import           HGeometry.Properties
import           Linear.V1 (V1(..))
import           Linear.V2 (V2(..))
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))
import           Text.Read (Read (..))
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))
import qualified Data.Vector.Generic as GV
import           Data.Vector.Generic.Mutable (MVector(basicInitialize))
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV

--------------------------------------------------------------------------------

-- | d-dimensional vectors
data family Vector (d :: Nat) (r :: Type) :: Type

type instance Index     (Vector d r) = Int
type instance IxValue   (Vector d r) = r
type instance Dimension (Vector d r) = d

--------------------------------------------------------------------------------

-- $setup
-- >>> import HGeometry.Vector.Unpacked
-- >>> let myVec3 = Vector3 1 10 3

-- | Types that have a 'components' indexed traversal
class VectorLike_ vector where
  {-# MINIMAL generateA, components #-}

  -- | Generates a vector from a monadic operation (that takes the index)
  generateA :: Applicative f => (Int -> f (IxValue vector)) -> f vector

  -- | An Indexed Traversal over the components of a vector
  --
  -- >>> myVec3 ^.. components
  -- [1,10,3]
  -- >>> myVec ^@.. components
  -- [(0,1),(1,10),(2,3)]
  components :: IndexedTraversal1' Int vector (IxValue vector)
  -- | Access the i^th component. Consider using 'component' instead.
  --
  -- >>> myVec ^@. unsafeComponent 0
  -- (0,1)
  -- >>> myVec & unsafeComponent 2 %@~ \i x -> 100*i + x
  -- Vector3 1 10 303
  unsafeComponent :: Int -> IndexedLens' Int vector (IxValue vector)
  default unsafeComponent :: (Index vector ~ Int, Ixed vector)
                          => Int -> IndexedLens' Int vector (IxValue vector)
  unsafeComponent i = singular $ iix i
  {-# INLINE unsafeComponent #-}

-- | Generate a vector from a given function.
generate   :: VectorLike_ vector => (Int -> IxValue vector) -> vector
generate f = runIdentity $ generateA (Identity . f)
{-# INLINE generate #-}

-- | Convert a list of exactly d elements into a vector with dimension d.
--
-- >>> vectorFromList [10,2,3] :: Maybe (Vector 3 Int)
-- Just (Vector 10 2 3)
-- >>> vectorFromList [10,2,3,5] :: Maybe (Vector 3 Int)
-- Nothing
-- >>> vectorFromList [10,2] :: Maybe (Vector 3 Int)
-- Nothing
vectorFromList :: VectorLike_ vector => [IxValue vector] -> Maybe vector
vectorFromList = evalStateT $ do v <- generateA next
                                 rest <- get
                                 guard (null rest)
                                 pure v
  where
    -- Note that this depends on the specific order in which we evaluate
    -- elements in generateA, so arguably this is somewhat dangerous.
    next   :: Int -> StateT [r] Maybe r
    next _ = get >>= \case
               []   -> fail "vectorFromList: no next element"
               x:xs -> do put xs
                          pure x
{-# INLINE vectorFromList #-}

--------------------------------------------------------------------------------
-- * Generic functions on VectorLike things

-- | Lens to access te i^t component.
--
-- >>> myVec3 ^. component @0
-- 1
-- >>> myVec3 ^. component @1
-- 2
-- >>> myVec3 & component @1 %~ (*5)
-- Vector3 1 10 3
-- >>> myVec2 & component @1 %~ (*5)
-- Vector2 10 100
component :: forall i vector. (VectorLike_ vector, i < Dimension vector, KnownNat i)
          => IndexedLens' Int vector (IxValue vector)
component = unsafeComponent (fromInteger . natVal $ Proxy @i)
{-# INLINE component #-}

-- | Shorthand for accessing the x-component
xComponent :: (VectorLike_ vector, 0 < Dimension vector)
           => IndexedLens' Int vector (IxValue vector)
xComponent = component @0
{-# INLINE xComponent #-}

-- | Shorthand for accessing the x-component
yComponent :: (VectorLike_ vector, 1 < Dimension vector)
           => IndexedLens' Int vector (IxValue vector)
yComponent = component @1
{-# INLINE yComponent #-}

-- | Shorthand for accessing the x-component
zComponent :: (VectorLike_ vector, 2 < Dimension vector)
           => IndexedLens' Int vector (IxValue vector)
zComponent = component @2
{-# INLINE zComponent #-}

-- | Shorthand for accessing the x-component
wComponent :: (VectorLike_ vector, 3 < Dimension vector)
           => IndexedLens' Int vector (IxValue vector)
wComponent = component @3
{-# INLINE wComponent #-}


--------------------------------------------------------------------------------
-- * Generic instances

instance ( VectorLike_ (Vector d r)
         , Show r
         , KnownNat d
         ) => Show (Vector d r) where
  -- | Show implementation for vectors
  showsPrec k v = showParen (k > app_prec) $
                     showString constr . showChar ' ' .
                     unwordsS (map (showsPrec 11) (v^..components))
    where
      app_prec = 10
      constr   = "Vector" <> show (fromIntegral (natVal @d Proxy))
      unwordsS = foldr (.) id . List.intersperse (showChar ' ')

instance ( VectorLike_ (Vector d r)
         , Read r
         , KnownNat d
         ) => Read (Vector d r) where
  readPrec = readData $
      readUnaryWith (replicateM d readPrec) constr $ \rs ->
        case vectorFromList rs of
          Just p -> p
          _      -> error "internal error in HGeometry.Vector read instance."
    where
      d        = fromIntegral (natVal @d Proxy)
      constr   = "Vector" <> show d


instance Additive_ (Vector d r) => Metric_ (Vector d r)

-- instance Generic (Vector d r) => NFData (Vector d r)

instance ( Additive_ (Vector d r)
         , UniformRange r
         ) => UniformRange (Vector d r) where
  uniformRM (lows,highs) gen = Apply.unwrapApplicative $
      liftI2A (\l h -> Apply.WrapApplicative $ uniformRM (l,h) gen) lows highs

instance (VectorLike_ (Vector d r), Uniform r) => Uniform (Vector d r) where
  uniformM gen = generateA (const $ uniformM gen)
instance (Additive_ (Vector d r), Uniform r, UniformRange r) => Random (Vector d r) where

--------------------------------------------------------------------------------

infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/

-- | Basically a copy of the Linear.Additive class
class VectorLike_ vector => Additive_ vector where
  {-# MINIMAL liftU2, liftI2A #-}

  -- | zero vector
  zero :: Num (IxValue vector) => vector
  zero = generate (const 0)
  {-# INLINE zero #-}

  -- | add two vectors
  (^+^) :: Num (IxValue vector) => vector -> vector -> vector
  u ^+^ v = liftU2 (+) u v
  {-# INLINE (^+^) #-}

  -- | subtract vectors
  (^-^)   :: Num (IxValue vector) => vector -> vector -> vector
  u ^-^ v = u ^+^ negated v
  {-# INLINE (^-^) #-}

  -- | Linearly interpolate between the two vectors
  lerp           :: Num (IxValue vector) => IxValue vector -> vector -> vector -> vector
  lerp alpha u v = alpha *^ u ^+^ (1-alpha) *^ v
  {-# INLINE lerp #-}

  -- | Apply a function to merge the 'non-zero' components of two
  -- vectors, unioning the rest of the values.
  liftU2       :: (IxValue vector -> IxValue vector -> IxValue vector)
               -> vector -> vector -> vector

  -- | Apply a function to the components of two vectors.
  liftI2       :: (IxValue vector -> IxValue vector -> IxValue vector)
               -> vector -> vector -> vector
  liftI2 f u v = runIdentity $ liftI2A (\x x' -> Identity $ f x x') u v
  {-# INLINE liftI2 #-}

  -- | Apply an Applicative function to the components of two vectors.
  liftI2A :: Apply.Apply f
          => (IxValue vector -> IxValue vector -> f (IxValue vector)) -> vector -> vector
          -> f vector

-- | "zip through the two vectors", folding over the result.
--
-- as an example, we can implement the dot product of two vectors u and v using:
--
-- >>> let myDot u v = getSum $ foldMapZip (\x x' -> Sum $ x * x') u v
-- >>> myDot (Vector3 1 2 3) (Vector3 10 20 30)
-- 140
foldMapZip       :: (Semigroup m, Additive_ vector)
                 => (IxValue vector -> IxValue vector -> m) -> vector -> vector -> m
foldMapZip f u v = getConst $ liftI2A (\x x' -> Const $ f x x') u v
{-# INLINE foldMapZip #-}

-- | unit vector
unit :: forall vector. (Additive_ vector, Num (IxValue vector)) => vector
unit = over components (const 1) (zero @vector)
{-# INLINE unit #-}

-- | negate v
negated :: (Num (IxValue vector), VectorLike_ vector) => vector -> vector
negated = ((-1) *^)
{-# INLINABLE negated #-}

-- | left scalar multiplication
(*^)   :: (Num (IxValue vector), VectorLike_ vector) => IxValue vector -> vector -> vector
s *^ v = over components (s*) v
{-# INLINABLE (*^) #-}

-- | right scalar multiplication
(^*)   :: (Num (IxValue vector), VectorLike_ vector)
       => vector -> IxValue vector -> vector
v ^* s = s *^ v
{-# INLINABLE (^*) #-}

-- | scalar division
(^/)   :: (VectorLike_ vector, Fractional (IxValue vector))
       => vector -> IxValue vector -> vector
v ^/ s = v ^* (1/s)
{-# INLINABLE (^/) #-}

-- | sum a collection of vectors.
sumV :: (Foldable f, Additive_ vector, Num (IxValue vector)) => f vector -> vector
sumV = F.foldl' (^+^) zero
{-# INLINABLE sumV #-}

-- | Produce a default basis for a vector space. If the dimensionality
-- of the vector space is not statically known, see 'basisFor'.
basis :: (Additive_ vector, Num (IxValue vector)) => [vector]
basis = basisFor zero
{-# INLINABLE basis #-}

-- | Produce a default basis for a vector space from which the
-- argument is drawn.
basisFor :: (Additive_ vector, Num (IxValue vector)) => vector -> [vector]
basisFor = \t ->
   ifoldMapOf components ?? t $ \i _ ->
     return                  $
       iover  components ?? t $ \j _ ->
         if i == j then 1 else 0
{-# INLINABLE basisFor #-}

-- instance Monoid c => Additive_ (Const c a) where
--   zero = Const mempty
--   liftU2 _f l _ = l
--   liftI2 _f l _ = l

--------------------------------------------------------------------------------
-- * Metric

-- | The equivalent class of Linear.Metric
--
-- Note that we do not define a distance itself, and that norm and
-- signorm have a Radical constraint rather than Floating.
class Additive_ vector => Metric_ vector where
  {-# MINIMAL #-}

  -- | Compute the inner product of two vectors or (equivalently)
  -- convert a vector f a into a covector f a -> a.
  dot :: Num (IxValue vector) => vector -> vector -> IxValue vector
  dot u v = sumOf components $ liftI2 (*) u v
  {-# INLINE dot #-}

  -- | Compute the squared norm. The name quadrance arises from Norman
  -- J. Wildberger's rational trigonometry.
  quadrance   :: Num (IxValue vector) => vector -> IxValue vector
  quadrance v = dot v v
  {-# INLINE quadrance #-}

  -- | Compute the quadrance of the difference
  qd     :: Num (IxValue vector) => vector -> vector -> IxValue vector
  qd u v = quadrance $ u ^-^ v
  {-# INLINE qd #-}

  -- -- | Compute the distance between two vectors in a metric space
  -- distance :: Radical (IxValue vector) => vector -> vector -> IxValue vector

--   -- | Compute the norm of a vector in a metric space
--   norm :: Radical (IxValue vector) => vector -> IxValue vector
--   norm = sqrt . quadrance
--   {-# INLINE norm #-}
--
--   -- | Convert a non-zero vector to unit vector.
--   signorm   :: (Radical (IxValue vector), Fractional (IxValue vector)) => vector -> vector
--   signorm v = v ^/ norm v
--   {-# INLINE signorm #-}



--------------------------------------------------------------------------------
-- * Linear implementations

instance VectorLike_ (V1 r) where
  generateA f = V1 <$> f 0
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'  :: Apply.Apply f => (r -> f r) -> V1 r -> f (V1 r)
      traverse' f (V1 x)  = V1 <$> f x
      itraverse' :: Apply.Apply f => (Int -> r -> f r) -> V1 r -> f (V1 r)
      itraverse' f (V1 x) = V1 <$> f 0 x
  {-# INLINE components #-}
  unsafeComponent i = case i of
    0 -> ilens (\(V1 x) -> (i,x)) (\(V1 _) x' -> V1 x')
    _ -> error $ "unsafeComponent: V1. index out of bounds" <> show i
  {-# INLINE unsafeComponent #-}

instance VectorLike_ (V2 r) where
  generateA f = V2 <$> f 0 <*> f 1
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'  :: Apply.Apply f => (r -> f r) -> V2 r -> f (V2 r)
      traverse' f (V2 x y)  = V2 <$> f x Apply.<.> f y
      itraverse' :: Apply.Apply f => (Int -> r -> f r) -> V2 r -> f (V2 r)
      itraverse' f (V2 x y) = V2 <$> f 0 x Apply.<.> f 1 y
  {-# INLINE components #-}
  unsafeComponent i = case i of
    0 -> ilens (\(V2 x _) -> (i,x)) (\(V2 _ y) x' -> V2 x' y )
    1 -> ilens (\(V2 _ y) -> (i,y)) (\(V2 x _) y' -> V2 x  y')
    _ -> error $ "unsafeComponent: V2. index out of bounds" <> show i
  {-# INLINE unsafeComponent #-}

instance VectorLike_ (V3 r) where
  generateA f = V3 <$> f 0 <*> f 1 <*> f 2
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'  :: Apply.Apply f => (r -> f r) -> V3 r -> f (V3 r)
      traverse' f (V3 x y z)  = V3 <$> f x Apply.<.> f y Apply.<.> f z
      itraverse' :: Apply.Apply f => (Int -> r -> f r) -> V3 r -> f (V3 r)
      itraverse' f (V3 x y z) = V3 <$> f 0 x Apply.<.> f 1 y Apply.<.> f 2 z
  {-# INLINE components #-}
  unsafeComponent i = case i of
    0 -> ilens (\(V3 x _ _) -> (i,x)) (\(V3 _ y z) x' -> V3 x' y  z )
    1 -> ilens (\(V3 _ y _) -> (i,y)) (\(V3 x _ z) y' -> V3 x  y' z )
    2 -> ilens (\(V3 _ _ z) -> (i,z)) (\(V3 x y _) z' -> V3 x  y  z')
    _ -> error $ "unsafeComponent: V3. index out of bounds" <> show i
  {-# INLINE unsafeComponent #-}

instance VectorLike_ (V4 r) where
  generateA f = V4 <$> f 0 <*> f 1 <*> f 2 <*> f 3
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'  :: Apply.Apply f => (r -> f r) -> V4 r -> f (V4 r)
      traverse' f (V4 x y z w)  = V4 <$> f x Apply.<.> f y Apply.<.> f z Apply.<.> f w
      itraverse' :: Apply.Apply f => (Int -> r -> f r) -> V4 r -> f (V4 r)
      itraverse' f (V4 x y z w) = V4 <$> f 0 x Apply.<.> f 1 y Apply.<.> f 2 z Apply.<.> f 3 w
  {-# INLINE components #-}
  unsafeComponent i = case i of
    0 -> ilens (\(V4 x _ _ _) -> (i,x)) (\(V4 _ y z w) x' -> V4 x' y  z  w)
    1 -> ilens (\(V4 _ y _ _) -> (i,y)) (\(V4 x _ z w) y' -> V4 x  y' z  w)
    2 -> ilens (\(V4 _ _ z _) -> (i,z)) (\(V4 x y _ w) z' -> V4 x  y  z' w)
    3 -> ilens (\(V4 _ _ _ w) -> (i,w)) (\(V4 x y z _) w' -> V4 x  y  z  w')
    _ -> error $ "unsafeComponent: V4. index out of bounds" <> show i
  {-# INLINE unsafeComponent #-}

instance Additive_ (V1 r) where
  zero   = V1 0
  {-# INLINE zero #-}
  liftU2 f (V1 x) (V1 x') = V1 (f x x')
  {-# INLINE liftU2 #-}
  liftI2 f (V1 x) (V1 x') = V1 (f x x')
  {-# INLINE liftI2 #-}
  liftI2A f (V1 x) (V1 x') = V1 <$> f x x'
  {-# INLINE liftI2A #-}

instance Additive_ (V2 r) where
  zero   = V2 0 0
  {-# INLINE zero #-}
  liftU2 f (V2 x y) (V2 x' y') = V2 (f x x') (f y y')
  {-# INLINE liftU2 #-}
  liftI2 f (V2 x y) (V2 x' y') = V2 (f x x') (f y y')
  {-# INLINE liftI2 #-}
  liftI2A f (V2 x y) (V2 x' y') = V2 <$> f x x' Apply.<.> f y y'
  {-# INLINE liftI2A #-}

instance Additive_ (V3 r) where
  zero   = V3 0 0 0
  {-# INLINE zero #-}
  liftU2 f (V3 x y z) (V3 x' y' z') = V3 (f x x') (f y y') (f z z')
  {-# INLINE liftU2 #-}
  liftI2 f (V3 x y z) (V3 x' y' z') = V3 (f x x') (f y y') (f z z')
  {-# INLINE liftI2 #-}
  liftI2A f (V3 x y z) (V3 x' y' z') = V3 <$> f x x' Apply.<.> f y y' Apply.<.> f z z'
  {-# INLINE liftI2A #-}

instance Additive_ (V4 r) where
  zero   = V4 0 0 0 0
  {-# INLINE zero #-}
  liftU2 f (V4 x y z w) (V4 x' y' z' w') = V4 (f x x') (f y y') (f z z') (f w w')
  {-# INLINE liftU2 #-}
  liftI2 f (V4 x y z w) (V4 x' y' z' w') = V4 (f x x') (f y y') (f z z') (f w w')
  {-# INLINE liftI2 #-}
  liftI2A f (V4 x y z w) (V4 x' y' z' w') = V4 <$> f x x' Apply.<.> f y y' Apply.<.> f z z' Apply.<.> f w w'
  {-# INLINE liftI2A #-}

instance Metric_ (V1 r)
instance Metric_ (V2 r)
instance Metric_ (V3 r)
instance Metric_ (V4 r)

--------------------------------------------------------------------------------
-- * unboxed vectors

-- | elements of the vector are stored consecutively
newtype instance UMV.MVector s (Vector d r) = MV_VectorD (UMV.MVector s r)
newtype instance UV.Vector     (Vector d r) = V_VectorD  (UV.Vector     r)

natVal' :: forall d. KnownNat d => Int
natVal' = fromInteger $ natVal (Proxy @d)

instance ( KnownNat d
         , GMV.MVector UMV.MVector r
         , VectorLike_ (Vector d r)
         ) => GMV.MVector UMV.MVector (Vector d r) where

  basicLength (MV_VectorD v) = let d = natVal' @d
                               in GMV.basicLength v `div` d
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_VectorD v) = let d = natVal' @d
                                        in MV_VectorD $ GMV.basicUnsafeSlice (d*s) (d*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_VectorD v) (MV_VectorD v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = let d = natVal' @d
                     in MV_VectorD <$> GMV.basicUnsafeNew (d*n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_VectorD v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_VectorD v) i = let d = natVal' @d
                                     in generateA $ \j -> GMV.basicUnsafeRead v (d*i+j)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_VectorD v) i w = imapMOf_ components f w
    where
      f j x = GMV.basicUnsafeWrite v (d*i+j) x
      d = natVal' @d
  {-# INLINE basicUnsafeWrite #-}


instance ( KnownNat d
         , GV.Vector UV.Vector r
         , VectorLike_ (Vector d r)
         ) => GV.Vector UV.Vector (Vector d r) where

  basicUnsafeFreeze (MV_VectorD mv) = V_VectorD <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_VectorD v) = MV_VectorD <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_VectorD v) = let d = natVal' @d
                              in GV.basicLength v `div` d
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_VectorD v) = let d = natVal' @d
                                       in V_VectorD $ GV.basicUnsafeSlice (d*s) (d*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_VectorD v) i = let d = natVal' @d
                                      in generateA $ \j -> GV.basicUnsafeIndexM v (d*i+j)
  {-# INLINE basicUnsafeIndexM #-}

instance ( KnownNat d, UV.Unbox r, VectorLike_ (Vector d r)
         ) => UV.Unbox (Vector d r) where


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * Additional Functionality
-- not sure if I should specialize these

-- | 'isScalarmultipleof u v' test if v is a scalar multiple of u.
--
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 10 10
-- True
-- >>> Vector3 1 1 2 `isScalarMultipleOf` Vector3 10 10 20
-- True
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 10 1
-- False
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 (-1) (-1)
-- True
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 11.1 11.1
-- True
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 11.1 11.2
-- False
-- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 11.1 11.2
-- False
-- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 4 2
-- True
-- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 4 0
-- False
-- >>> Vector3 2 1 0 `isScalarMultipleOf` Vector3 4 0 5
-- False
-- >>> Vector3 0 0 0 `isScalarMultipleOf` Vector3 4 0 5
-- True
isScalarMultipleOf       :: ( Eq (IxValue vector), Fractional (IxValue vector)
                            , Metric_ vector
                            )
                         => vector -> vector  -> Bool
u `isScalarMultipleOf` v = let d = u `dot` v
                               num = quadrance u * quadrance v
                           in num == 0 || num == d*d
{-# INLINE isScalarMultipleOf #-}

-- | Given two colinar vectors, u and v, test if they point in the same direction, i.e.
-- iff scalarMultiple' u v == Just lambda, with lambda > 0
--
-- pre: u and v are colinear, u and v are non-zero
sameDirection     :: ( Additive_ vector
                     , Num (IxValue vector), Eq (IxValue vector)
                     ) => vector -> vector -> Bool
sameDirection u v = getAll $ foldMapZip (\ux vx -> All $ signum ux == signum vx) u v
{-# INLINE sameDirection #-}

-- | scalarMultiple u v computes the scalar labmda s.t. v = lambda * u (if it exists)
scalarMultiple     :: (Eq (IxValue vector), Fractional (IxValue vector), Additive_ vector)
                   => vector -> vector -> Maybe (IxValue vector)
scalarMultiple u v
      | allZero u || allZero v = Just 0
      | otherwise              = scalarMultiple' u v
  where
    allZero = allOf components (== 0)
{-# INLINABLE scalarMultiple #-}

data ScalarMultiple r = No | Maybe | Yes r deriving (Eq,Show)

instance Eq r => Semigroup (ScalarMultiple r) where
  No      <> _       = No
  _       <> No      = No
  Maybe   <> x       = x
  x       <> Maybe   = x
  (Yes x) <> (Yes y)
     | x == y               = Yes x
     | otherwise            = No

instance Eq r => Monoid (ScalarMultiple r) where
  mempty = Maybe
  mappend = (<>)

-- | Actual implementation of scalarMultiple
scalarMultiple'      :: (Eq (IxValue vector), Fractional (IxValue vector), Additive_ vector)
                     => vector -> vector -> Maybe (IxValue vector)
scalarMultiple' u v = g $ foldMapZip f u v
  where
    f 0  0  = Maybe -- we don't know lambda yet, but it may still be a scalar mult.
    f _  0  = No      -- Not a scalar multiple
    f ui vi = Yes $ ui / vi -- can still be a scalar multiple

    g No      = Nothing
    g Maybe   = error "scalarMultiple': found a Maybe, which means the vectors either have length zero, or one of them is all Zero!"
    g (Yes x) = Just x
