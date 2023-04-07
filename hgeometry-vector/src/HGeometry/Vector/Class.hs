{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Class defining d-dimensional vectors
--
--------------------------------------------------------------------------------
module HGeometry.Vector.Class
  ( AsVector_(..)
  , Vector_(..) --, pattern Vector1_, pattern Vector2_, pattern Vector3_, pattern Vector4_
  , _Vector
  , Has_
  , generate, vectorFromList
  , component
  , xComponent, yComponent, zComponent, wComponent
  -- , HasComponents(..), components1

  -- , ConstructVector, ConstructableVector_(..)

  -- , vectorFromVector
  , prefix, suffix
  , cons, snoc
  , uncons, unsnoc
  -- , vZipWith

  , Additive_(..), zero, liftI2, lerp, (^+^), (^-^)
  , negated, (*^), (^*), (^/), sumV, basis, unit
  , foldMapZip
  , Metric_(..)
  -- , VectorFor
  ) where

-- import           Control.Arrow ((&&&))
import           Control.Lens hiding (cons,snoc,uncons,unsnoc)
import           Control.Monad.State
import           Control.Monad (guard, replicateM)
import           Data.Coerce
import qualified Data.Foldable as F
import qualified Data.Functor.Apply as Apply
import           Data.Functor.Classes (readData, readUnaryWith)
-- import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Type.Ord
import           GHC.TypeNats
import           HGeometry.Properties
import           HGeometry.Vector.Type
import           Text.Read (Read (..))
-- import           HGeometry.Vector.Additive
-- import           HGeometry.Vector.Metric
import qualified Linear.V1 as Linear
import qualified Linear.V2 as Linear
import qualified Linear.V3 as Linear
import qualified Linear.V4 as Linear
import           Prelude hiding (zipWith)
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))
import qualified HGeometry.Number.Radical as Radical

--------------------------------------------------------------------------------


{- $setup
>>> import HGeometry.Vector
>>> import Data.Semigroup
>>> let myVec2 = Vector2 10 20 :: Vector 2 Int
>>> let myVec3 = Vector3 1 2 3 :: Vector 3 Int
-}

--------------------------------------------------------------------------------

class ( r ~ IxValue vector
      , d ~ Dimension vector
      ) => AsVector_ vector d r | vector -> d
                                , vector -> r  where
  -- | Convert into a 'Vector d r'.
  _Vector' :: Iso' vector (Vector d r)
  default _Vector' :: ( Vector_ (Vector d r) d r
                     , Vector_ vector       d r
                     ) => Iso' vector (Vector d r)
  _Vector' = _Vector
  {-# INLINE _Vector' #-}

_Vector :: ( Vector_ (Vector d r) d r
           , Vector_ vector       d r
           , Vector_ vector'      d s
           , Vector_ (Vector d s) d s
           ) => Iso vector vector' (Vector d r) (Vector d s)
_Vector = iso (\v -> generate (\i -> v^?!component' i))
              (\v -> generate (\i -> v^?!component' i))
{-# INLINE _Vector #-}


instance AsVector_ (Vector d r) d r where
  _Vector' = id
  {-# INLINE _Vector' #-}

class ( HasComponents vector vector
      , AsVector_ vector d r
      , KnownNat d
      ) => Vector_ vector d r where
  {-# MINIMAL generateA #-}

  -- | Generates a vector from an Applicative operation (that takes the
  -- index)
  generateA :: Applicative f => (Int -> f r) -> f vector

  -- | traversal to access the i^th coordinate.
  component' :: Int -> IndexedTraversal' Int vector r
  default component' :: (Index vector ~ Int, Ixed vector)
                          => Int -> IndexedTraversal' Int vector r
  component' = iix
  {-# INLINE component' #-}

-- | Specifies that we have an appropriate constraint for the vector implementation
type Has_ c d r = c (Vector d r) d r

----------------------------------------

-- | Generate a vector from a given function.
generate   :: Vector_ vector d r => (Int -> r) -> vector
generate f = runIdentity $ generateA (Identity . f)
{-# INLINE generate #-}

-- | Convert a list of exactly d elements into a vector with dimension d.
--
-- >>> vectorFromList [10,2,3] :: Maybe (Vector 3 Int)
-- Just (Vector3 10 2 3)
-- >>> vectorFromList [10,2,3,5] :: Maybe (Vector 3 Int)
-- Nothing
-- >>> vectorFromList [10,2] :: Maybe (Vector 3 Int)
-- Nothing
vectorFromList :: Vector_ vector d r => [r] -> Maybe vector
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

-- -- | Construct a vector from a list of exactly d components. Crashes
-- -- when we get the wrong number of components.
-- uncheckedVectorFromList :: Vector_ vector d r => [r] -> vector
-- uncheckedVectorFromList = fromMaybe (error "uncheckedVectorFromList") . vectorFromList
-- {-# INLINE uncheckedVectorFromList #-}

----------------------------------------

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
component :: forall i vector r d.
             (i <= d-1, KnownNat i, Vector_ vector d r)
          => IndexedLens' Int vector r
component = singular $ component' (fromIntegral . natVal $ Proxy @i)
{-# INLINE component #-}

-- | Shorthand for accessing the x-component
--
-- >>> Vector3 1 2 3 ^. xComponent
-- 1
-- >>> Vector2 1 2 & xComponent .~ 10
-- Vector2 10 2
xComponent :: (Vector_ vector d r, 1 <= d)
           => IndexedLens' Int vector r
xComponent = component @0
{-# INLINE xComponent #-}

-- | Shorthand for accessing the y-component
--
-- >>> Vector3 1 2 3 ^. yComponent
-- 2
-- >>> Vector2 1 2 & yComponent .~ 10
-- Vector2 1 10
yComponent :: (Vector_ vector d r, 2 <= d)
           => IndexedLens' Int vector r
yComponent = component @1
{-# INLINE yComponent #-}

-- | Shorthand for accessing the z-component
--
-- >>> Vector3 1 2 3 ^. zComponent
-- 3
-- >>> Vector3 1 2 3 & zComponent .~ 10
-- Vector3 1 2 10
zComponent :: (Vector_ vector d r, 3 <= d)
           => IndexedLens' Int vector r
zComponent = component @2
{-# INLINE zComponent #-}

-- | Shorthand for accessing the w-component
--
-- >>> Vector4 1 2 3 4 ^. wComponent
-- 4
-- >>> Vector4 1 2 3 4 & wComponent .~ 10
-- Vector4 1 2 3 10
wComponent :: (Vector_ vector d r, 4 <= d)
           => IndexedLens' Int vector r
wComponent = component @3
{-# INLINE wComponent #-}

--------------------------------------------------------------------------------

-- | Take a prefix of length i of the vector
--
-- >>> prefix myVec3 :: Vector 2 Int
-- Vector2 1 2
prefix   :: forall i d vector vector' r. ( i <= d
                                         , Vector_ vector  d r
                                         , Vector_ vector' i r
                                         )
         => vector -> vector'
prefix v = generate (\i -> v^?!component' i)
{-# INLINE prefix #-}

-- | Take a suffix of length i  of the vector
--
-- >>> suffix @_ @_ @_ @(Vector 2 Int) myVec3
-- Vector2 2 3
suffix   :: forall i d vector vector' r. ( i <= d
                                       , Vector_ vector  d r
                                       , Vector_ vector' i r
                                       )
         => vector -> vector'
suffix v = let d = fromIntegral . natVal $ Proxy @d
               s = d - (fromIntegral . natVal $ Proxy @i)
           in generate $ (\j -> v^?!component' (s+j))
{-# INLINE suffix #-}

--------------------------------------------------------------------------------

class Vector_ vector d r => Additive_ vector d r where
  -- | Apply a function to merge the 'non-zero' components of two
  -- vectors, unioning the rest of the values.
  liftU2 :: (r -> r -> r) -> vector -> vector -> vector

  -- | Apply an Applicative function to the components of two vectors.
  liftI2A :: Apply.Apply f => (r -> r -> f r) -> vector -> vector -> f vector

----------------------------------------
infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/

-- | zero vector
zero :: (Num r, Additive_ vector d r) => vector
zero = generate (const 0)
{-# INLINE zero #-}

-- | add two vectors
(^+^)   :: (Num r, Additive_ vector d r) => vector -> vector -> vector
u ^+^ v = liftU2 (+) u v
{-# INLINE (^+^) #-}

-- | subtract vectors
(^-^)   :: (Num r, Additive_ vector d r) => vector -> vector -> vector
u ^-^ v = u ^+^ negated v
{-# INLINE (^-^) #-}

-- | Linearly interpolate between the two vectors
lerp           :: (Num r, Additive_ vector d r) => r -> vector -> vector -> vector
lerp alpha u v = alpha *^ u ^+^ (1-alpha) *^ v
{-# INLINE lerp #-}

-- | Apply a function to the components of two vectors.
liftI2       :: Additive_ vector d r => (r -> r -> r) -> vector -> vector -> vector
liftI2 f u v = runIdentity $ liftI2A (\x x' -> Identity $ f x x') u v
{-# INLINE liftI2 #-}

-- | "zip through the two vectors", folding over the result.
--
-- as an example, we can implement the dot product of two vectors u and v using:
--
-- >>> let myDot u v = getSum $ foldMapZip (\x x' -> Sum $ x * x') u v
-- >>> myDot (Vector3 1 2 3) (Vector3 10 20 30)
-- 140
foldMapZip       :: (Additive_ vector d r, Semigroup m)
                 => (r -> r -> m) -> vector -> vector -> m
foldMapZip f u v = getConst $ liftI2A (\x x' -> Const $ f x x') u v
{-# INLINE foldMapZip #-}

-- | unit vector
unit :: forall vector d r. (Additive_ vector d r, Num r) => vector
unit = over (components @vector @vector) (const 1) zero
{-# INLINE unit #-}

-- | negate v
negated :: (Num r, Vector_ vector d r) => vector -> vector
negated = ((-1) *^)
{-# INLINABLE negated #-}

-- | left scalar multiplication
(*^)   :: (Num r, Vector_ vector d r) => r -> vector -> vector
s *^ v = over components (s*) v
{-# INLINABLE (*^) #-}

-- | right scalar multiplication
(^*)   :: (Num r, Vector_ vector d r) => vector -> r -> vector
v ^* s = s *^ v
{-# INLINABLE (^*) #-}

-- | scalar division
(^/)   :: (Vector_ vector d r, Fractional r) => vector -> r -> vector
v ^/ s = v ^* (1/s)
{-# INLINABLE (^/) #-}

-- | sum a collection of vectors.
sumV :: (Foldable f, Additive_ vector d r, Num r) => f vector -> vector
sumV = F.foldl' (^+^) zero
{-# INLINABLE sumV #-}

-- | Produce a default basis for a vector space. If the dimensionality
-- of the vector space is not statically known, see 'basisFor'.
basis :: (Additive_ vector d r, Num r) => [vector]
basis = basisFor zero
{-# INLINABLE basis #-}

-- | Produce a default basis for a vector space from which the
-- argument is drawn.
basisFor :: (Additive_ vector d r, Num r) => vector -> [vector]
basisFor = \t ->
   ifoldMapOf components ?? t $ \i _ ->
     return                  $
       iover  components ?? t $ \j _ ->
         if i == j then 1 else 0
{-# INLINABLE basisFor #-}


----------------------------------------
-- * Metric

-- | The equivalent class of Linear.Metric
--
-- Note that we do not define a distance itself, and that norm and
-- signorm have a Radical constraint rather than Floating.
class Additive_ vector d r => Metric_ vector d r where
  {-# MINIMAL #-}

  -- | Compute the inner product of two vectors or (equivalently)
  -- convert a vector f a into a covector f a -> a.
  dot    :: Num r => vector -> vector -> r
  dot u v = sumOf components $ liftI2 (*) u v
  {-# INLINE dot #-}

  -- | Compute the squared norm. The name quadrance arises from Norman
  -- J. Wildberger's rational trigonometry.
  quadrance   :: Num r => vector -> r
  quadrance v = dot v v
  {-# INLINE quadrance #-}

  -- | Compute the quadrance of the difference
  qd     :: Num r => vector -> vector -> r
  qd u v = quadrance $ u ^-^ v
  {-# INLINE qd #-}

  -- -- | Compute the distance between two vectors in a metric space
  -- distance :: Radical r => vector -> vector -> IxValue vector

  -- | Compute the norm of a vector in a metric space
  norm :: Radical.Radical r => vector -> r
  norm = Radical.sqrt . quadrance
  {-# INLINE norm #-}

  -- | Convert a non-zero vector to unit vector.
  signorm   :: ( Radical.Radical r
               , Fractional r
               ) => vector -> vector
  signorm v = v ^/ norm v
  {-# INLINE signorm #-}

--------------------------------------------------------------------------------

instance ( Additive_ (Vector d r) d r
         , UniformRange r
         ) => UniformRange (Vector d r) where
  uniformRM (lows,highs) gen = Apply.unwrapApplicative $
      liftI2A (\l h -> Apply.WrapApplicative $ uniformRM (l,h) gen) lows highs

instance (Vector_ (Vector d r) d r, Uniform r) => Uniform (Vector d r) where
  uniformM gen = generateA (const $ uniformM gen)
instance (Additive_ (Vector d r) d r, Uniform r, UniformRange r) => Random (Vector d r)

--------------------------------------------------------------------------------
-- * instances for Linear

----------------------------------------

type instance Dimension (Linear.V1 r) = 1

instance AsVector_ (Linear.V1 r) 1 r where
  _Vector' = iso (coerce @(Linear.V1 r) @(Vector 1 r))
                 (coerce @(Vector 1 r) @(Linear.V1 r) )
  {-# INLINE _Vector' #-}

instance Vector_ (Linear.V1 r) 1 r where
  generateA f = Linear.V1 <$> f 0
  {-# INLINE generateA #-}
  component' i f v@(Linear.V1 x) = case i of
                                     0 -> Linear.V1 <$> indexed f i x
                                     _ -> pure v
  {-# INLINE component' #-}

instance Additive_ (Linear.V1 r) 1 r where
  liftU2 f (Linear.V1 x) (Linear.V1 x') = Linear.V1 $ f x x'
  {-# INLINE liftU2 #-}
  liftI2A f (Linear.V1 x) (Linear.V1 x') = Linear.V1 <$> f x x'
  {-# INLINE liftI2A #-}

instance Metric_ (Linear.V1 r) 1 r


----------------------------------------

----------------------------------------

type instance Dimension (Linear.V2 r) = 2

instance AsVector_ (Linear.V2 r) 2 r where
  _Vector' = iso (coerce @(Linear.V2 r) @(Vector 2 r))
                 (coerce @(Vector 2 r) @(Linear.V2 r) )
  {-# INLINE _Vector' #-}

instance Vector_ (Linear.V2 r) 2 r where
  generateA f = Linear.V2 <$> f 0 <*> f 1
  {-# INLINE generateA #-}
  component' i f v@(Linear.V2 x y) = case i of
                                       0 -> flip Linear.V2 y <$> indexed f i x
                                       1 -> Linear.V2 x      <$> indexed f i y
                                       _ -> pure v
  {-# INLINE component' #-}

instance Additive_ (Linear.V2 r) 2 r where
  liftU2 f (Linear.V2 x y) (Linear.V2 x' y') = Linear.V2 (f x x') (f y y')
  {-# INLINE liftU2 #-}
  liftI2A f (Linear.V2 x y) (Linear.V2 x' y') = Linear.V2 <$> f x x' Apply.<.> f y y'
  {-# INLINE liftI2A #-}

instance Metric_ (Linear.V2 r) 2 r


--------------------------------------------------------------------------------

type instance Dimension (Linear.V3 r) = 3

instance AsVector_ (Linear.V3 r) 3 r where
  _Vector' = iso (coerce @(Linear.V3 r) @(Vector 3 r))
                 (coerce @(Vector 3 r) @(Linear.V3 r) )
  {-# INLINE _Vector' #-}

instance Vector_ (Linear.V3 r) 3 r where
  generateA f = Linear.V3 <$> f 0 <*> f 1 <*> f 2
  {-# INLINE generateA #-}
  component' i f v@(Linear.V3 x y z) = case i of
                                         0 -> (\x' -> Linear.V3 x' y z) <$> indexed f i x
                                         1 -> (\y' -> Linear.V3 x y' z) <$> indexed f i y
                                         2 -> (\z' -> Linear.V3 x y z') <$> indexed f i z
                                         _ -> pure v
  {-# INLINE component' #-}

instance Additive_ (Linear.V3 r) 3 r where
  liftU2 f (Linear.V3 x y z) (Linear.V3 x' y' z') = Linear.V3 (f x x') (f y y') (f z z')
  {-# INLINE liftU2 #-}
  liftI2A f (Linear.V3 x y z) (Linear.V3 x' y' z') =
    Linear.V3 <$> f x x' Apply.<.> f y y' Apply.<.> f z z'
  {-# INLINE liftI2A #-}

instance Metric_ (Linear.V3 r) 3 r

--------------------------------------------------------------------------------

type instance Dimension (Linear.V4 r) = 4

instance AsVector_ (Linear.V4 r) 4 r where
  _Vector' = iso (coerce @(Linear.V4 r) @(Vector 4 r))
                 (coerce @(Vector 4 r) @(Linear.V4 r) )
  {-# INLINE _Vector' #-}

instance Vector_ (Linear.V4 r) 4 r where
  generateA f = Linear.V4 <$> f 0 <*> f 1 <*> f 2 <*> f 3
  {-# INLINE generateA #-}
  component' i f v@(Linear.V4 x y z w) = case i of
                                           0 -> (\x' -> Linear.V4 x' y z w) <$> indexed f i x
                                           1 -> (\y' -> Linear.V4 x y' z w) <$> indexed f i y
                                           2 -> (\z' -> Linear.V4 x y z' w) <$> indexed f i z
                                           3 -> (\w' -> Linear.V4 x y z w') <$> indexed f i w
                                           _ -> pure v
  {-# INLINE component' #-}

instance Additive_ (Linear.V4 r) 4 r where
  liftU2 f (Linear.V4 x y z w) (Linear.V4 x' y' z' w') =
    Linear.V4 (f x x') (f y y') (f z z') (f w w')
  {-# INLINE liftU2 #-}
  liftI2A f (Linear.V4 x y z w) (Linear.V4 x' y' z' w') =
    Linear.V4 <$> f x x' Apply.<.> f y y' Apply.<.> f z z' Apply.<.> f w w'
  {-# INLINE liftI2A #-}

instance Metric_ (Linear.V4 r) 4 r

--------------------------------------------------------------------------------

instance Vector_ (Vector 1 r) 1 r  where
  generateA f = Vector1 <$> f 0
  {-# INLINE generateA #-}
instance Vector_ (Vector 2 r) 2 r  where
  generateA f = Vector2 <$> f 0 <*> f 1
  {-# INLINE generateA #-}
instance Vector_ (Vector 3 r) 3 r  where
  generateA f = Vector3 <$> f 0 <*> f 1 <*> f 2
  {-# INLINE generateA #-}
instance Vector_ (Vector 4 r) 4 r  where
  generateA f = Vector4 <$> f 0 <*> f 1 <*> f 2 <*> f 3
  {-# INLINE generateA #-}

instance Additive_ (Vector 1 r) 1 r where
  liftU2 f (Vector1 x) (Vector1 x') = Vector1 $ f x x'
  {-# INLINE liftU2 #-}
  liftI2A f (Vector1 x) (Vector1 x') = Vector1 <$> f x x'
  {-# INLINE liftI2A #-}

instance Additive_ (Vector 2 r) 2 r where
  liftU2 f (Vector2 x y) (Vector2 x' y') = Vector2 (f x x') (f y y')
  {-# INLINE liftU2 #-}
  liftI2A f (Vector2 x y) (Vector2 x' y') = Vector2 <$> f x x' Apply.<.> f y y'
  {-# INLINE liftI2A #-}

instance Additive_ (Vector 3 r) 3 r where
  liftU2 f (Vector3 x y z) (Vector3 x' y' z') = Vector3 (f x x') (f y y') (f z z')
  {-# INLINE liftU2 #-}
  liftI2A f (Vector3 x y z) (Vector3 x' y' z') =
    Vector3 <$> f x x' Apply.<.> f y y' Apply.<.> f z z'
  {-# INLINE liftI2A #-}

instance Additive_ (Vector 4 r) 4 r where
  liftU2 f (Vector4 x y z w) (Vector4 x' y' z' w') =
    Vector4 (f x x') (f y y') (f z z') (f w w')
  {-# INLINE liftU2 #-}
  liftI2A f (Vector4 x y z w) (Vector4 x' y' z' w') =
    Vector4 <$> f x x' Apply.<.> f y y' Apply.<.> f z z' Apply.<.> f w w'
  {-# INLINE liftI2A #-}

instance Metric_ (Vector 1 r) 1 r
instance Metric_ (Vector 2 r) 2 r
instance Metric_ (Vector 3 r) 3 r
instance Metric_ (Vector 4 r) 4 r

--------------------------------------------------------------------------------

instance ( Vector_ (Vector d r) d r
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

--------------------------------------------------------------------------------

-- | Add an element to the front of the vector
--
-- >>> cons 5 myVec2 :: Vector 3 Int
-- Vector3 5 10 20
cons     :: forall vector' vector d r.
            (Vector_ vector d r, Vector_ vector' (d+1) r)
         => r -> vector -> vector'
cons x v = generate $ \case
                        0 -> x
                        i -> v^?!component' (i-1)
{-# INLINE cons #-}

-- | Add an element to the back of the vector.
--
-- >>> snoc myVec2 5 :: Vector 3 Int
-- Vector3 10 20 5
snoc     :: forall vector' vector d r.
            ( Vector_ vector d r, Vector_ vector' (d+1) r)
         => vector -> r -> vector'
snoc v x = let d = fromIntegral . natVal $ Proxy @d
           in generate $ \i -> if i == d then x else v^?!component' i
{-# INLINE snoc #-}

-- | Extract the first element from the vector
--
-- >>> uncons myVec3 :: (Int, Vector 2 Int)
-- (1,Vector2 2 3)
uncons   :: forall vector' vector d r.
            ( Vector_ vector (d+1) r, Vector_ vector' d r
            , 0 <= (d+1)-1, d <= Dimension vector -- these ones are silly
            ) => vector -> (r, vector')
uncons v = ( v^.component @0, suffix v)
{-# INLINE uncons #-}

-- | Extract the last element from the vector
--
-- >>> unsnoc myVec3  :: (Vector 2 Int, Int)
-- (Vector2 1 2,3)
unsnoc   :: forall vector' vector d r.
            (Vector_ vector (d+1) r, Vector_ vector' d r
            , d <= d+1-1, d <= Dimension vector -- these are silly
            ) => vector -> (vector',r)
unsnoc v = ( prefix v, v^.component @d )
{-# INLINE unsnoc #-}

{-
-- | Type family that expresses that we can construct a d-dimensional
-- vector from an arity d function.
type ConstructVector :: Type -> Nat -> Type
type family ConstructVector vector d where
  ConstructVector vector 0 = vector
  ConstructVector vector d = IxValue vector -> ConstructVector vector (d-1)


  {-# MINIMAL vectorFromList #-}

-- | Vectors that we can construct using an arity d function
class Vector_ vector d r => ConstructableVector_ vector d r where
  -- | Construct a vector from a d-arity function.
  --
  mkVector :: ConstructVector vector d


--------------------------------------------------------------------------------
-- instances for Linear

type instance Dimension (LinearV1.V1 r) = 1
instance Vector_ (LinearV1.V1 r) 1 r where
  componentProxy i = indexing $ case (fromIntegral . natVal $ i) of
                       0  -> _1
                       i' -> error $ "componentProxy: " <> show i' <> " out of bounds"
  {-# INLINE componentProxy #-}
  vectorFromList = \case
    [x] -> Just $ LinearV1.V1 x
    _   -> Nothing
  {-# INLINE vectorFromList #-}

instance ConstructableVector_ (LinearV1.V1 r) 1 r where
  mkVector = LinearV1.V1

type instance Dimension (LinearV2.V2 r) = 2
instance Vector_ (LinearV2.V2 r) 2 r where
  componentProxy i = indexing $ case (fromIntegral . natVal $ i) of
                       0  -> _1
                       1  -> _2
                       i' -> error $ "componentProxy: " <> show i' <> " out of bounds"
  {-# INLINE componentProxy #-}
  vectorFromList = \case
    [x,y] -> Just $ LinearV2.V2 x y
    _     -> Nothing
  {-# INLINE vectorFromList #-}

instance ConstructableVector_ (LinearV2.V2 r) 2 r where
  mkVector = LinearV2.V2

type instance Dimension (LinearV3.V3 r) = 3
instance Vector_ (LinearV3.V3 r) 3 r where
  componentProxy i = indexing $ case (fromIntegral . natVal $ i) of
                       0  -> _1
                       1  -> _2
                       2  -> _3
                       i' -> error $ "componentProxy: " <> show i' <> " out of bounds"
  {-# INLINE componentProxy #-}
  vectorFromList = \case
    [x,y,z] -> Just $ LinearV3.V3 x y z
    _       -> Nothing
  {-# INLINE vectorFromList #-}

instance ConstructableVector_ (LinearV3.V3 r) 3 r where
  mkVector = LinearV3.V3

type instance Dimension (LinearV4.V4 r) = 4
instance Vector_ (LinearV4.V4 r) 4 r where
  componentProxy i = indexing $ case (fromIntegral . natVal $ i) of
                       0  -> _1
                       1  -> _2
                       2  -> _3
                       3  -> _4
                       i' -> error $ "componentProxy: " <> show i' <> " out of bounds"
  {-# INLINE componentProxy #-}
  vectorFromList = \case
    [x,y,z,w] -> Just $ LinearV4.V4 x y z w
    _         -> Nothing
  {-# INLINE vectorFromList #-}

instance ConstructableVector_ (LinearV4.V4 r) 4 r where
  mkVector = LinearV4.V4

-}

--------------------------------------------------------------------------------

-- -- | A bidirectional pattern synonym for 1 dimensional vectors.
-- pattern Vector1_   :: Vector_ vector 1 r => r -> vector
-- pattern Vector1_ x <- (view (component @0) -> x)
--   where
--     Vector1_ x = generate (const x)
-- {-# COMPLETE Vector1_ #-}
-- {-# INLINE Vector1_ #-}

-- -- | A bidirectional pattern synonym for 2 dimensional vectors.
-- pattern Vector2_     :: Vector_ vector 2 r => r -> r -> vector
-- pattern Vector2_ x y <- (view (component @0) &&& view (component @1) -> (x,y))
--   where
--     Vector2_ x y = uncheckedVectorFromList [x,y]
-- {-# COMPLETE Vector2_ #-}
-- {-# INLINE Vector2_ #-}


-- -- | A bidirectional pattern synonym for 3 dimensional vectors.
-- pattern Vector3_       :: Vector_ vector 3 r => r -> r -> r -> vector
-- pattern Vector3_ x y z <- (view (component @0) &&& view (component @1) &&& view (component @2)
--                           -> (x,(y,z)))
--   where
--     Vector3_ x y z = uncheckedVectorFromList [x,y,z]
-- {-# COMPLETE Vector3_ #-}

-- -- | A bidirectional pattern synonym for 4 dimensional vectors.
-- pattern Vector4_         :: Vector_ vector 4 r => r -> r -> r -> r -> vector
-- pattern Vector4_ x y z w <- (    view (component @0) &&& view (component @1)
--                              &&& view (component @2) &&& view (component @3)
--                             -> (x,(y,(z,w))))
--   where
--     Vector4_ x y z w = uncheckedVectorFromList [x,y,z,w]
-- {-# COMPLETE Vector4_ #-}
