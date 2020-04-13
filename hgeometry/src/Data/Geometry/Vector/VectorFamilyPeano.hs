{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector.VectorFamilyPeano where

import           Control.Applicative (liftA2)
import           Control.DeepSeq
import           Control.Lens hiding (element)
import           Data.Aeson (FromJSON(..),ToJSON(..))
-- import           Data.Aeson (ToJSON(..),FromJSON(..))
import qualified Data.Foldable as F
import qualified Data.Geometry.Vector.VectorFixed as FV
import           Data.Proxy
import qualified Data.Vector.Fixed as V
import           Data.Vector.Fixed.Cont (PeanoNum(..), Fun(..))
import           GHC.TypeLits
import           Linear.Affine (Affine(..))
import           Linear.Metric
import qualified Linear.V2 as L2
import qualified Linear.V3 as L3
import qualified Linear.V4 as L4
import           Linear.Vector
import           Data.Hashable

--------------------------------------------------------------------------------
-- * Natural number stuff

type One = S Z
type Two = S One
type Three = S Two
type Four = S Three
type Many d = S (S (S (S (S d))))


type family FromPeano (d :: PeanoNum) :: Nat where
  FromPeano Z     = 0
  FromPeano (S d) = 1 + FromPeano d


data SingPeano (d :: PeanoNum) where
  SZ :: SingPeano Z
  SS :: !(SingPeano d) -> SingPeano (S d)

class ImplicitPeano (d :: PeanoNum) where
  implicitPeano :: SingPeano d
instance ImplicitPeano Z where
  implicitPeano = SZ
instance ImplicitPeano d => ImplicitPeano (S d) where
  implicitPeano = SS implicitPeano

--------------------------------------------------------------------------------
-- * d dimensional Vectors

-- | Datatype representing d dimensional vectors. The default implementation is
-- based n VectorFixed. However, for small vectors we automatically select a
-- more efficient representation.
newtype VectorFamily (d :: PeanoNum) (r :: *) =
  VectorFamily { _unVF :: VectorFamilyF d r }

-- | Mapping between the implementation type, and the actual implementation.
type family VectorFamilyF (d :: PeanoNum) :: * -> * where
  VectorFamilyF Z        = Const ()
  VectorFamilyF One      = Identity
  VectorFamilyF Two      = L2.V2
  VectorFamilyF Three    = L3.V3
  VectorFamilyF Four     = L4.V4
  VectorFamilyF (Many d) = FV.Vector (FromPeano (Many d))

type instance V.Dim (VectorFamily d)  = FromPeano d
type instance Index   (VectorFamily d r) = Int
type instance IxValue (VectorFamily d r) = r

type instance V.Dim L2.V2 = 2
type instance V.Dim L3.V3 = 3
type instance V.Dim L4.V4 = 4

unVF :: Lens (VectorFamily  d r) (VectorFamily d t)
             (VectorFamilyF d r) (VectorFamilyF d t)
unVF = lens _unVF (const VectorFamily)
{-# INLINE unVF #-}

-- type ImplicitArity d = (ImplicitPeano d, V.Arity (FromPeano d))
class (ImplicitPeano d, V.Arity (FromPeano d)) => ImplicitArity d
instance (ImplicitPeano d, V.Arity (FromPeano d)) => ImplicitArity d

instance (Eq r, ImplicitArity d) => Eq (VectorFamily d r) where
  (VectorFamily u) == (VectorFamily v) = case (implicitPeano :: SingPeano d) of
        SZ                         -> u == v
        (SS SZ)                    -> u == v
        (SS (SS SZ))               -> u == v
        (SS (SS (SS SZ)))          -> u == v
        (SS (SS (SS (SS SZ))))     -> u == v
        (SS (SS (SS (SS (SS _))))) -> u == v
  {-# INLINE (==) #-}

instance (Ord r, ImplicitArity d) => Ord (VectorFamily d r) where
  (VectorFamily u) `compare` (VectorFamily v) = case (implicitPeano :: SingPeano d) of
        SZ                         -> u `compare` v
        (SS SZ)                    -> u `compare` v
        (SS (SS SZ))               -> u `compare` v
        (SS (SS (SS SZ)))          -> u `compare` v
        (SS (SS (SS (SS SZ))))     -> u `compare` v
        (SS (SS (SS (SS (SS _))))) -> u `compare` v
  {-# INLINE compare #-}


instance ImplicitArity d => Functor (VectorFamily d) where
  fmap f = VectorFamily . g f . _unVF
    where g = case (implicitPeano :: SingPeano d) of
                SZ                         -> fmap
                (SS SZ)                    -> fmap
                (SS (SS SZ))               -> fmap
                (SS (SS (SS SZ)))          -> fmap
                (SS (SS (SS (SS SZ))))     -> fmap
                (SS (SS (SS (SS (SS _))))) -> fmap
  {-# INLINE fmap #-}


instance ImplicitArity d => Foldable (VectorFamily d) where
  foldMap f = g f . _unVF
    where g = case (implicitPeano :: SingPeano d) of
                SZ                         -> foldMap
                (SS SZ)                    -> foldMap
                (SS (SS SZ))               -> foldMap
                (SS (SS (SS SZ)))          -> foldMap
                (SS (SS (SS (SS SZ))))     -> foldMap
                (SS (SS (SS (SS (SS _))))) -> foldMap
  {-# INLINE foldMap #-}

instance ImplicitArity d => Traversable (VectorFamily d) where
  traverse f = fmap VectorFamily . g f . _unVF
    where g = case (implicitPeano :: SingPeano d) of
                SZ                         -> traverse
                (SS SZ)                    -> traverse
                (SS (SS SZ))               -> traverse
                (SS (SS (SS SZ)))          -> traverse
                (SS (SS (SS (SS SZ))))     -> traverse
                (SS (SS (SS (SS (SS _))))) -> traverse
  {-# INLINE traverse #-}

instance ImplicitArity d => Applicative (VectorFamily d) where
  pure = VectorFamily . case (implicitPeano :: SingPeano d) of
                SZ                         -> pure
                (SS SZ)                    -> pure
                (SS (SS SZ))               -> pure
                (SS (SS (SS SZ)))          -> pure
                (SS (SS (SS (SS SZ))))     -> pure
                (SS (SS (SS (SS (SS _))))) -> pure
  {-# INLINE pure #-}
  liftA2 f (VectorFamily u) (VectorFamily v) = VectorFamily $
      case (implicitPeano :: SingPeano d) of
                SZ                         -> liftA2 f u v
                (SS SZ)                    -> liftA2 f u v
                (SS (SS SZ))               -> liftA2 f u v
                (SS (SS (SS SZ)))          -> liftA2 f u v
                (SS (SS (SS (SS SZ))))     -> liftA2 f u v
                (SS (SS (SS (SS (SS _))))) -> liftA2 f u v
  {-# INLINE liftA2 #-}


instance ImplicitArity d => V.Vector (VectorFamily d) r where
  construct = fmap VectorFamily $ case (implicitPeano :: SingPeano d) of
                SZ                         -> Fun $ Const ()
                (SS SZ)                    -> V.construct
                (SS (SS SZ))               -> Fun L2.V2
                (SS (SS (SS SZ)))          -> Fun L3.V3
                (SS (SS (SS (SS SZ))))     -> Fun L4.V4
                (SS (SS (SS (SS (SS _))))) -> V.construct
  {-# INLINE construct #-}
  inspect (VectorFamily v) ff@(Fun f) = case (implicitPeano :: SingPeano d) of
                SZ                         -> f
                (SS SZ)                    -> V.inspect v ff
                (SS (SS SZ))               -> let (L2.V2 x y) = v     in f x y
                (SS (SS (SS SZ)))          -> let (L3.V3 x y z) = v   in f x y z
                (SS (SS (SS (SS SZ))))     -> let (L4.V4 x y z w) = v in f x y z w
                (SS (SS (SS (SS (SS _))))) -> V.inspect v ff
  {-# INLINE inspect #-}
  basicIndex v i = v^.singular (element' i)
  {-# INLINE basicIndex #-}

instance (ImplicitArity d, Show r) => Show (VectorFamily d r) where
  show v = mconcat [ "Vector", show $ F.length v , " "
                   , show $ F.toList v ]

instance (NFData r, ImplicitArity d) => NFData (VectorFamily d r) where
  rnf (VectorFamily v) = case (implicitPeano :: SingPeano d) of
                           SZ                         -> rnf v
                           (SS SZ)                    -> rnf v
                           (SS (SS SZ))               -> rnf v
                           (SS (SS (SS SZ)))          -> rnf v
                           (SS (SS (SS (SS SZ))))     -> rnf v
                           (SS (SS (SS (SS (SS _))))) -> rnf v
  {-# INLINE rnf #-}


instance (ImplicitPeano d, Hashable r) => Hashable (VectorFamily d r) where
  hashWithSalt = case (implicitPeano :: SingPeano d) of
                   SZ                         -> hashWithSalt
                   (SS SZ)                    -> hashWithSalt
                   (SS (SS SZ))               -> hashWithSalt
                   (SS (SS (SS SZ)))          -> hashWithSalt
                   (SS (SS (SS (SS SZ))))     -> hashWithSalt
                   (SS (SS (SS (SS (SS _))))) -> hashWithSalt


instance ImplicitArity d => Ixed (VectorFamily d r) where
  ix = element'

element' :: forall d r. ImplicitArity d => Int -> Traversal' (VectorFamily d r) r
element' = case (implicitPeano :: SingPeano d) of
               SZ                         -> elem0
               (SS SZ)                    -> elem1
               (SS (SS SZ))               -> elem2
               (SS (SS (SS SZ)))          -> elem3
               (SS (SS (SS (SS SZ))))     -> elem4
               (SS (SS (SS (SS (SS _))))) -> elemD
{-# INLINE element' #-}

elem0   :: Int -> Traversal' (VectorFamily Z r) r
elem0 _ = \_ v -> pure v
{-# INLINE elem0 #-}
-- zero length vectors don't store any elements

elem1 :: Int -> Traversal' (VectorFamily One r) r
elem1 = \case
           0 -> unVF.(lens runIdentity (\_ -> Identity))
           _ -> \_ v -> pure v
{-# INLINE elem1 #-}

elem2 :: Int -> Traversal' (VectorFamily Two r) r
elem2 = \case
          0 -> unVF.L2._x
          1 -> unVF.L2._y
          _ -> \_ v -> pure v
{-# INLINE elem2 #-}

elem3 :: Int -> Traversal' (VectorFamily Three r) r
elem3 = \case
          0 -> unVF.L3._x
          1 -> unVF.L3._y
          2 -> unVF.L3._z
          _ -> \_ v -> pure v
{-# INLINE elem3 #-}

elem4 :: Int -> Traversal' (VectorFamily Four r) r
elem4 = \case
          0 -> unVF.L4._x
          1 -> unVF.L4._y
          2 -> unVF.L4._z
          3 -> unVF.L4._w
          _ -> \_ v -> pure v
{-# INLINE elem4 #-}

elemD   :: V.Arity (FromPeano (Many d)) => Int -> Traversal' (VectorFamily (Many d) r) r
elemD i = unVF.FV.element' i
{-# INLINE elemD #-}


instance ImplicitArity d => Metric (VectorFamily d)

instance ImplicitArity d => Additive (VectorFamily d) where
  zero = pure 0
  u ^+^ v = liftA2 (+) u v

instance ImplicitArity d => Affine (VectorFamily d) where
  type Diff (VectorFamily d) = VectorFamily d

  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v

instance (FromJSON r, ImplicitArity d)  => FromJSON (VectorFamily d r) where
  parseJSON y = parseJSON y >>= \xs -> case vectorFromList xs of
                  Nothing -> fail . mconcat $
                    [ "FromJSON (Vector d a), wrong number of elements. Expected "
                    , show $ natVal (Proxy :: Proxy (FromPeano d))
                    , " elements but found "
                    , show $ length xs
                    , "."
                    ]
                  Just v -> pure v

instance (ToJSON r, ImplicitArity d) => ToJSON (VectorFamily d r) where
  toJSON     = toJSON     . F.toList
  toEncoding = toEncoding . F.toList

--------------------------------------------------------------------------------

vectorFromList :: ImplicitArity d => [r] -> Maybe (VectorFamily d r)
vectorFromList = V.fromListM

vectorFromListUnsafe :: ImplicitArity d => [r] -> VectorFamily d r
vectorFromListUnsafe = V.fromList

-- | Get the head and tail of a vector
destruct   :: (ImplicitArity d, ImplicitArity (S d))
           => VectorFamily (S d) r -> (r, VectorFamily d r)
destruct v = (head $ F.toList v, vectorFromListUnsafe . tail $ F.toList v)
  -- FIXME: this implementaion of tail is not particularly nice

-- snoc     :: (ImplicitArity d, ImplicitArity (S d))
--          => VectorFamily d r -> r -> VectorFamily (S d) r
-- snoc = flip V.snoc
