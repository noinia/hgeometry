{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector.VectorFamily6 where

import           Control.Applicative (liftA2)
import           Control.DeepSeq
import           Control.Lens hiding (element)
-- import           Data.Aeson (ToJSON(..),FromJSON(..))
import qualified Data.Foldable as F
import qualified Data.Geometry.Vector.VectorFixed as FV
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Traversable (foldMapDefault,fmapDefault)
import qualified Data.Vector.Fixed as V
import           Data.Vector.Fixed.Cont (Peano(..), PeanoNum(..), Fun(..))
import           GHC.TypeLits
import           Linear.Affine (Affine(..))
import           Linear.Metric
import qualified Linear.V2 as L2
import qualified Linear.V3 as L3
import qualified Linear.V4 as L4
import           Linear.Vector

--------------------------------------------------------------------------------
-- * d dimensional Vectors


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

-- | Mapping between the implementation type, and the actual implementation.
type family VectorFamilyF (d :: PeanoNum) :: * -> * where
  VectorFamilyF Z        = Const ()
  VectorFamilyF One      = Identity
  VectorFamilyF Two      = L2.V2
  VectorFamilyF Three    = L3.V3
  VectorFamilyF Four     = L4.V4
  VectorFamilyF (Many d) = FV.Vector (FromPeano (Many d))


-- | Datatype representing d dimensional vectors. The default implementation is
-- based n VectorFixed. However, for small vectors we automatically select a
-- more efficient representation.
newtype VectorFamily (d :: PeanoNum) (r :: *) =
  VectorFamily { _unVF :: VectorFamilyF d r }

type ImplicitArity d = (ImplicitPeano d, V.Arity (FromPeano d))



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




type instance V.Dim (VectorFamily d)  = FromPeano d




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
  -- basicIndex (VectorFamily v) i = case (implicitPeano :: SingPeano d) of
  --               SZ                         -> err
  --               (SS SZ)                    -> if i == 0 then runIdentity v else err
  --               (SS (SS SZ))               -> let (L2.V2 x y) = v     in f x y
  --               (SS (SS (SS SZ)))          -> let (L3.V3 x y z) = v   in f x y z
  --               (SS (SS (SS (SS SZ))))     -> let (L4.V4 x y z w) = v in f x y z w
  --               (SS (SS (SS (SS (SS _))))) -> V.basicIndex v i
  --   where
  --     err = error "VectorFamily: basicIndex out of range"
  -- {-# INLINE basicIndex #-}


instance (ImplicitArity d, Show r) => Show (VectorFamily d r) where
  show v = mconcat [ "Vector", show $ F.length v , " "
                   , show $ F.toList v ]

deriving instance (NFData (VectorFamilyF d r)) => NFData (VectorFamily d r)


type instance Index   (VectorFamily d r) = Int
type instance IxValue (VectorFamily d r) = r

--------------------------------------------------------------------------------


newtype Vector (d :: Nat) (r :: *) = MKVector { _unV :: VectorFamily (Peano d) r }

type instance V.Dim (Vector d)  = d


type instance Index   (Vector d r) = Int
type instance IxValue (Vector d r) = r

type Arity d = ImplicitArity (Peano d)

deriving instance (Eq r,  Arity d) => Eq  (Vector d r)
deriving instance (Ord r, Arity d) => Ord (Vector d r)

deriving instance Arity d => Functor     (Vector d)
deriving instance Arity d => Foldable    (Vector d)
deriving instance Arity d => Traversable (Vector d)

instance (Arity d, Show r) => Show (Vector d r) where
  show v = mconcat [ "Vector", show $ F.length v , " "
                   , show $ F.toList v ]


deriving instance (NFData (VectorFamily (Peano d) r)) => NFData (Vector d r)




--------------------------------------------------------------------------------
-- * Convenience "constructors"

pattern Vector   :: VectorFamilyF (Peano d) r -> Vector d r
pattern Vector v = MKVector (VectorFamily v)

pattern Vector1   :: r -> Vector 1 r
pattern Vector1 x = (Vector (Identity x))

pattern Vector2     :: r -> r -> Vector 2 r
pattern Vector2 x y = (Vector (L2.V2 x y))

pattern Vector3        :: r -> r -> r -> Vector 3 r
pattern Vector3 x y z  = (Vector (L3.V3 x y z))

pattern Vector4         :: r -> r -> r -> r -> Vector 4 r
pattern Vector4 x y z w = (Vector (L4.V4 x y z w))

--------------------------------------------------------------------------------

-- -- destruct            :: (Vec d r, Vec (d + 1) r, 1 <= (d + 1))
-- --                     => Vector (d + 1) r -> (r, Vector d r)
-- -- destruct (Vector v) = (V.head v, Vector $ V.tail v)


-- -- -- vectorFromList :: Arity d => [a] -> Maybe (Vector d a)
-- -- vectorFromList = fmap Vector . V.fromListM

-- -- vectorFromListUnsafe :: V.Arity d => [a] -> Vector d a
-- -- vectorFromListUnsafe = Vector . V.fromList

 --------------------------------------------------------------------------------

-- | Cross product of two three-dimensional vectors
cross       :: Num r => Vector 3 r -> Vector 3 r -> Vector 3 r
(Vector u) `cross` (Vector v) = Vector $ u `L3.cross` v
