--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Type
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- D-dimensional Vectors
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Vector.Type
  ( Vector(.., Vector1, Vector2, Vector3, Vector4)
  , HasComponents(..)
  , cross
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Aeson
import           Data.Coerce
import qualified Data.Functor.Apply as Apply
import           Data.Kind (Type)
import qualified Data.List as List
import           Data.Proxy
import           Data.Semigroup.Foldable
import           Data.These
import qualified Data.Vector as Array
import           Data.YAML
import           Data.Zip
import           GHC.Generics (Generic)
import           GHC.TypeNats
import           HGeometry.Properties
import qualified Linear.V1 as Linear
import qualified Linear.V2 as Linear
import qualified Linear.V3 as Linear
import qualified Linear.V4 as Linear
import           Prelude hiding (zipWith)

--------------------------------------------------------------------------------

-- | Data type representing d-dimensional vectors storing elements of type r.
data family Vector (d :: Nat) (r :: Type)

type instance Dimension (Vector d r) = d
type instance NumType   (Vector d r) = r
type instance IxValue   (Vector d r) = r
type instance Index     (Vector d r) = Int

--------------------------------------------------------------------------------

{- $setup
>>> import HGeometry.Vector
>>> let myVec2 = Vector2 10 20 :: Vector 2 Int
>>> let myVec3 = Vector3 1 2 3 :: Vector 3 Int
-}

--------------------------------------------------------------------------------

-- | Types that have a 'components' indexed traversal
class HasComponents vector vector' where
  -- | An Indexed Traversal over the components of a vector
  --
  -- >>> myVec3 ^.. components
  -- [1,2,3]
  -- >>> myVec3 ^@.. components
  -- [(0,1),(1,2),(2,3)]
  components :: IndexedTraversal1 Int vector vector' (IxValue vector) (IxValue vector')

--------------------------------------------------------------------------------

newtype instance Vector 1 r = MkVector1 (Linear.V1 r)
  deriving newtype (Eq,Ord,NFData,Foldable1)
  deriving (Semialign,Zip) via Identity
  deriving stock (Generic,Functor,Foldable,Traversable)

-- | Construct a vector1
pattern Vector1 :: r -> Vector 1 r
pattern Vector1 x = MkVector1 (Linear.V1 x)
{-# COMPLETE Vector1 #-}

asV1 :: forall r s. Iso (Vector 1 r) (Vector 1 s) (Linear.V1 r) (Linear.V1 s)
asV1 = iso (coerce @(Vector 1 r) @(Linear.V1 r))
           (coerce @(Linear.V1 s) @(Vector 1 s))
{-# INLINE asV1 #-}

instance Traversable1 (Vector 1) where
  traverse1 f (Vector1 x) = Vector1 <$> f x
  {-# INLINE traverse1 #-}

instance TraversableWithIndex Int (Vector 1)
instance FoldableWithIndex    Int (Vector 1)
instance FunctorWithIndex     Int (Vector 1)

instance Ixed (Vector 1 r) where
  ix i f v@(Vector1 x) = case i of
                           0 -> Vector1 <$> f x
                           _ -> pure v
  {-# INLINE ix #-}

instance HasComponents (Vector 1 r) (Vector 1 s) where
  components = asV1 . components
  {-# INLINE components #-}


instance FromYAML r => FromYAML (Vector 1 r) where
  -- node pos -> Parser
  parseYAML node = withSeq "Vector1" f node
    where
      f = \case
        [pos] -> Vector1 <$> parseYAML pos
        _     -> failAtNode node "expected exactly 1 element"

instance FromJSON r => FromJSON (Vector 1 r) where
  parseJSON = withArray "Vector1" (f . Array.toList)
    where
      f = \case
        [x] -> Vector1 <$> parseJSON x
        _   -> fail "expected exactly 1 element"

--------------------------------------------------------------------------------

newtype instance Vector 2 r = MkVector2 (Linear.V2 r)
  deriving newtype (Eq,Ord,NFData,Foldable1)
  deriving stock (Generic,Functor,Foldable,Traversable)

-- | Construct a vector2
pattern Vector2 :: r -> r -> Vector 2 r
pattern Vector2 x y = MkVector2 (Linear.V2 x y)
{-# COMPLETE Vector2 #-}

asV2 :: forall r s. Iso (Vector 2 r) (Vector 2 s) (Linear.V2 r) (Linear.V2 s)
asV2 = iso (coerce @(Vector 2 r) @(Linear.V2 r))
           (coerce @(Linear.V2 s) @(Vector 2 s))
{-# INLINE asV2 #-}

instance Traversable1 (Vector 2) where
  traverse1 f (Vector2 x y) = Vector2 <$> f x Apply.<.> f y
  {-# INLINE traverse1 #-}

instance TraversableWithIndex Int (Vector 2)
instance FoldableWithIndex    Int (Vector 2)
instance FunctorWithIndex     Int (Vector 2)

instance Ixed (Vector 2 r) where
  ix i f v@(Vector2 x y) = case i of
                             0 -> flip Vector2 y <$> f x
                             1 -> Vector2 x      <$> f y
                             _ -> pure v
  {-# INLINE ix #-}

instance HasComponents (Vector 2 r) (Vector 2 s) where
  components = asV2 . components
  {-# INLINE components #-}

instance Semialign (Vector 2) where
  align = zipWith These
  {-# INLINE align #-}
instance Zip (Vector 2) where
  zipWith f (Vector2 x y) (Vector2 x' y') = Vector2 (f x x') (f y y')
  {-# INLINE zipWith #-}

instance FromYAML r => FromYAML (Vector 2 r) where
  parseYAML node = withSeq "Vector2" f node
    where
      f = \case
        [posX,posY] -> Vector2 <$> parseYAML posX <*> parseYAML posY
        _           -> failAtNode node "expected exactly 2 elements"

instance FromJSON r => FromJSON (Vector 2 r) where
  parseJSON = withArray "Vector2" (f . Array.toList)
    where
      f = \case
        [x,y] -> Vector2 <$> parseJSON x <*> parseJSON y
        _     -> fail "expected exactly 2 elements"

--------------------------------------------------------------------------------

newtype instance Vector 3 r = MkVector3 (Linear.V3 r)
  deriving newtype (Eq,Ord,NFData,Foldable1)
  deriving stock (Generic,Functor,Foldable,Traversable)

-- | Construct a vector3
pattern Vector3       :: r -> r -> r -> Vector 3 r
pattern Vector3 x y z = MkVector3 (Linear.V3 x y z)
{-# COMPLETE Vector3 #-}

asV3 :: forall r s. Iso (Vector 3 r) (Vector 3 s) (Linear.V3 r) (Linear.V3 s)
asV3 = iso (coerce @(Vector 3 r) @(Linear.V3 r))
           (coerce @(Linear.V3 s) @(Vector 3 s))
{-# INLINE asV3 #-}

instance Traversable1 (Vector 3) where
  traverse1 f (Vector3 x y z) = Vector3 <$> f x Apply.<.> f y Apply.<.> f z
  {-# INLINE traverse1 #-}

instance TraversableWithIndex Int (Vector 3)
instance FoldableWithIndex    Int (Vector 3)
instance FunctorWithIndex     Int (Vector 3)

instance Ixed (Vector 3 r) where
  ix i f v@(Vector3 x y z) = case i of
                               0 -> (\x' -> Vector3 x' y z) <$> f x
                               1 -> (\y' -> Vector3 x y' z) <$> f y
                               2 -> (\z' -> Vector3 x y z') <$> f z
                               _ -> pure v
  {-# INLINE ix #-}

instance HasComponents (Vector 3 r) (Vector 3 s) where
  components = asV3 . components
  {-# INLINE components #-}

instance Semialign (Vector 3) where
  align = zipWith These
  {-# INLINE align #-}
instance Zip (Vector 3) where
  zipWith f (Vector3 x y z) (Vector3 x' y' z') = Vector3 (f x x') (f y y') (f z z')
  {-# INLINE zipWith #-}

instance FromYAML r => FromYAML (Vector 3 r) where
  -- node pos -> Parser
  parseYAML node = withSeq "Vector3" f node
    where
      f = \case
        [posX,posY,posZ] -> Vector3 <$> parseYAML posX <*> parseYAML posY <*> parseYAML posZ
        _                -> failAtNode node "expected exactly 3 elements"

instance FromJSON r => FromJSON (Vector 3 r) where
  parseJSON = withArray "Vector3" (f . Array.toList)
    where
      f = \case
        [x,y,z] -> Vector3 <$> parseJSON x <*> parseJSON y <*> parseJSON z
        _       -> fail "expected exactly 3 elements"


-- | cross product
cross                            :: Num r
                                  => Vector 3 r -> Vector 3 r -> Vector 3 r
cross (MkVector3 u) (MkVector3 v) = MkVector3 $ Linear.cross u v
{-# INLINE cross #-}

--------------------------------------------------------------------------------

newtype instance Vector 4 r = MkVector4 (Linear.V4 r)
  deriving newtype (Eq,Ord,NFData,Foldable1)
  deriving stock (Generic,Functor,Foldable,Traversable)

-- | Construct a vector4
pattern Vector4         :: r -> r -> r -> r -> Vector 4 r
pattern Vector4 x y z w = MkVector4 (Linear.V4 x y z w)
{-# COMPLETE Vector4 #-}

asV4 :: forall r s. Iso (Vector 4 r) (Vector 4 s) (Linear.V4 r) (Linear.V4 s)
asV4 = iso (coerce @(Vector 4 r) @(Linear.V4 r))
           (coerce @(Linear.V4 s) @(Vector 4 s))
{-# INLINE asV4 #-}

instance Traversable1 (Vector 4) where
  traverse1 f (Vector4 x y z w) = Vector4 <$> f x Apply.<.> f y Apply.<.> f z Apply.<.> f w
  {-# INLINE traverse1 #-}

instance TraversableWithIndex Int (Vector 4)
instance FoldableWithIndex    Int (Vector 4)
instance FunctorWithIndex     Int (Vector 4)

instance Ixed (Vector 4 r) where
  ix i f v@(Vector4 x y z w) = case i of
                                 0 -> (\x' -> Vector4 x' y z w) <$> f x
                                 1 -> (\y' -> Vector4 x y' z w) <$> f y
                                 2 -> (\z' -> Vector4 x y z' w) <$> f z
                                 3 -> (\w' -> Vector4 x y z w') <$> f w
                                 _ -> pure v
  {-# INLINE ix #-}

instance HasComponents (Vector 4 r) (Vector 4 s) where
  components = asV4 . components
  {-# INLINE components #-}

instance Semialign (Vector 4) where
  align = zipWith These
  {-# INLINE align #-}
instance Zip (Vector 4) where
  zipWith f (Vector4 x y z w) (Vector4 x' y' z' w') = Vector4 (f x x') (f y y') (f z z') (f w w')
  {-# INLINE zipWith #-}

instance FromYAML r => FromYAML (Vector 4 r) where
  -- node pos -> Parser
  parseYAML node = withSeq "Vector4" f node
    where
      f = \case
        [posX,posY,posZ,posW] ->
          Vector4 <$> parseYAML posX <*> parseYAML posY <*> parseYAML posZ  <*> parseYAML posW
        _                     -> failAtNode node "expected exactly 4 elements"

instance FromJSON r => FromJSON (Vector 4 r) where
  parseJSON = withArray "Vector4" (f . Array.toList)
    where
      f = \case
        [x,y,z,w] -> Vector4 <$> parseJSON x <*> parseJSON y <*> parseJSON z <*> parseJSON w
        _         -> fail "expected exactly 4 elements"

--------------------------------------------------------------------------------
-- * Linear Instances

instance HasComponents (Linear.V1 r) (Linear.V1 s)  where
  components = conjoined traverse1 (itraverse1 . indexed)
    where
      itraverse1 :: Apply.Apply f => (Int -> r -> f s) -> Linear.V1 r -> f (Linear.V1 s)
      itraverse1 f (Linear.V1 x) = Linear.V1 <$> f 0 x
  {-# INLINE components #-}

instance HasComponents (Linear.V2 r) (Linear.V2 s)  where
  components = conjoined traverse1 (itraverse1 . indexed)
    where
      itraverse1 :: Apply.Apply f => (Int -> r -> f s) -> Linear.V2 r -> f (Linear.V2 s)
      itraverse1 f (Linear.V2 x y) =
        Linear.V2 <$> f 0 x Apply.<.> f 1 y
  {-# INLINE components #-}

instance HasComponents (Linear.V3 r) (Linear.V3 s)  where
  components = conjoined traverse1 (itraverse1 . indexed)
    where
      itraverse1 :: Apply.Apply f => (Int -> r -> f s) -> Linear.V3 r -> f (Linear.V3 s)
      itraverse1 f (Linear.V3 x y z) =
        Linear.V3 <$> f 0 x Apply.<.> f 1 y Apply.<.> f 2 z
  {-# INLINE components #-}

instance HasComponents (Linear.V4 r) (Linear.V4 s)  where
  components = conjoined traverse1 (itraverse1 . indexed)
    where
      itraverse1 :: Apply.Apply f => (Int -> r -> f s) -> Linear.V4 r -> f (Linear.V4 s)
      itraverse1 f (Linear.V4 x y z w) =
        Linear.V4 <$> f 0 x Apply.<.> f 1 y Apply.<.> f 2 z Apply.<.> f 3 w
  {-# INLINE components #-}


--------------------------------------------------------------------------------

instance ( HasComponents (Vector d r) (Vector d r)
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

--------------------------------------------------------------------------------

instance (ToYAML r, HasComponents (Vector d r) (Vector d r)) => ToYAML (Vector d r) where
  toYAML = toYAML . toListOf components

instance (ToJSON r, HasComponents (Vector d r) (Vector d r)) => ToJSON (Vector d r) where
  toJSON = toJSON . toListOf components
