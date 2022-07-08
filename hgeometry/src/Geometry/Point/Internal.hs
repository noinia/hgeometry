{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Point
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional points.
--
--------------------------------------------------------------------------------
module Geometry.Point.Internal
  ( Point(..)
  , vector
  , fromGenericPoint, toGenericPoint
  -- , pattern Point1
  -- , pattern Point2
  -- , pattern Point3
  -- , pattern Point4
  , PointFunctor(..)
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.Foldable as F
import           Data.Functor.Classes
import           Data.Hashable
import           Data.List (intersperse)
import           Data.Proxy
import           GHC.Generics (Generic)
import           GHC.TypeLits
import qualified Geometry.Point.Class as PointClass
import           Geometry.Point.EuclideanDistance
import           Geometry.Properties
import           Geometry.Vector
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))
import           Test.QuickCheck (Arbitrary, Arbitrary1)
import           Text.Read (Read (..), readListPrecDefault)


--------------------------------------------------------------------------------
-- $setup
-- >>> :{
-- let myVector :: Vector 3 Int
--     myVector = Vector3 1 2 3
--     myPoint = Point myVector
-- :}


--------------------------------------------------------------------------------
-- * A d-dimensional Point

-- | A d-dimensional point.
--
-- There are convenience pattern synonyms for 1, 2 and 3 dimensional points.
--
-- >>> let f (Point1 x) = x in f (Point1 1)
-- 1
-- >>> let f (Point2 x y) = x in f (Point2 1 2)
-- 1
-- >>> let f (Point3 x y z) = z in f (Point3 1 2 3)
-- 3
-- >>> let f (Point3 x y z) = z in f (Point $ Vector3 1 2 3)
-- 3
newtype Point d r = Point { toVec :: Vector d r }
  deriving (Generic)


instance (Show r, Arity d) => Show (Point d r) where
  showsPrec = liftShowsPrec showsPrec showList

instance (Arity d) => Show1 (Point d) where
  liftShowsPrec sp _ d (Point v) = showParen (d > 10) $
      showString constr . showChar ' ' .
      unwordsS (map (sp 11) (F.toList v))
    where
      constr = "Point" <> show (fromIntegral (natVal @d Proxy))
      unwordsS = foldr (.) id . intersperse (showChar ' ')

instance (Read r, Arity d) => Read (Point d r) where
  readPrec     = liftReadPrec readPrec readListPrec
  readListPrec = readListPrecDefault

instance (Arity d) => Read1 (Point d) where
  liftReadPrec rp _rl = readData $
      readUnaryWith (replicateM d rp) constr $ \rs ->
        case PointClass.pointFromList rs of
          Just p -> p
          _      -> error "internal error in Geometry.Point read instance."
    where
      d = fromIntegral (natVal (Proxy :: Proxy d))
      constr = "Point" <> show d
  liftReadListPrec = liftReadListPrecDefault

-- readPt :: forall d r. (Arity d, Read r) => ReadP (Point d r)
-- readPt = do let d = natVal (Proxy :: Proxy d)
--             _  <- string $ "Point" <> show d
--             rs <- if d > 3
--               then readPrec_to_P readPrec minPrec
--               else replicateM (fromIntegral d) (readPrec_to_P readPrec minPrec)
--             case pointFromList rs of
--               Just p -> pure p
--               _      -> pfail

deriving instance (Eq r, Arity d)           => Eq (Point d r)
deriving instance Arity d                   => Eq1 (Point d)
deriving instance (Ord r, Arity d)          => Ord (Point d r)
deriving instance Arity d                   => Functor (Point d)
deriving instance Arity d                   => Applicative (Point d)
deriving instance Arity d                   => Foldable (Point d)
deriving instance Arity d                   => Traversable (Point d)
deriving instance (Arity d, NFData r)       => NFData (Point d r)
deriving instance (Arity d, Arbitrary r)    => Arbitrary (Point d r)
deriving instance Arity d                   => Arbitrary1 (Point d)
deriving instance (Arity d, Hashable r)     => Hashable (Point d r)
deriving instance (Arity d, Random r)       => Random (Point d r)
deriving instance (Bounded r, Arity d)      => Bounded (Point d r)

instance Arity d => FunctorWithIndex Int (Point d) where
  imap = iover PointClass.coordinates
instance Arity d => FoldableWithIndex Int (Point d) where
  ifoldMap = ifoldMapOf PointClass.coordinates
instance Arity d => TraversableWithIndex Int (Point d) where
  itraverse = itraverseOf PointClass.coordinates

instance (Arity d, UniformRange r) => UniformRange (Point d r) where
  uniformRM (Point lows, Point highs) gen = Point <$> uniformRM (lows,highs) gen

instance (Arity d, Uniform r) => Uniform (Point d r) where
  uniformM gen = Point <$> uniformM gen

type instance NumType (Point d r) = r
type instance Dimension (Point d r) = d

instance Arity d =>  Affine (Point d) where
  type Diff (Point d) = Vector d

  p .-. q = toVec p ^-^ toVec q
  p .+^ v = Point $ toVec p ^+^ v

instance Arity d => PointClass.Point_ Point d r where
  fromVector = Point
  asVector = vector

instance (FromJSON r, Arity d, KnownNat d) => FromJSON (Point d r) where
  parseJSON = fmap Point . parseJSON

instance (ToJSON r, Arity d) => ToJSON (Point d r) where
  toJSON     = toJSON     . toVec
  toEncoding = toEncoding . toVec

instance (Num r, Arity d) => HasSquaredEuclideanDistance (Point d r) where
  pointClosestTo _ p = PointClass.fromVector . view PointClass.asVector $ p

-- -- | Point representing the origin in d dimensions
-- --
-- -- >>> origin :: Point 4 Int
-- -- Point4 0 0 0 0
-- origin :: (Arity d, Num r) => Point d r
-- origin = Point $ pure 0

-- ** Accessing points

-- | Lens to access the vector corresponding to this point.
--
-- >>> (Point3 1 2 3) ^. vector
-- Vector3 1 2 3
-- >>> origin & vector .~ Vector3 1 2 3
-- Point3 1 2 3
vector :: Lens (Point d r) (Point d r') (Vector d r) (Vector d r')
vector = lens toVec (const Point)
{-# INLINABLE vector #-}

-- -- | Get the coordinate in a given dimension. This operation is unsafe in the
-- -- sense that no bounds are checked. Consider using `coord` instead.
-- --
-- --
-- -- >>> Point3 1 2 3 ^. unsafeCoord 2
-- -- 2
-- unsafeCoord   :: Arity d => Int -> Lens' (Point d r) r
-- unsafeCoord i = vector . singular (ix (i-1))
--                 -- Points are 1 indexed, vectors are 0 indexed
-- {-# INLINABLE unsafeCoord #-}

-- -- | Get the coordinate in a given dimension
-- --
-- -- >>> Point3 1 2 3 ^. coord @2
-- -- 2
-- -- >>> Point3 1 2 3 & coord @1 .~ 10
-- -- Point3 10 2 3
-- -- >>> Point3 1 2 3 & coord @3 %~ (+1)
-- -- Point3 1 2 4
-- coord :: forall i d r. (1 <= i, i <= d, Arity d, KnownNat i)
--       => Lens' (Point d r) r
-- coord = unsafeCoord $ fromIntegral (natVal $ C @i)
-- {-# INLINABLE coord #-}

--  -- somehow these rules don't fire
-- -- {-# SPECIALIZE coord :: C 1 -> Lens' (Point 2 r) r#-}
-- -- {-# SPECIALIZE coord :: C 2 -> Lens' (Point 2 r) r#-}
-- -- {-# SPECIALIZE coord :: C 3 -> Lens' (Point 3 r) r#-}


-- -- | Constructs a point from a list of coordinates. The length of the
-- -- list has to match the dimension exactly.
-- --
-- -- >>> pointFromList [1,2,3] :: Maybe (Point 3 Int)
-- -- Just (Point3 1 2 3)
-- -- >>> pointFromList [1] :: Maybe (Point 3 Int)
-- -- Nothing
-- -- >>> pointFromList [1,2,3,4] :: Maybe (Point 3 Int)
-- -- Nothing
-- pointFromList :: Arity d => [r] -> Maybe (Point d r)
-- pointFromList = fmap Point . Vec.vectorFromList


-- -- | Project a point down into a lower dimension.
-- projectPoint :: (Arity i, Arity d, i <= d) => Point d r -> Point i r
-- projectPoint = Point . prefix . toVec

-- --------------------------------------------------------------------------------
-- -- * Convenience functions to construct 1, 2 and 3 dimensional points

-- -- | A bidirectional pattern synonym for 1 dimensional points.
-- pattern Point1   :: r -> Point 1 r
-- pattern Point1 x = Point (Vector1 x)
-- {-# COMPLETE Point1 #-}


-- -- | A bidirectional pattern synonym for 2 dimensional points.
-- pattern Point2       :: r -> r -> Point 2 r
-- pattern Point2 x y = Point (Vector2 x y)
-- {-# COMPLETE Point2 #-}

-- -- | A bidirectional pattern synonym for 3 dimensional points.
-- pattern Point3       :: r -> r -> r -> Point 3 r
-- pattern Point3 x y z = (Point (Vector3 x y z))
-- {-# COMPLETE Point3 #-}

-- -- | A bidirectional pattern synonym for 4 dimensional points.
-- pattern Point4         :: r -> r -> r -> r -> Point 4 r
-- pattern Point4 x y z w = (Point (Vector4 x y z w))
-- {-# COMPLETE Point4 #-}

--------------------------------------------------------------------------------
-- * Point Functors

-- | Types that we can transform by mapping a function on each point in the structure
class PointFunctor g where
  pmap :: (Point (Dimension (g r)) r -> Point (Dimension (g s)) s) -> g r -> g s

instance PointFunctor (Point d) where
  pmap f = f


-- | Convert a generic point into this specific point.
fromGenericPoint :: PointClass.Point_ point d r => point d r -> Point d r
fromGenericPoint = PointClass.fromVector . view PointClass.asVector

-- | Convert this point into a generic point.
toGenericPoint :: PointClass.Point_ point d r => Point d r -> point d r
toGenericPoint = PointClass.fromVector . view PointClass.asVector
