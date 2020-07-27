{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Point
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional points.
--
--------------------------------------------------------------------------------
module Data.Geometry.Point.Internal
  ( Point(..)
  , origin, vector
  , pointFromList

  , coord , unsafeCoord

  , projectPoint

  , pattern Point1
  , pattern Point2
  , pattern Point3
  , PointFunctor(..)

  , cmpByDistanceTo
  , squaredEuclideanDist, euclideanDist
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Aeson
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import qualified Data.Geometry.Vector as Vec
import           Data.Hashable
import           Data.Ord (comparing)
import           Data.Proxy
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           System.Random (Random(..))
import           Test.QuickCheck (Arbitrary)
import           Text.ParserCombinators.ReadP (ReadP, string,pfail)
import           Text.ParserCombinators.ReadPrec (lift)
import           Text.Read (Read(..),readListPrecDefault, readPrec_to_P,minPrec)


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
newtype Point d r = Point { toVec :: Vector d r } deriving (Generic)

instance (Show r, Arity d) => Show (Point d r) where
  show (Point v) = mconcat [ "Point", show $ F.length v , " "
                           , show $ F.toList v
                           ]
instance (Read r, Arity d) => Read (Point d r) where
  readPrec     = lift readPt
  readListPrec = readListPrecDefault

readPt :: forall d r. (Arity d, Read r) => ReadP (Point d r)
readPt = do let d = natVal (Proxy :: Proxy d)
            _  <- string $ "Point" <> show d <> " "
            rs <- readPrec_to_P readPrec minPrec
            case pointFromList rs of
              Just p -> pure p
              _      -> pfail

deriving instance (Eq r, Arity d)        => Eq (Point d r)
deriving instance (Ord r, Arity d)       => Ord (Point d r)
deriving instance Arity d                => Functor (Point d)
deriving instance Arity d                => Foldable (Point d)
deriving instance Arity d                => Traversable (Point d)
deriving instance (Arity d, NFData r)    => NFData (Point d r)
deriving instance (Arity d, Arbitrary r) => Arbitrary (Point d r)
deriving instance (Arity d, Hashable r)  => Hashable (Point d r)
deriving instance (Arity d, Random r)    => Random (Point d r)


type instance NumType (Point d r) = r
type instance Dimension (Point d r) = d

instance Arity d =>  Affine (Point d) where
  type Diff (Point d) = Vector d

  p .-. q = toVec p ^-^ toVec q
  p .+^ v = Point $ toVec p ^+^ v

instance (FromJSON r, Arity d, KnownNat d) => FromJSON (Point d r) where
  parseJSON = fmap Point . parseJSON

instance (ToJSON r, Arity d) => ToJSON (Point d r) where
  toJSON     = toJSON     . toVec
  toEncoding = toEncoding . toVec

-- | Point representing the origin in d dimensions
--
-- >>> origin :: Point 4 Int
-- Point4 [0,0,0,0]
origin :: (Arity d, Num r) => Point d r
origin = Point $ pure 0


-- ** Accessing points

-- | Lens to access the vector corresponding to this point.
--
-- >>> (Point3 1 2 3) ^. vector
-- Vector3 [1,2,3]
-- >>> origin & vector .~ Vector3 1 2 3
-- Point3 [1,2,3]
vector :: Lens' (Point d r) (Vector d r)
vector = lens toVec (const Point)
{-# INLINABLE vector #-}

-- | Get the coordinate in a given dimension. This operation is unsafe in the
-- sense that no bounds are checked. Consider using `coord` instead.
--
--
-- >>> Point3 1 2 3 ^. unsafeCoord 2
-- 2
unsafeCoord   :: Arity d => Int -> Lens' (Point d r) r
unsafeCoord i = vector . singular (ix (i-1))
                -- Points are 1 indexed, vectors are 0 indexed
{-# INLINABLE unsafeCoord #-}

-- | Get the coordinate in a given dimension
--
-- >>> Point3 1 2 3 ^. coord (C :: C 2)
-- 2
-- >>> Point3 1 2 3 & coord (C :: C 1) .~ 10
-- Point3 [10,2,3]
-- >>> Point3 1 2 3 & coord (C :: C 3) %~ (+1)
-- Point3 [1,2,4]
coord   :: forall proxy i d r. (1 <= i, i <= d, Arity d, KnownNat i)
        => proxy i -> Lens' (Point d r) r
coord _ = unsafeCoord $ fromIntegral (natVal $ C @i)
{-# INLINABLE coord #-}

 -- somehow these rules don't fire
-- {-# SPECIALIZE coord :: C 1 -> Lens' (Point 2 r) r#-}
-- {-# SPECIALIZE coord :: C 2 -> Lens' (Point 2 r) r#-}
-- {-# SPECIALIZE coord :: C 3 -> Lens' (Point 3 r) r#-}


-- | Constructs a point from a list of coordinates. The length of the
-- list has to match the dimension exactly.
--
-- >>> pointFromList [1,2,3] :: Maybe (Point 3 Int)
-- Just Point3 [1,2,3]
-- >>> pointFromList [1] :: Maybe (Point 3 Int)
-- Nothing
-- >>> pointFromList [1,2,3,4] :: Maybe (Point 3 Int)
-- Nothing
pointFromList :: Arity d => [r] -> Maybe (Point d r)
pointFromList = fmap Point . Vec.vectorFromList


-- | Project a point down into a lower dimension.
projectPoint :: (Arity i, Arity d, i <= d) => Point d r -> Point i r
projectPoint = Point . prefix . toVec

--------------------------------------------------------------------------------
-- * Convenience functions to construct 1, 2 and 3 dimensional points

-- | We provide pattern synonyms for 1, 2 and 3 dimensional points. i.e.
-- we can write:
--
--
-- >>> :{
--   let
--     f            :: Num r => Point 1 r -> r
--     f (Point1 x) = x + 1
--   in f (Point1 1)
-- :}
-- 2
pattern Point1   :: r -> Point 1 r
pattern Point1 x = Point (Vector1 x)
{-# COMPLETE Point1 #-}


-- | Pattern synonym for 2 dimensional points
--
-- >>> :{
--   let
--     f              :: Point 2 r -> r
--     f (Point2 x y) = x
--   in f (Point2 1 2)
-- :}
-- 1
pattern Point2       :: r -> r -> Point 2 r
pattern Point2 x y = Point (Vector2 x y)
{-# COMPLETE Point2 #-}

-- | Similarly, we can write:
--
-- >>> :{
--   let
--     g                :: Point 3 r -> r
--     g (Point3 x y z) = z
--   in g myPoint
-- :}
-- 3
pattern Point3       :: r -> r -> r -> Point 3 r
pattern Point3 x y z = (Point (Vector3 x y z))
{-# COMPLETE Point3 #-}

--------------------------------------------------------------------------------
-- * Point Functors

-- | Types that we can transform by mapping a function on each point in the structure
class PointFunctor g where
  pmap :: (Point (Dimension (g r)) r -> Point (Dimension (g s)) s) -> g r -> g s

  -- pemap :: (d ~ Dimension (g r)) => (Point d r :+ p -> Point d s :+ p) -> g r -> g s
  -- pemap =

instance PointFunctor (Point d) where
  pmap f = f


--------------------------------------------------------------------------------
-- * Functions specific to Two Dimensional points

-- | Compare by distance to the first argument
cmpByDistanceTo              :: (Ord r, Num r, Arity d)
                             => Point d r :+ c -> Point d r :+ p -> Point d r :+ q -> Ordering
cmpByDistanceTo (c :+ _) p q = comparing (squaredEuclideanDist c) (p^.core) (q^.core)




-- | Squared Euclidean distance between two points
squaredEuclideanDist :: (Num r, Arity d) => Point d r -> Point d r -> r
squaredEuclideanDist = qdA

-- | Euclidean distance between two points
euclideanDist :: (Floating r, Arity d) => Point d r -> Point d r -> r
euclideanDist = distanceA
