{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.KDTree where

import Data.Geometry.Properties
import Data.Geometry.Box
import           Control.Lens hiding (imap, element)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Point
import           Data.Geometry.Vector
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Fixed as FV
import           GHC.TypeLits
import           Prelude hiding (replicate)

--------------------------------------------------------------------------------

newtype Coord (d :: Nat) = Coord { unCoord ::  Int}

instance KnownNat d => Eq (Coord d) where
  (Coord i) == (Coord j) = (i `mod` d) == (j `mod` d)
    where
      d = fromInteger . natVal $ (Proxy :: Proxy d)

instance KnownNat d => Show (Coord d) where
  show (Coord i) = show $ 1 + (i `mod` d)
    where
      d = fromInteger . natVal $ (Proxy :: Proxy d)

instance KnownNat d => Enum (Coord d) where
  toEnum i = Coord $ 1 + (i `mod` d)
    where
      d = fromInteger . natVal $ (Proxy :: Proxy d)
  fromEnum = (subtract 1) . unCoord

-- TODO: Store boxes
data Split d r = Split !(Coord d) !r deriving (Show,Eq)

data KDTree' d p r = Leaf (Point d r :+ p)
                   | Node (KDTree' d p r) (Split d r) (KDTree' d p r)

deriving instance (Show p, Show r, Arity d, KnownNat d) => Show (KDTree' d p r)
deriving instance (Eq p, Eq r, Arity d, KnownNat d) => Eq (KDTree' d p r)

-- TODO: instance for monotraversable


data KDTree d p r = Empty
                  | Tree (KDTree' d p r)


buildKDTree :: [Point d r :+ p] -> KDTree d p r
buildKDTree = undefined

buildKDTree' :: (Arity d, KnownNat d, Index' 0 d, Ord r)
             => NonEmpty.NonEmpty (Point d r :+ p) -> KDTree' d p r
buildKDTree' = build (Coord 1) . FV.imap sort . FV.replicate . S.fromList . F.toList
  where
    sort i = S.unstableSortBy (comparing (^.core.unsafeCoord i))

build      :: (Index' 0 d, Arity d, KnownNat d, Ord r)
           => Coord d -> PointSet d p r -> KDTree' d p r
build i ps = case asSingleton ps of
    Just p  -> Leaf p
    Nothing -> let (l,m,r) = splitOn i ps
                   j       = succ i
               in Node (build j l) m (build j r)

--------------------------------------------------------------------------------

-- TODO: Rewrite this using a monotraversable fold
reportSubTree :: KDTree' d p r -> NonEmpty.NonEmpty (Point d r :+ p)
reportSubTree (Leaf p)     = p NonEmpty.:| []
reportSubTree (Node l _ r) = reportSubTree l <> reportSubTree r

searchKDTree                  :: (Arity d, Ord r)
                              => Box d q r -> KDTree' d p r -> [Point d r :+ p]
searchKDTree qr (Leaf p)
  | (p^.core) `intersects` qr = [p]
  | otherwise                 = []
searchKDTree qr (Node l m r)  = undefined -- TODO: We need to store the associated regions


--------------------------------------------------------------------------------


type PointSet d p r = Vector d (S.Seq (Point d r :+ p))

splitOn                 :: (Arity d, KnownNat d, Ord r) => Coord d -> PointSet d p r
                        -> (PointSet d p r, Split d r, PointSet d p r)
splitOn c@(Coord i) pts = (l, Split c m, r)
  where
    mp = let xs = pts^.element' (i-1) in xs `S.index` (S.length xs `div` 2)
    m  = mp^.core.unsafeCoord i
    -- (S.viewR -> (l:>m), r) = let xs = pts^.element' (i-1)
    --                          in S.splitAt (1 + S.length xs `div` 2) xs
    f = S.partition (\p -> p^.core.unsafeCoord i < m)

    (l,r) = unzip' . fmap f $ pts

    -- unzip' :: Vector d (a,b) -> (Vector d a, Vector d b)
    unzip' = bimap vectorFromListUnsafe vectorFromListUnsafe . unzip . F.toList

-- TODO: Make sure that the partitioning is the same in all lists, in
-- particular in case of points that have the same i-coordinates


-- pattern (Singleton p) <-

asSingleton   :: (Index' 0 d, Arity d) => PointSet d p r -> Maybe (Point d r :+ p)
asSingleton v = case F.toList $ v^.element (C :: C 0) of
                [p] -> Just p
                _   -> Nothing
