{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.KDTree where

import           Control.Lens hiding (imap, element, Empty)
import           Data.BinaryTree
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Sequence as S
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Fixed as FV
import           GHC.TypeLits
import           Prelude hiding (replicate)

import Debug.Trace

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
  fromEnum = subtract 1 . unCoord


data Split d r = Split !(Coord d) !r !(Box d () r)

deriving instance (Show r, Arity d, KnownNat d) => Show (Split d r)
deriving instance (Eq r, Arity d, KnownNat d)   => Eq (Split d r)


type Split' d r = SP (Coord d) r

newtype KDTree' d p r = KDT { unKDT :: BinLeafTree (Split d r) (Point d r :+ p) }

deriving instance (Show p, Show r, Arity d, KnownNat d) => Show (KDTree' d p r)
deriving instance (Eq p, Eq r, Arity d, KnownNat d)     => Eq   (KDTree' d p r)


data KDTree d p r = Empty
                  | Tree (KDTree' d p r)

deriving instance (Show p, Show r, Arity d, KnownNat d) => Show (KDTree d p r)
deriving instance (Eq p, Eq r, Arity d, KnownNat d)     => Eq   (KDTree d p r)


toMaybe          :: KDTree d p r -> Maybe (KDTree' d p r)
toMaybe Empty    = Nothing
toMaybe (Tree t) = Just t


buildKDTree :: (Arity d, KnownNat d, Index' 0 d, Ord r)
            => [Point d r :+ p] -> KDTree d p r
buildKDTree = maybe Empty (Tree . buildKDTree') . NonEmpty.nonEmpty

buildKDTree' :: (Arity d, KnownNat d, Index' 0 d, Ord r)
             => NonEmpty.NonEmpty (Point d r :+ p) -> KDTree' d p r
buildKDTree' = KDT . addBoxes
             . build (Coord 1) . FV.imap sort . FV.replicate . S.fromList . F.toList
  where
    sort     i = S.unstableSortBy (comparing (^.core.unsafeCoord (1 + i)))
                -- compute one tree with bounding boxes, then merge them together
    addBoxes t = let bbt = foldUpData (\l _ r -> boundingBoxList' [l,r])
                                      (boundingBox . (^.core)) t
                 in zipExactWith (\(SP c m) b -> Split c m b) const t bbt



build      :: (Index' 0 d, Arity d, KnownNat d, Ord r)
           => Coord d -> PointSet d p r -> BinLeafTree (Split' d r) (Point d r :+ p)
build i ps = case asSingleton ps of
    Just p  -> Leaf p
    Nothing -> let (l,m,r) = splitOn i ps
                   j       = succ i
               in Node (build j l) m (build j r)

--------------------------------------------------------------------------------

reportSubTree :: KDTree' d p r -> NonEmpty.NonEmpty (Point d r :+ p)
reportSubTree = NonEmpty.fromList . F.toList . unKDT

searchKDTree
  :: (Arity d, Ord r) =>
     Box d q r -> KDTree d p r -> [Point d r :+ p]
searchKDTree qr = maybe [] (searchKDTree' qr) . toMaybe

searchKDTree'                  :: (Arity d, Ord r)
                              => Box d q r -> KDTree' d p r -> [Point d r :+ p]
searchKDTree' qr = search . unKDT
  where
    search (Leaf p)
      | (p^.core) `intersects` qr = [p]
      | otherwise                 = []
    search t@(Node l (Split _ _ b) r)
      | b `containedIn` qr        = F.toList t
      | otherwise                 = l' ++ r'
      where
        l' = if b `intersects` boxOf l then search l else []
        r' = if b `intersects` boxOf r then search r else []

    boxOf (Leaf p)                 = boundingBox (p^.core)
    boxOf (Node _ (Split _ _ b) _) = b

containedIn :: (Arity d, Ord r) => Box d q r -> Box d p r -> Bool
(Box (Min p :+ _) (Max q :+ _)) `containedIn` b = all (`intersects` b) [p,q]

--------------------------------------------------------------------------------


type PointSet d p r = Vector d (S.Seq (Point d r :+ p))

splitOn                 :: (Arity d, KnownNat d, Ord r) => Coord d -> PointSet d p r
                        -> (PointSet d p r, Split' d r, PointSet d p r)
splitOn c@(Coord i) pts = (l, SP c m, r)
  where
    -- i = traceShow (c,j) j

    mp = let xs = pts^.element' (i-1)
         in traceShow (S.length xs) $   xs `S.index` (S.length xs `div` 2)
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
