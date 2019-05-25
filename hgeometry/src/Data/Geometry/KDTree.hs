{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.KDTree where

import           Control.Lens hiding (imap, element, Empty, (:<))
import           Data.BinaryTree
import           Unsafe.Coerce(unsafeCoerce)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import           Data.Proxy
import           Data.LSeq (LSeq, pattern (:<|))
import qualified Data.LSeq as LSeq
import           Data.Util
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


-- | Expects the input to be a set, i.e. no duplicates
--
-- running time: \(O(n \log n)\)
buildKDTree :: (Arity d, 1 <= d, Ord r)
            => [Point d r :+ p] -> KDTree d p r
buildKDTree = maybe Empty (Tree . buildKDTree') . NonEmpty.nonEmpty

buildKDTree' :: (Arity d, 1 <= d, Ord r)
             => NonEmpty.NonEmpty (Point d r :+ p) -> KDTree' d p r
buildKDTree' = KDT . addBoxes . build (Coord 1) . toPointSet . LSeq.fromNonEmpty
  where     -- compute one tree with bounding boxes, then merge them together
    addBoxes t = let bbt = foldUpData (\l _ r -> boundingBoxList' [l,r])
                                      (boundingBox . (^.core)) t
                 in zipExactWith (\(SP c m) b -> Split c m b) const t bbt


-- | Nub by sorting first
ordNub :: Ord a => NonEmpty.NonEmpty a -> NonEmpty.NonEmpty a
ordNub = fmap NonEmpty.head . NonEmpty.group1 . NonEmpty.sort



toPointSet :: (Arity d, Ord r)
           => LSeq n (Point d r :+ p) -> PointSet (LSeq n) d p r
toPointSet = FV.imap sort . FV.replicate
  where
    sort i = LSeq.unstableSortBy (compareOn $ 1 + i)


compareOn       :: (Ord r, Arity d)
                => Int -> Point d r :+ e -> Point d r :+ e -> Ordering
compareOn i p q = let f = (^.core.unsafeCoord i)
                  in (f p, p^.core) `compare` (f q, q^.core)


build      :: (1 <= d, Arity d, Ord r)
           => Coord d
           -> PointSet (LSeq 1) d p r
           -> BinLeafTree (Split' d r) (Point d r :+ p)
build i ps = case asSingleton ps of
    Left p    -> Leaf p
    Right ps' -> let (l,m,r) = splitOn i ps'
                     j       = succ i
                   -- the pattern match proves tha tthe seq has >= 2 elements
                 in Node (build j l) m (build j r)


--------------------------------------------------------------------------------

reportSubTree :: KDTree' d p r -> NonEmpty.NonEmpty (Point d r :+ p)
reportSubTree = NonEmpty.fromList . F.toList . unKDT

-- | Searches in a KDTree
--
-- running time: \(O(n^{(d-1)/d} + k)\)
searchKDTree    :: (Arity d, Ord r)
                => Box d q r -> KDTree d p r -> [Point d r :+ p]
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
        l' = if qr `intersects` boxOf l then search l else []
        r' = if qr `intersects` boxOf r then search r else []


boxOf :: (Arity d, Ord r) => BinLeafTree (Split d r) (Point d r :+ p) -> Box d () r
boxOf (Leaf p)                 = boundingBox (p^.core)
boxOf (Node _ (Split _ _ b) _) = b

containedIn :: (Arity d, Ord r) => Box d q r -> Box d p r -> Bool
(Box (CWMin p :+ _) (CWMax q :+ _)) `containedIn` b = all (`intersects` b) [p,q]

--------------------------------------------------------------------------------


type PointSet seq d p r = Vector d (seq (Point d r :+ p))

-- | running time: \(O(n)\)
splitOn                 :: (Arity d, KnownNat d, Ord r)
                        => Coord d
                        -> PointSet (LSeq 2) d p r
                        -> ( PointSet (LSeq 1) d p r
                           , Split' d r
                           , PointSet (LSeq 1) d p r)
splitOn c@(Coord i) pts = (l, SP c (m^.core.unsafeCoord i), r)
  where
    -- i = traceShow (c,j) j

    m = let xs = fromJust $ pts^?element' (i-1)
        in xs `LSeq.index` (F.length xs `div` 2)

    -- Since the input seq has >= 2 elems, F.length xs / 2 >= 1. It follows
    -- that the both sets thus have at least one elemnt.
    -- f :: LSeq 2 _ -> (LSeq 1 _, LSeq 1 _)
    f = bimap LSeq.promise LSeq.promise
      . LSeq.partition (\p -> compareOn i p m == LT)

    (l,r) = unzip' . fmap f $ pts

    -- unzip' :: Vector d (a,b) -> (Vector d a, Vector d b)
    unzip' = bimap vectorFromListUnsafe vectorFromListUnsafe . unzip . F.toList


asSingleton   :: (1 <= d, Arity d)
              => PointSet (LSeq 1) d p r
              -> Either (Point d r :+ p) (PointSet (LSeq 2) d p r)
asSingleton v = case v^.element (C :: C 0) of
                  (p :<| s) | null s -> Left p -- only one lement
                  _                  -> Right $ unsafeCoerce v
