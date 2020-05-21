{-# LANGUAGE UndecidableInstances #-}
--
-- Implementation of
-- Simulation of Simplicity: A Technique to Cope with Degenerate Cases in Geometric Algorithms
--
-- By
-- Herbert Edelsbrunner and Ernst Peter Mucke
module Algorithms.Geometry.SoS where

import           Control.Lens
import           Control.Monad.ST.Strict
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Ext
import           Data.Geometry.Point.Internal
import           Data.Geometry.Point.Class
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector hiding (imap)
import qualified Data.Geometry.Vector as GV
import qualified Data.List as List
import           Data.Maybe
import           Data.Ord (Down(..))
import           Data.Reflection
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           GHC.TypeNats
import           Linear.Matrix
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))

--------------------------------------------------------------------------------

data Sign = Negative | Positive deriving (Show,Eq,Ord,Enum,Bounded)

flipSign :: Sign -> Sign
flipSign = \case
  Negative -> Positive
  Positive -> Negative

--------------------------------------------------------------------------------



sideTest'             :: ( SoS p, Dimension p ~ 2, r ~ NumType p
                         , Eq r, Num r
                         ) => [p] -> Sign
sideTest' (q:p1:p2:_) = sideTest q (Vector2 p1 p2)





--------------------------------------------------------------------------------

-- |
simulateSimplicity :: forall t d r b. (Traversable t, SoSD d)
                   => (forall p. ( AsPoint p, HasIndex p
                                 , d ~ Dimension p, r ~ NumType p
                                 ) => t p -> b)
                   -> t (Point d r) -> b
simulateSimplicity = simulateSimplicity'



-- | The actual implementation of SoS
simulateSimplicity'     :: forall t d r b. (Traversable t, SoSD d)
                        => (forall i. ( CanAquire i (Point d r)
                                      , SoS (P i d r)
                                      ) => t (P i d r) -> b)
                        -> t (Point d r) -> b
simulateSimplicity' alg = runAcquire alg'
  where
    alg' :: forall i. CanAquire i (Point d r) => t i -> b
    alg' = alg . fmap (P @i @d @r)
      -- ideally the fmap would just be a coerce, but GHC does not want to do that.


--------------------------------------------------------------------------------

-- | Run a computation on something that can aquire i's.
runAcquire         :: forall t a b. Traversable t
                   => (forall i. CanAquire i a => t i -> b)
                   -> t a -> b
runAcquire alg pts = reify v $ \px -> alg (coerceTS px ts)
  where
    (v,ts) = replaceByIndex pts

    coerceTS   :: proxy s -> t Int -> t (I s a)
    coerceTS _ = fmap I
      -- Ideally this would just be a coerce. But GHC doesn't want to do that.




-- | Replaces every element by an index. Returns the new traversable
-- containing only these indices, as well as a vector with the
-- values. (such that indexing in this value gives the original
-- value).
replaceByIndex     :: forall t a. Traversable t => t a -> (V.Vector a, t Int)
replaceByIndex ts' = runST $ do
                               v <- MV.new n
                               t <- traverse (lbl v) ts
                               (,t) <$> V.unsafeFreeze v
  where
    (ts, n) = labelWithIndex ts'

    lbl         :: MV.MVector s' a -> (Int,a) -> ST s' Int
    lbl v (i,x) = MV.write v i x >> pure i

-- | Label each element with its index. Returns the new collection as
-- well as its size.
labelWithIndex :: Traversable t => t a -> (t (Int, a), Int)
labelWithIndex = flip runState 0 . traverse lbl
  where
    lbl   :: a -> State Int (Int,a)
    lbl x = do i <- get
               put $ i+1
               pure (i,x)


--------------------------------------------------------------------------------

class HasIndex i where
  indexOf :: i -> Int

class HasIndex i => CanAquire i a where
  aquire  :: i -> a

----------------------------------------

newtype I s a = I Int deriving (Eq, Ord, Enum)

instance Show (I s a) where
  showsPrec i (I j) = showsPrec i j

instance HasIndex (I s a) where
  indexOf (I i) = i

instance Reifies s (V.Vector a) => (I s a) `CanAquire` a where
  aquire (I i) = let v = reflect @s undefined in v V.! i

----------------------------------------

class AsPoint p where
  asPoint :: p -> Point (Dimension p) (NumType p)

instance AsPoint (Point d r) where
  asPoint = id

instance AsPoint p => AsPoint (p :+ e) where
  asPoint = asPoint . view core
  {-# INLINE asPoint #-}

--------------------------------------------------------------------------------

-- data R r = Constant !r | Indirect !r {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- instance Show r => Show (R r) where
--   show (Constant r)     = show r
--   show (Indirect r _ _) = show r

-- instance Eq r => Eq (R r) where

data RWithIdx r = RWithIdx !r
                           {-# UNPACK #-} !Int -- ^ index of the point in [0,n]
                           {-# UNPACK #-} !Int -- ^ index of the coordinate in [0,d-1]
                deriving (Eq)

instance Show r => Show (RWithIdx r) where
  showsPrec i (RWithIdx r _ _) = showsPrec i r

instance Ord r => Ord (RWithIdx r) where
  (RWithIdx x i j) `compare` (RWithIdx y k l) = x `compare` y
                                             <> (Down i) `compare` (Down k)
                                             <> j `compare` l
  -- see the paper, function Smaller, for the slightly weird implementation I guess.




-- | Data type that allows us to combine indexed R values and "normal"
-- constant R values.  Two indexed r values are equal if and only if
-- their indices match exactly. That means that values comming form
-- different indices can always be ordered, i.e. we get rid of degeneracies.
--
-- When comparing an indexed value against a constant the
-- equality/ordering is determined only w.r.t. the "real" r value.
--
data R r = Constant !r
         | IndexedR !r {-# UNPACK #-} !Int -- ^ index of the point in [0,n]
                       {-# UNPACK #-} !Int -- ^ index of the coordinate in [0,d-1]

toR :: R r -> r
toR = \case
  Constant r      -> r
  IndexedR r _ _  -> r

fromR :: r -> R r
fromR = Constant

fromIndexed                  :: RWithIdx r -> R r
fromIndexed (RWithIdx x i j) = IndexedR x i j

instance Show r => Show (R r) where
  showsPrec i = showsPrec i . toR

instance Eq r => Eq (R r) where
  (IndexedR x i j) == (IndexedR y k l) = x == y && i == k && j == l
  a                == b                = toR a == toR b

instance Ord r => Ord (R r) where
  (IndexedR x i j) `compare` (IndexedR y k l) = RWithIdx x i j `compare` RWithIdx y k l
  a                `compare` b                = toR a          `compare` toR b


-- instance (Num r, Ord r) => Num (R r) where
--   -- +, *, -, abs, signum,fromInteger, negate
--   -- (Constant )
--   signum
--   fromInteger = Constant

--   -- negate is prob. troublesome

--------------------------------------------------------------------------------

newtype P i d r = P i deriving (HasIndex, Eq, Show)

instance i `CanAquire` (Point d r) => (P i d r) `CanAquire` (Point d r) where
  aquire (P i) = aquire i

type instance NumType   (P i d r) = r
type instance Dimension (P i d r) = d


instance i `CanAquire` (Point d r) => AsPoint (P i d r) where
  asPoint (P i) = aquire i

asPointWithIndex       :: (Arity d, i `CanAquire` Point d r)
                       => P i d r -> Point d (RWithIdx r)
asPointWithIndex (P i) = Point . imap (\j r -> RWithIdx r (indexOf i) j) . toVec $ aquire i





-- instance (i `CanAquire` Point d r, Arity d) => P i d r `CanAquire` Point d (R i) where
--   aquire (P i) = Point $ pure ()




--------------------------------------------------------------------------------

-- | To support Simulation of Simplicity a point type p must support:
--
-- - Retrieving the Index of the point
-- - The dimension of p must support SoS
type SoS p = ( AsPoint p
             , HasIndex p
             , SoSD (Dimension p)
             )

-- | A dimension d has support for SoS when we can:
--
-- - sort a vector that has (d+1) entries and count the number of
-- - exchanges made generate the terms of the determinant of the
--   \(\Lambda_{d+1}\) matrix in the right order.
type SoSD d = ( SortI (d + 1)
              , ToTerms d
              )



-- | Given a query point q, and a vector of d points defining a
-- hyperplane test if q lies above or below the hyperplane.
--
sideTest      :: ( SoS p
                 , d ~ Dimension p, Arity d, Arity (d+1)
                 , r ~ NumType p, Num r, Eq r
                 )
              => p -> Vector d p -> Sign
sideTest q ps = case bimap odd signDet $ sort q ps of
                  (True,  d) -> flipSign d
                  (False, d) -> d

{-# SPECIALIZE sideTest ::
  (CanAquire i (Point 1 r), Num r, Eq r) => P i 1 r -> Vector 1 (P i 1 r) -> Sign #-}
{-# SPECIALIZE sideTest ::
  (CanAquire i (Point 2 r), Num r, Eq r) => P i 2 r -> Vector 2 (P i 2 r) -> Sign #-}
{-# SPECIALIZE sideTest ::
  (CanAquire i (Point 3 r), Num r, Eq r) => P i 3 r -> Vector 3 (P i 3 r) -> Sign #-}


-- | Returns the number of comparisons and the sorted matrix, in which the rows are
-- sorted in increasing order of the indices.
sort      :: forall p d r
          . (d ~ Dimension p, Arity d, Arity (d+1), SortI (d + 1)
            , r ~ NumType p
            , AsPoint p, HasIndex p
            )
          => p -> Vector d p -> (Int, Vector (d + 1) (Vector d r))
sort q ps = second (fmap asVec) . sortI . fmap (with indexOf) $ q `GV.cons` ps
  where
    asVec (With _ p) = toVec . asPoint $ p

{-# SPECIALIZE sort ::
  CanAquire i (Point 1 r) => P i 1 r -> Vector 1 (P i 1 r) -> (Int, Vector 2 (Vector 1 r)) #-}
{-# SPECIALIZE sort ::
  CanAquire i (Point 2 r) => P i 2 r -> Vector 2 (P i 2 r) -> (Int, Vector 3 (Vector 2 r)) #-}
{-# SPECIALIZE sort ::
  CanAquire i (Point 3 r) => P i 3 r -> Vector 3 (P i 3 r) -> (Int, Vector 4 (Vector 3 r)) #-}


-- | Data type for p values that have an index.
data With p = With {-# UNPACK #-} !Int p deriving (Show)

with     :: (p -> Int) -> p -> With p
with f p = With (f p) p

instance Eq (With p) where
  (With i _) == (With j _) = i == j
instance Ord (With p) where
  (With i _) `compare` (With j _) = i `compare` j

--------------------------------------------------------------------------------

class SortI d where
  sortI :: Ord a => Vector d a -> (Int, Vector d a)

instance SortI 1 where
  sortI v = (0,v)
  {-# INLINE sortI #-}

instance SortI 2 where
  sortI v@(Vector2 i j) = case i `compare` j of
                            GT -> (1, Vector2 j i)
                            _  -> (0, v)
  {-# INLINE sortI #-}

-- | Based on the optimal sort in vector-algorithms
instance SortI 3 where
  sortI v@(Vector3 i j k) =
    case compare i j of
      GT -> case compare i k of
              GT -> case compare k j of
                      LT -> (1, Vector3 k j i)
                      _  -> (2, Vector3 j k i)
              _  -> (1, Vector3 j i k)
      _  -> case compare j k of
              GT -> case compare i k of
                      GT -> (2, Vector3 k i j)
                      _  -> (1, Vector3 i k j)
              _  -> (0,v)
  {-# INLINE sortI #-}

instance SortI 4 where
  sortI v@(Vector4 i j k l) =
    case compare i j of
        GT -> case compare i k of
                GT -> case compare j k of
                        GT -> case compare j l of
                                GT -> case compare k l of
                                        GT -> (2, Vector4 l k j i)
                                        _  -> (3, Vector4 k l j i)
                                _  -> case compare i l of
                                        GT -> (2, Vector4 k j l i)
                                        _  -> (1, Vector4 k j i l)
                        _ -> case compare k l of
                               GT -> case compare j l of
                                       GT -> (1, Vector4 l j k i)
                                       _  -> (2, Vector4 j l k i)
                               _  -> case compare i l of
                                       GT -> (3, Vector4 j k l i)
                                       _  -> (2, Vector4 j k i l)
                _  -> case compare i l of
                        GT -> case compare j l of
                                GT -> (2, Vector4 l j i k)
                                _  -> (3, Vector4 j l i k)
                        _  -> case compare k l of
                                GT -> (2, Vector4 j i l k)
                                _  -> (1, Vector4 j i k l)
        _  -> case compare j k of
                GT -> case compare i k of
                        GT -> case compare i l of
                                GT -> case compare k l of
                                        GT -> (3, Vector4 l k i j)
                                        _  -> (2, Vector4 k l i j)
                                _  -> case compare j l of
                                        GT -> (3, Vector4 k i l j)
                                        _  -> (2, Vector4 k i j l)
                        _  -> case compare k l of
                                GT -> case compare i l of
                                        GT -> (2, Vector4 l i k j)
                                        _  -> (1, Vector4 i l k j)
                                _  -> case compare j l of
                                        GT -> (2, Vector4 i k l j)
                                        _  -> (1, Vector4 i k j l)
                _  -> case compare j l of
                        GT -> case compare i l of
                                GT -> (3, Vector4 l i j k)
                                _  -> (2, Vector4 i l j k)
                        _  -> case compare k l of
                                GT -> (1, Vector4 i j l k)
                                _  -> (0, v)
  {-# INLINE sortI #-}


-- | Determines the sign of the Determinant.
--
-- pre: the rows in the input vector are given in increasing index
-- order
signDet :: (Num r, Eq r, ToTerms d) => Vector (d + 1) (Vector d r) -> Sign
signDet = signFromTerms . toTerms

{-# SPECIALIZE signDet :: (Num r, Eq r) => Vector 2 (Vector 1 r) -> Sign #-}
{-# SPECIALIZE signDet :: (Num r, Eq r) => Vector 3 (Vector 2 r) -> Sign #-}
{-# SPECIALIZE signDet :: (Num r, Eq r) => Vector 4 (Vector 3 r) -> Sign #-}


-- | Given the terms, in decreasing order of significance, computes the sign
--
-- i.e. expects a list of terms, we base the sign on the sign of the first non-zero term.
--
-- pre: the list contains at least one such a term.
signFromTerms :: (Num r, Eq r) => [r] -> Sign
signFromTerms = List.head . mapMaybe signum'
  where
    signum' x = case signum x of
                  -1    -> Just Negative
                  0     -> Nothing
                  1     -> Just Positive
                  _     -> error "signum': absurd"

--------------------------------------------------------------------------------

class ToTerms d where
  toTerms :: Num r => Vector (d + 1) (Vector d r) -> [r]


instance ToTerms 1 where
  -- note this is Lambda_2 from the paper
  toTerms (Vector2 (Vector1 i)
                   (Vector1 j)) = [ i - j -- det22 [i 1, j 1] = i*1 - j*1
                                  , 1
                                  ]
  {-# INLINE toTerms #-}

instance ToTerms 2 where
  -- note this is Lambda_3 from the paper
  toTerms (Vector3 (Vector2 i1 i2)
                   (Vector2 j1 j2)
                   (Vector2 k1 k2)) = [ det33 $ V3 (V3 i1 i2 1)
                                                   (V3 j1 j2 1)
                                                   (V3 k1 k2 1)
                                      , -(j1 - k1) --

                                      , j2 - k2    -- det22 [[j2, 1], [k2, 1]] = j2*1 - k2*1
                                      , i1 - k1
                                      , 1
                                      ]
  {-# INLINE toTerms #-}


instance ToTerms 3 where
  -- note this is Lambda_4 from the paper
  toTerms (Vector4 (Vector3 i1 i2 i3)
                   (Vector3 j1 j2 j3)
                   (Vector3 k1 k2 k3)
                   (Vector3 l1 l2 l3)
          ) = [ det44 $ V4 (V4 i1 i2 i3 1)   -- 0
                           (V4 j1 j2 j3 1)
                           (V4 k1 k2 k3 1)
                           (V4 l1 l2 l3 1)
              , det33 $ V3 (V3 j1 j2 1)      -- 1
                           (V3 k1 k2 1)
                           (V3 l1 l2 1)
              , ((-1) *) .                   -- 2
                det33 $ V3 (V3 j1 j3 1)
                           (V3 k1 k3 1)
                           (V3 l1 l3 1)
              , det33 $ V3 (V3 j2 j3 1)      -- 3
                           (V3 k2 k3 1)
                           (V3 l2 l3 1)
              , ((-1) *) .                   -- 4
                det33 $ V3 (V3 i1 i2 1)
                           (V3 k1 k2 1)
                           (V3 l1 l2 1)
              , k1 - l1                      -- 5
              , -(k2 - l2)                   -- 6
              , det33 $ V3 (V3 i1 i3 1)      -- 7
                           (V3 k1 k3 1)
                           (V3 l1 l3 1)
              , k3 - l3                      -- 8
              , ((-1) *) .                   -- 9
                det33 $ V3 (V3 i2 i3 1)
                           (V3 k2 k3 1)
                           (V3 l2 l3 1)
              , det33 $ V3 (V3 i1 i2 1)      -- 10
                           (V3 j1 j2 1)
                           (V3 l1 l2 1)
              , -(j1 - l1)                   -- 11
              , j2 - l2                      -- 12
              , i1 - l1                      -- 13
              , 1                            -- 14
              ]
  {-# INLINE toTerms #-}

--------------------------------------------------------------------------------

-- | Determines the sign of the Determinant, assuming the input is
-- given in homogeneous coordinates.
--
-- pre: the rows in the input vector are given in increasing index
-- order
signDetHom :: (Num r, Eq r, ToTermsHom d) => Matrix d d r -> Sign
signDetHom = signFromTerms . toTermsHom

class ToTermsHom d where
  toTermsHom :: Matrix d d r -> [r]




-- class ToTermsHom 2 where
--   toTerms m@(Matrix (Vector2 i1 i2)
--                     (Vector2 j1 j2)
--             )                     = [ det22 m
--                                     , -i1
--                                     , j2
--                                     ,
--                                     ]


-- toTerms   :: Matrix d d r -> [r]
-- toTerms m = undefined



--------------------------------------------------------------------------------

-- TODO: Remove this one
instance HasIndex (Point d r :+ Int) where
  indexOf = view extra


test1 :: Sign
test1 = sideTest (Point1 1 :+ 0 :: Point 1 Int :+ Int) (Vector1 $ Point1 5 :+ 1)

test2 :: Sign
test2 = sideTest (Point1 5 :+ 0 :: Point 1 Int :+ Int) (Vector1 $ Point1 5 :+ 1)


test3 :: Sign
test3 = sideTest (Point2 (-1) 5 :+ 0 :: Point 2 Int :+ Int) (Vector2 (Point2 0 0  :+ 1)
                                                                     (Point2 0 10 :+ 2)
                                                            )


pattern Point1 x = Point (Vector1 x)


testV :: Sign
testV = simulateSimplicity sideTest' [ Point2 (-1) 5
                                     , Point2 0 0
                                     , Point2 0 10
                                     ]
