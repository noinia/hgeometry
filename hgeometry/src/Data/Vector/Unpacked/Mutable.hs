{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeFamilies      #-}
module Data.Vector.Unpacked.Mutable where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Coerce             (coerce)
import Data.Proxy              (Proxy (..))
-- import           Data.Ratio
import           Data.Tree
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable         as V
import qualified Data.Vector.Unboxed.Mutable as VU
import           GHC.Real

newtype MVector s a = Vector { unVector :: Unpacked s a (R a) }

data Unpacked s a (r::Rep) where
  Rational :: !(V.MVector s Integer) -> !(V.MVector s Integer) -> Unpacked s Rational RatRep
  Tuple    :: !(Unpacked s (Left (a,b)) (R (Left (a,b)))) ->
              !(Unpacked s (Right (a,b)) (R (Right (a,b)))) ->
              Unpacked s (a,b) (TRep (R a) (R b))
  Unboxed  :: VU.Unbox a => !(VU.MVector s a) -> Unpacked s a URep
  Boxed    :: !(V.MVector s a) -> Unpacked s a BRep

instance Unpack a => GM.MVector MVector a where
  basicLength = uLength . unVector
  basicUnsafeSlice i n = Vector . uUnsafeSlice i n . unVector
  basicOverlaps a b = uOverlaps (unVector a) (unVector b)

  basicUnsafeNew :: (PrimMonad m) => Int -> m (MVector (PrimState m) a)
  basicUnsafeNew n = Vector <$> uUnsafeNew (Proxy @ a) n

  basicInitialize :: PrimMonad m => MVector (PrimState m) a -> m ()
  basicInitialize v = uInitialize (unVector v)

  basicUnsafeReplicate :: PrimMonad m => Int -> a -> m (MVector (PrimState m) a)
  basicUnsafeReplicate i v = Vector <$> uUnsafeReplicate i v

  basicUnsafeRead  :: PrimMonad m => MVector (PrimState m) a -> Int -> m a
  basicUnsafeRead v i = uUnsafeRead (unVector v) i

  basicUnsafeWrite :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()
  basicUnsafeWrite v i a = uUnsafeWrite (unVector v) i a

  basicClear       :: PrimMonad m => MVector (PrimState m) a -> m ()
  basicClear v = uClear (unVector v)

  basicSet         :: PrimMonad m => MVector (PrimState m) a -> a -> m ()
  basicSet v a = uSet (unVector v) a

  basicUnsafeCopy  :: PrimMonad m => MVector (PrimState m) a   -- ^ target
                                  -> MVector (PrimState m) a   -- ^ source
                                  -> m ()
  basicUnsafeCopy a b = uUnsafeCopy (unVector a) (unVector b)

  basicUnsafeMove  :: PrimMonad m => MVector (PrimState m) a   -- ^ target
                                  -> MVector (PrimState m) a   -- ^ source
                                  -> m ()
  basicUnsafeMove a b = uUnsafeMove (unVector a) (unVector b)


uUnsafeMove :: PrimMonad m => Unpacked (PrimState m) a (R a)   -- ^ target
                           -> Unpacked (PrimState m) a (R a)   -- ^ source
                           -> m ()
uUnsafeMove (Rational a1 b1) (Rational a2 b2) = do
  GM.basicUnsafeMove a1 a2
  GM.basicUnsafeMove b1 b2
uUnsafeMove (Tuple a1 b1) (Tuple a2 b2)       = do
  uUnsafeMove a1 a2
  uUnsafeMove b1 b2
uUnsafeMove (Unboxed u1) (Unboxed u2)         = GM.basicUnsafeMove u1 u2
uUnsafeMove (Boxed u1) (Boxed u2)             = GM.basicUnsafeMove u1 u2

uUnsafeCopy :: PrimMonad m => Unpacked (PrimState m) a (R a)   -- ^ target
                           -> Unpacked (PrimState m) a (R a)   -- ^ source
                           -> m ()
uUnsafeCopy (Rational a1 b1) (Rational a2 b2) = do
  GM.basicUnsafeCopy a1 a2
  GM.basicUnsafeCopy b1 b2
uUnsafeCopy (Tuple a1 b1) (Tuple a2 b2)       = do
  uUnsafeCopy a1 a2
  uUnsafeCopy b1 b2
uUnsafeCopy (Unboxed u1) (Unboxed u2)         = GM.basicUnsafeCopy u1 u2
uUnsafeCopy (Boxed u1) (Boxed u2)             = GM.basicUnsafeCopy u1 u2

uSet :: PrimMonad m => Unpacked (PrimState m) a (R a) -> a -> m ()
uSet (Rational l r) (a :% b) = GM.basicSet l a >> GM.basicSet r b
uSet (Unboxed u) v           = GM.basicSet u (coerce v)
uSet (Boxed u) v             = GM.basicSet u (coerce v)
uSet (Tuple l r) (a,b)       = uSet l a >> uSet r b

uClear :: PrimMonad m => Unpacked (PrimState m) a (R a) -> m ()
uClear (Rational a b) = GM.basicClear a >> GM.basicClear b
uClear (Unboxed u)    = GM.basicClear u
uClear (Boxed v)      = GM.basicClear v
uClear (Tuple a b)    = uClear a >> uClear b

uUnsafeWrite :: PrimMonad m => Unpacked (PrimState m) a (R a) -> Int -> a -> m ()
uUnsafeWrite (Rational l r) i (a :% b) = do
  GM.basicUnsafeWrite l i a
  GM.basicUnsafeWrite r i b
uUnsafeWrite (Unboxed u) i a = GM.basicUnsafeWrite u i (coerce a)
uUnsafeWrite (Boxed v) i a   = GM.basicUnsafeWrite v i (coerce a)
uUnsafeWrite (Tuple l r) i (a,b) = do
  uUnsafeWrite l i a
  uUnsafeWrite r i b

uUnsafeRead :: PrimMonad m => Unpacked (PrimState m) a (R a) -> Int -> m a
uUnsafeRead (Rational a b) i = (:%) <$> GM.basicUnsafeRead a i <*> GM.basicUnsafeRead b i
uUnsafeRead (Unboxed u) i    = GM.basicUnsafeRead u i
uUnsafeRead (Boxed v) i      = GM.basicUnsafeRead v i
uUnsafeRead (Tuple a b) i    = (,) <$> uUnsafeRead a i <*> uUnsafeRead b i

uInitialize :: PrimMonad m => Unpacked (PrimState m) a (R a) -> m ()
uInitialize (Rational a b) = do
      GM.basicInitialize a
      GM.basicInitialize b
uInitialize (Unboxed u) = GM.basicInitialize u
uInitialize (Boxed v) = GM.basicInitialize v
uInitialize (Tuple a b) = uInitialize a >> uInitialize b

uUnsafeSlice :: Int -> Int -> Unpacked s a (R a) -> Unpacked s a (R a)
uUnsafeSlice i n = \case
  Rational a b -> Rational
    (GM.basicUnsafeSlice i n a)
    (GM.basicUnsafeSlice i n b)
  Unboxed u -> Unboxed $ GM.basicUnsafeSlice i n u
  Boxed v -> Boxed $ GM.basicUnsafeSlice i n v
  Tuple a b -> Tuple (uUnsafeSlice i n a) (uUnsafeSlice i n b)

uOverlaps :: Unpacked s a (R a) -> Unpacked s a (R a) -> Bool
uOverlaps (Rational a1 b1) (Rational a2 b2) = GM.basicOverlaps a1 a2 || GM.basicOverlaps b1 b2
uOverlaps (Tuple a1 b1) (Tuple a2 b2)       = uOverlaps a1 a2 || uOverlaps b1 b2
uOverlaps (Unboxed u1) (Unboxed u2)         = GM.basicOverlaps u1 u2
uOverlaps (Boxed u1) (Boxed u2)             = GM.basicOverlaps u1 u2

rep :: MVector s a -> Tree String
rep (Vector xs) = urep xs

uLength :: Unpacked s a (R a) -> Int
uLength (Rational a _) = V.length a
uLength (Unboxed u)    = VU.length u
uLength (Boxed v)      = V.length v
uLength (Tuple a _)    = uLength a

urep :: Unpacked s a (R a) -> Tree String
urep Rational{}  = Node "Rational" [Node "UNBOXED" [], Node "UNBOXED" []]
urep Unboxed{}   = Node "UNBOXED" []
urep Boxed{}     = Node "BOXED" []
urep (Tuple a b) = Node "Tuple" [urep a, urep b]

class Unpack a where
  uUnsafeNew :: PrimMonad m => Proxy a -> Int -> m (Unpacked (PrimState m) a (R a))
  uUnsafeReplicate :: PrimMonad m => Int -> a -> m (Unpacked (PrimState m) a (R a))

  default uUnsafeNew :: (VU.Unbox a, R a ~ URep, PrimMonad m) =>
    Proxy a -> Int -> m (Unpacked (PrimState m) a (R a))
  uUnsafeNew _ n = Unboxed <$> GM.basicUnsafeNew n

  default uUnsafeReplicate :: (VU.Unbox a, R a ~ URep, PrimMonad m) =>
    Int -> a -> m (Unpacked (PrimState m) a (R a))
  uUnsafeReplicate n a = Unboxed <$> GM.basicUnsafeReplicate n a


-- Unboxed instances:
instance Unpack () where
instance Unpack Double where

-- Partially unboxed.
instance Unpack Rational where
  uUnsafeNew _ n = (Rational <$> GM.basicUnsafeNew n <*> GM.basicUnsafeNew n)
  uUnsafeReplicate n (a :% b) = Rational
    <$> GM.basicUnsafeReplicate n a
    <*> GM.basicUnsafeReplicate n b


instance (Unpack a, Unpack b) => Unpack (a,b) where
  uUnsafeNew :: PrimMonad m => Proxy (a, b) -> Int -> m (Unpacked (PrimState m) (a,b) (TRep (R a) (R b)))
  uUnsafeNew _ n = Tuple <$> uUnsafeNew (Proxy @ a) n <*> uUnsafeNew (Proxy @ b) n

  uUnsafeReplicate n (a,b) = Tuple <$> uUnsafeReplicate n a <*> uUnsafeReplicate n b

-- Boxed instance.
instance {-# OVERLAPS #-} (R a ~ BRep) => Unpack a where
  uUnsafeNew _ n = Boxed <$> GM.basicUnsafeNew n
  uUnsafeReplicate n a = Boxed <$> GM.basicUnsafeReplicate n a

data Rep = TRep Rep Rep | BRep | URep | RatRep

-- newtype CVector s a = CVector (Col s a (R a))

-- test_uIndex :: PrimMonad m => CVector (PrimState m) Double -> m Double
-- test_uIndex (CVector col) = case col of
--   ColU u -> GM.read u 0
--   -- ColB v -> GM.read v 0

-- test_uIndex_two :: PrimMonad m => CVector (PrimState m) Double -> m Double
-- test_uIndex_two vec = cRead vec 0

-- test_uIndex_three :: CVector (PrimState IO) (Double, (Double, Double)) -> IO (Double, (Double, Double))
-- test_uIndex_three vec = cRead vec 0

-- {-# INLINE cRead #-}
-- cRead :: PrimMonad m => CVector (PrimState m) a -> Int -> m a
-- cRead (CVector col) i = colRead col i

-- {-# RULES "colReadDouble" colRead = colReadDouble #-}
-- {-# INLINE colReadDouble #-}
-- colReadDouble :: PrimMonad m => Col (PrimState m) Double URep -> Int -> m Double
-- colReadDouble col i = case col of
--   ColU u -> GM.read u i

-- {-# RULES "colReadTuple" colRead = (\col ->
--   case col of
--     ColT a b -> \i -> do
--       a' <- colRead a i
--       b' <- colRead b i
--       return (a', b')) :: PrimMonad m => Col (PrimState m) (a, b) (TRep (R a) (R b)) -> Int -> m (a,b) #-}
-- {- INLINE colReadTuple -}
-- colReadTuple :: PrimMonad m => Col (PrimState m) (a, b) (TRep (R a) (R b)) -> Int -> m (a,b)
-- colReadTuple col = case col of
--   ColT a b -> \i -> do
--     a' <- colRead a i
--     b' <- colRead b i
--     return (a', b')

-- {-# RULES "colRead_1" colRead_1 = colRead #-}

-- {-# NOINLINE colRead #-}
-- {- SPECIALIZE colRead :: Col (PrimState IO) Double URep -> Int -> IO Double -}
-- colRead :: PrimMonad m => Col (PrimState m) a (R a) -> Int -> m a
-- colRead col i = case col of
--   ColT a b -> do
--     a' <- colRead_1 a i
--     b' <- colRead_1 b i
--     return (a', b')
--   ColU u -> GM.read u i
--   ColB u -> GM.read u i

-- {- NOINLINE colRead_1 -}
-- colRead_1 :: PrimMonad m => Col (PrimState m) a (R a) -> Int -> m a
-- colRead_1 col i = case col of
--   ColT a b -> do
--     a' <- colRead_1 a i
--     b' <- colRead_1 b i
--     return (a', b')
--   ColU u -> GM.read u i
--   ColB u -> GM.read u i

-- data Col s a (r::Rep) where
--   -- ColT    :: !(Col s (Left (a,b)) (R a)) -> !(Col s (Right (a,b)) (R b))  -> Col s (a,b) (TRep (R a) (R b))
--   ColT    :: !(Col s (Left (a,b)) (R (Left (a,b)))) -> !(Col s (Right (a,b)) (R (Right (a,b)))) -> Col s (a,b) (TRep (R a) (R b))
--   ColU    :: VU.Unbox a => !(VU.MVector s a) -> Col s a URep
--   ColB    :: !(V.MVector s a)                -> Col s a BRep

-- class UnpackCol a where
--   uUnsafeNewCol :: PrimMonad m => Proxy a -> Int -> m (Col (PrimState m) a (R a))
--   uUnsafeReplicateCol :: PrimMonad m => Int -> a -> m (Col (PrimState m) a (R a))

--   default uUnsafeNewCol :: (VU.Unbox a, R a ~ URep, PrimMonad m)
--     => Proxy a -> Int -> m (Col (PrimState m) a (R a))
--   uUnsafeNewCol _ n = ColU <$> GM.basicUnsafeNew n

--   default uUnsafeReplicateCol :: (VU.Unbox a, R a ~ URep, PrimMonad m) => Int -> a -> m (Col (PrimState m) a (R a))
--   uUnsafeReplicateCol n a = ColU <$> GM.basicUnsafeReplicate n a

-- instance UnpackCol () where
-- instance UnpackCol Double where
-- instance (UnpackCol a, UnpackCol b) => UnpackCol (a,b) where
--   uUnsafeNewCol :: PrimMonad m => Proxy (a, b) -> Int -> m (Col (PrimState m) (a,b) (TRep (R a) (R b)))
--   uUnsafeNewCol _ n = ColT <$> uUnsafeNewCol (Proxy @ a) n <*> uUnsafeNewCol (Proxy @ b) n

--   uUnsafeReplicateCol n (a,b) = ColT <$> uUnsafeReplicateCol n a <*> uUnsafeReplicateCol n b
-- instance {-# OVERLAPS #-} (R a ~ BRep) => UnpackCol a where
--   uUnsafeNewCol = undefined
--   uUnsafeReplicateCol = undefined

type family Left a where
  Left (a,b) = a

type family Right a where
  Right (a,b) = b

type family R a :: Rep where
  R Rational = RatRep
  R ()       = URep
  R (a,b)    = TRep (R a) (R b)
  R Double   = URep
  R a        = BRep

-- type family S a where
--   S Rational = Rational
--   S (V2 a) = V2 (S a)
--   S () = Packing ()
--   S Double = Packing Double
--   S (a,b) = (S a, S b)
--   S a = NoPacking a

-- Element cost of boxed [Point 2 Double :+ ()]:   11 words
-- Element cost of unboxed [Point 2 Double :+ ()]: 2 words
-- Element cost of boxed [Point 2 Rational :+ ()]: 61 words
-- Element cost of packed [Point 2 Rational :+ ()]: 8 words
