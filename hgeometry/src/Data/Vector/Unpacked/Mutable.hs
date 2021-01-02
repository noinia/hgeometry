{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}
module Data.Vector.Unpacked.Mutable where

import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Data.Coerce                 (Coercible, coerce)
import           Data.Proxy                  (Proxy (..))
-- import           Data.Ratio
import           Data.Tree
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable         as V
import qualified Data.Vector.Unboxed.Mutable as VU
import           GHC.Real
import           Linear.V2

newtype NoPacking a = NoPacking {unNoPacking :: a} deriving (Show)

newtype MVector s a = Vector (Unpacked s (S a))

data Unpacked s a where
  Rational :: !(V.MVector s Integer) -> !(V.MVector s Integer) -> Unpacked s Rational
  Tuple    :: !(Unpacked s a) -> !(Unpacked s b) -> Unpacked s (a,b)
  Unboxed  :: VU.Unbox a => !(VU.MVector s a) -> Unpacked s a
  Boxed    :: !(V.MVector s a) -> Unpacked s (NoPacking a)

-- instance VU.Unbox (MVector s a)

instance Unpack a => GM.MVector MVector a where
  basicLength = uLength . coerce
  basicUnsafeSlice i n = coerce . uUnsafeSlice i n . coerce
  basicOverlaps a b = uOverlaps (coerce a) (coerce b)
  
  basicUnsafeNew :: (PrimMonad m) => Int -> m (MVector (PrimState m) a)
  basicUnsafeNew n = Vector <$> uUnsafeNew (Proxy @ a) n

  basicInitialize :: PrimMonad m => MVector (PrimState m) a -> m ()
  basicInitialize v = uInitialize (coerce v)

  basicUnsafeReplicate :: PrimMonad m => Int -> a -> m (MVector (PrimState m) a)
  basicUnsafeReplicate i v = Vector <$> uUnsafeReplicate i v

  basicUnsafeRead  :: PrimMonad m => MVector (PrimState m) a -> Int -> m a
  basicUnsafeRead v i = coerce <$> uUnsafeRead (coerce v) i

  basicUnsafeWrite :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()
  basicUnsafeWrite v i a = uUnsafeWrite (coerce v) i (coerce a)

  basicClear       :: PrimMonad m => MVector (PrimState m) a -> m ()
  basicClear v = uClear (coerce v)

  basicSet         :: PrimMonad m => MVector (PrimState m) a -> a -> m ()
  basicSet v a = uSet (coerce v) (coerce a)

  basicUnsafeCopy  :: PrimMonad m => MVector (PrimState m) a   -- ^ target
                                  -> MVector (PrimState m) a   -- ^ source
                                  -> m ()
  basicUnsafeCopy a b = uUnsafeCopy (coerce a) (coerce b)

  basicUnsafeMove  :: PrimMonad m => MVector (PrimState m) a   -- ^ target
                                  -> MVector (PrimState m) a   -- ^ source
                                  -> m ()
  basicUnsafeMove a b = uUnsafeMove (coerce a) (coerce b)


uUnsafeMove :: PrimMonad m => Unpacked (PrimState m) a   -- ^ target
                           -> Unpacked (PrimState m) a   -- ^ source
                           -> m ()
uUnsafeMove (Rational a1 b1) (Rational a2 b2) = do
  GM.basicUnsafeMove a1 a2
  GM.basicUnsafeMove b1 b2
uUnsafeMove (Tuple a1 b1) (Tuple a2 b2)       = do
  uUnsafeMove a1 a2
  uUnsafeMove b1 b2
uUnsafeMove (Unboxed u1) (Unboxed u2)         = GM.basicUnsafeMove u1 u2
uUnsafeMove (Boxed u1) (Boxed u2)             = GM.basicUnsafeMove u1 u2
uUnsafeMove _ _ = error ""

uUnsafeCopy :: PrimMonad m => Unpacked (PrimState m) a   -- ^ target
                           -> Unpacked (PrimState m) a   -- ^ source
                           -> m ()
uUnsafeCopy (Rational a1 b1) (Rational a2 b2) = do
  GM.basicUnsafeCopy a1 a2
  GM.basicUnsafeCopy b1 b2
uUnsafeCopy (Tuple a1 b1) (Tuple a2 b2)       = do
  uUnsafeCopy a1 a2
  uUnsafeCopy b1 b2
uUnsafeCopy (Unboxed u1) (Unboxed u2)         = GM.basicUnsafeCopy u1 u2
uUnsafeCopy (Boxed u1) (Boxed u2)             = GM.basicUnsafeCopy u1 u2
uUnsafeCopy _ _ = error ""

uSet :: PrimMonad m => Unpacked (PrimState m) a -> a -> m ()
uSet (Rational l r) (a :% b) = GM.basicSet l a >> GM.basicSet r b
uSet (Unboxed u) v = GM.basicSet u v
uSet (Boxed u) v = GM.basicSet u (coerce v)
uSet (Tuple l r) (a,b) = uSet l a >> uSet r b

uClear :: PrimMonad m => Unpacked (PrimState m) a -> m ()
uClear (Rational a b) = GM.basicClear a >> GM.basicClear b
uClear (Unboxed u) = GM.basicClear u
uClear (Boxed v) = GM.basicClear v
uClear (Tuple a b) = uClear a >> uClear b

uUnsafeWrite :: PrimMonad m => Unpacked (PrimState m) a -> Int -> a -> m ()
uUnsafeWrite (Rational l r) i (a :% b) = do
  GM.basicUnsafeWrite l i a
  GM.basicUnsafeWrite r i b
uUnsafeWrite (Unboxed u) i a = GM.basicUnsafeWrite u i a
uUnsafeWrite (Boxed v) i a   = GM.basicUnsafeWrite v i (coerce a)
uUnsafeWrite (Tuple l r) i (a,b) = do
  uUnsafeWrite l i a
  uUnsafeWrite r i b

uUnsafeRead :: PrimMonad m => Unpacked (PrimState m) a -> Int -> m a
uUnsafeRead (Rational a b) i = (:%) <$> GM.basicUnsafeRead a i <*> GM.basicUnsafeRead b i
uUnsafeRead (Unboxed u) i    = GM.basicUnsafeRead u i
uUnsafeRead (Boxed v) i      = NoPacking <$> GM.basicUnsafeRead v i
uUnsafeRead (Tuple a b) i    = (,) <$> uUnsafeRead a i <*> uUnsafeRead b i

uInitialize :: PrimMonad m => Unpacked (PrimState m) a -> m ()
uInitialize (Rational a b) = do
      GM.basicInitialize a
      GM.basicInitialize b
uInitialize (Unboxed u) = GM.basicInitialize u
uInitialize (Boxed v) = GM.basicInitialize v
uInitialize (Tuple a b) = uInitialize a >> uInitialize b

uUnsafeSlice :: Int -> Int -> Unpacked s a -> Unpacked s a
uUnsafeSlice i n = \case
  Rational a b -> Rational
    (GM.basicUnsafeSlice i n a)
    (GM.basicUnsafeSlice i n b)
  Unboxed u -> Unboxed $ GM.basicUnsafeSlice i n u
  Boxed v -> Boxed $ GM.basicUnsafeSlice i n v
  Tuple a b -> Tuple (uUnsafeSlice i n a) (uUnsafeSlice i n b)

uOverlaps :: Unpacked s a -> Unpacked s a -> Bool
uOverlaps (Rational a1 b1) (Rational a2 b2) = GM.basicOverlaps a1 a2 || GM.basicOverlaps b1 b2
uOverlaps (Tuple a1 b1) (Tuple a2 b2)       = uOverlaps a1 a2 || uOverlaps b1 b2
uOverlaps (Unboxed u1) (Unboxed u2)         = GM.basicOverlaps u1 u2
uOverlaps (Boxed u1) (Boxed u2)             = GM.basicOverlaps u1 u2
uOverlaps _ _                               = error "cannot happen"

rep :: MVector s a -> Tree String
rep (Vector xs) = urep xs

uLength :: Unpacked s a -> Int
uLength (Rational a _) = V.length a
uLength (Unboxed u)    = VU.length u
uLength (Boxed v)      = V.length v
uLength (Tuple a _)    = uLength a

urep :: Unpacked s a -> Tree String
urep Rational{}     = Node "Rational" [Node "UNBOXED" [], Node "UNBOXED" []]
urep Unboxed{}      = Node "UNBOXED" []
urep Boxed{}        = Node "BOXED" []
urep (Tuple a b)    = Node "Tuple" [urep a, urep b]

class (Coercible (S a) a) => Unpack a where
  uUnsafeNew :: PrimMonad m => Proxy a -> Int -> m (Unpacked (PrimState m) (S a))
  uUnsafeReplicate :: PrimMonad m => Int -> a -> m (Unpacked (PrimState m) (S a))

  default uUnsafeNew :: (VU.Unbox (S a), S a ~ a, PrimMonad m) => Proxy a -> Int -> m (Unpacked (PrimState m) (S a))
  uUnsafeNew _ n = Unboxed <$> GM.basicUnsafeNew n

  default uUnsafeReplicate :: (VU.Unbox a, S a ~ a, PrimMonad m) => Int -> a -> m (Unpacked (PrimState m) (S a))
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
  uUnsafeNew :: PrimMonad m => Proxy (a, b) -> Int -> m (Unpacked (PrimState m) (S (a, b)))
  uUnsafeNew _ n = Tuple <$> uUnsafeNew (Proxy @ a) n <*> uUnsafeNew (Proxy @ b) n

  uUnsafeReplicate n (a,b) = Tuple <$> uUnsafeReplicate n a <*> uUnsafeReplicate n b

-- Boxed instance.
instance {-# OVERLAPS #-} (S a ~ NoPacking a) => Unpack a where
  uUnsafeNew _ n = Boxed <$> GM.basicUnsafeNew n
  uUnsafeReplicate n a = Boxed <$> GM.basicUnsafeReplicate n a

type family S a where
  S Rational = Rational
  S (V2 a) = V2 (S a)
  S () = ()
  S Double = Double
  S (a,b) = (S a, S b)
  S a = NoPacking a

-- Element cost of boxed [Point 2 Double :+ ()]:   11 words
-- Element cost of unboxed [Point 2 Double :+ ()]: 2 words
-- Element cost of boxed [Point 2 Rational :+ ()]: 61 words
-- Element cost of packed [Point 2 Rational :+ ()]: 8 words
