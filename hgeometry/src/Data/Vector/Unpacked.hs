{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}
module Data.Vector.Unpacked where

import           Control.DeepSeq
import           Control.Monad.Identity
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Tree
import qualified Data.Vector             as V
import qualified Data.Vector.Generic     as G
import qualified Data.Vector.Unboxed     as VU
import           GHC.Real

import           Data.Vector.Unpacked.Mutable (Left, R, Rep (..), Right, Unpack)
import qualified Data.Vector.Unpacked.Mutable as Mutable

import Data.Geometry.Point
-- import GHC.DataSize
import Linear.V2

newtype Vector a = Vector { unVector :: Unpacked a (R a) }

instance (Unpack a, Show a) => Show (Vector a) where
  show = show . G.toList

type instance G.Mutable Vector = Mutable.MVector
-- type instance G.Mutable Unpacked = Mutable.Unpacked

data Unpacked a (r::Rep) where
  Rational :: !(V.Vector Integer) -> !(V.Vector Integer) -> Unpacked Rational RatRep
  Tuple    :: !(Unpacked (Left (a,b)) (R (Left (a,b)))) ->
              !(Unpacked (Right (a,b)) (R (Right (a,b)))) ->
              Unpacked (a,b) (TRep (R a) (R b))
  Unboxed  :: VU.Unbox a => !(VU.Vector a) -> Unpacked a URep
  Boxed    :: !(V.Vector a) -> Unpacked a BRep

-- instance VU.Unbox (MVector s a)
instance (Unpack a) => G.Vector Vector a where
  basicUnsafeFreeze :: PrimMonad m => Mutable.MVector (PrimState m) a -> m (Vector a)
  basicUnsafeFreeze = fmap Vector . uUnsafeFreeze . Mutable.unVector
  basicUnsafeThaw :: PrimMonad m => Vector a -> m (G.Mutable Vector (PrimState m) a)
  basicUnsafeThaw = fmap Mutable.Vector . uUnsafeThaw . unVector
  basicLength :: Vector a -> Int
  basicLength = uLength . unVector
  basicUnsafeSlice :: Int -> Int -> Vector a -> Vector a
  basicUnsafeSlice i l = Vector . uUnsafeSlice i l . unVector

  {-# SPECIALIZE basicUnsafeIndexM :: Monad m => Vector Double -> Int -> m Double #-}
  basicUnsafeIndexM :: Monad m => Vector a -> Int -> m a
  basicUnsafeIndexM v i = uUnsafeIndexM (unVector v) i
  basicUnsafeCopy :: PrimMonad m => Mutable.MVector (PrimState m) a -> Vector a -> m ()
  basicUnsafeCopy mut immut = uUnsafeCopy (Mutable.unVector mut) (unVector immut)
  -- elemseq :: Vector a -> a -> b -> b

-- {-# RULES "basicUnsafeIndexM_Double" G.basicUnsafeIndexM = basicUnsafeIndexM_Double #-}
-- basicUnsafeIndexM_Double :: Monad m => Vector Double -> Int -> m Double
-- basicUnsafeIndexM_Double v i = coerce <$> uUnsafeIndexM (coerce v :: Unpacked (Packing Double)) i

-- {-# RULES "basicUnsafeIndexM_Tuple" forall u i. G.basicUnsafeIndexM (Vector u) i = basicUnsafeIndexM_Tuple u i #-}
-- basicUnsafeIndexM_Tuple :: (Unpack a, Unpack b, Monad m) => Unpacked (a,b) (R a) -> Int -> m (a,b)
-- basicUnsafeIndexM_Tuple v i = coerce <$> uUnsafeIndexM v i

uUnsafeSlice :: Int -> Int -> Unpacked a (R a) -> Unpacked a (R a)
uUnsafeSlice i j (Rational a b) =
  Rational (G.basicUnsafeSlice i j a) (G.basicUnsafeSlice i j b)
uUnsafeSlice i j (Tuple a b) =
  Tuple (uUnsafeSlice i j a) (uUnsafeSlice i j b)
uUnsafeSlice i j (Unboxed u) =
  Unboxed (G.basicUnsafeSlice i j u)
uUnsafeSlice i j (Boxed u) =
  Boxed (G.basicUnsafeSlice i j u)

uUnsafeCopy :: PrimMonad m => Mutable.Unpacked (PrimState m) a (R a) -> Unpacked a (R a) -> m ()
uUnsafeCopy (Mutable.Rational a1 b1) (Rational a2 b2) = do
  G.basicUnsafeCopy a1 a2
  G.basicUnsafeCopy b1 b2
uUnsafeCopy (Mutable.Tuple a1 b1) (Tuple a2 b2) = do
  uUnsafeCopy a1 a2
  uUnsafeCopy b1 b2
uUnsafeCopy (Mutable.Unboxed u1) (Unboxed u2) = do
  G.basicUnsafeCopy u1 u2
uUnsafeCopy (Mutable.Boxed u1) (Boxed u2) = do
  G.basicUnsafeCopy u1 u2

data Container a where
  SimpleContainer :: (Unpack a) => !(Vector a) -> Container a

cIndex :: Container (Double, ()) -> Int -> (Double, ())
cIndex (SimpleContainer arr) i = G.unsafeIndex arr i

tIndex :: Container (Double, Int) -> Int -> Double
tIndex (SimpleContainer arr) i = fst (G.unsafeIndex arr i)

bIndex :: Unpacked Bool BRep -> Int -> Bool
bIndex arr i = runIdentity $ uUnsafeIndexM arr i

uIndex :: Vector Double -> Int -> Double
uIndex arr i = runIdentity $ uUnsafeIndexM (unVector arr) i

cFromList :: Unpack a => [a] -> Container a
cFromList lst = SimpleContainer (G.fromList lst)


-- memoryDiff :: (NFData (v a), G.Vector v a) => v a -> v a -> IO Word
-- memoryDiff a b = do
--   a' <- recursiveSize $!! a
--   b' <- recursiveSize $!! b
--   return (b'-a')

listPoint2 :: [Point 2 Rational]
listPoint2 = [Point2 11 2, Point2 3 4, Point2 5 6, Point2 7 8, Point2 9 10]

listV2 :: [V2 Rational]
listV2 = -- [V2 11 2, V2 3 4, V2 5 6, V2 7 8, V2 9 10]
  [V2 1 1, V2 1 1, V2 1 1, V2 1 1, V2 1 1]

listV2' :: [V2 Int]
listV2' = [V2 1 2, V2 3 4, V2 5 6, V2 7 8, V2 9 10]

listT2 :: [(Rational, Rational)]
listT2 = [(,) 1 2, (,) 2 3, (,) 4 5, (,) 6 7]

{-# RULES "uUnsafeIndexM_Tuple" uUnsafeIndexM = (\arr ->
  case arr of
    Tuple a b -> \i -> do
      a' <- uUnsafeIndexM a i
      b' <- uUnsafeIndexM b i
      return (a', b')) :: Monad m => Unpacked (a, b) (TRep (R a) (R b)) -> Int -> m (a,b) #-}

{-# RULES
"uUnsafeIndexM_Double" uUnsafeIndexM = uUnsafeIndexM_Double
"uUnsafeIndexM_()"     uUnsafeIndexM = uUnsafeIndexM_Unit
#-}

{-# INLINE uUnsafeIndexM_Double #-}
uUnsafeIndexM_Double :: (Monad m) => Unpacked Double URep -> Int -> m Double
-- uUnsafeIndexM_Double arr i = uUnsafeIndexM_URep arr i -- Don't use this code. It prevents optimizations.
uUnsafeIndexM_Double arr i = case arr of
  Unboxed u    -> G.basicUnsafeIndexM u i

uUnsafeIndexM_Unit :: (Monad m) => Unpacked () URep -> Int -> m ()
uUnsafeIndexM_Unit arr i = case arr of
  Unboxed u    -> G.basicUnsafeIndexM u i

{-# INLINE uUnsafeIndexM_URep #-}
uUnsafeIndexM_URep :: (R a ~ URep, Monad m) => Unpacked a (R a) -> Int -> m a
uUnsafeIndexM_URep !arr !i = case arr of
  Unboxed u    -> G.basicUnsafeIndexM u i

{-# NOINLINE uUnsafeIndexM #-}
uUnsafeIndexM :: Monad m => Unpacked a (R a) -> Int -> m a
uUnsafeIndexM !arr !i = case arr of
  Rational a b -> (:%) <$> G.basicUnsafeIndexM a i <*> G.basicUnsafeIndexM b i
  Tuple a b    -> (,) <$> uUnsafeIndexM_1 a i <*> uUnsafeIndexM_1 b i
  Unboxed u    -> G.basicUnsafeIndexM u i
  Boxed u      -> G.basicUnsafeIndexM u i

uUnsafeIndexM_1 :: Monad m => Unpacked a (R a) -> Int -> m a
uUnsafeIndexM_1 !arr !i = case arr of
  Rational a b -> (:%) <$> G.basicUnsafeIndexM a i <*> G.basicUnsafeIndexM b i
  Tuple a b    -> (,) <$> uUnsafeIndexM_1 a i <*> uUnsafeIndexM_1 b i
  Unboxed u    -> G.basicUnsafeIndexM u i
  Boxed u      -> G.basicUnsafeIndexM u i


-- uUnsafeIndexM_Double :: Monad m => Unpacked (S Double) -> Int -> m (S Double)
-- uUnsafeIndexM_Double !arr !i = case arr of
--   Unboxed u    -> G.basicUnsafeIndexM u i

uUnsafeFreeze :: PrimMonad m => Mutable.Unpacked (PrimState m) a (R a) -> m (Unpacked a (R a))
uUnsafeFreeze mut = case mut of
  Mutable.Rational a b -> do
    a' <- G.basicUnsafeFreeze a
    b' <- G.basicUnsafeFreeze b
    return $ Rational a' b'
  Mutable.Tuple a b -> Tuple <$> uUnsafeFreeze a <*> uUnsafeFreeze b
  Mutable.Unboxed u -> Unboxed <$> G.basicUnsafeFreeze u
  Mutable.Boxed u -> Boxed <$> G.basicUnsafeFreeze u

uUnsafeThaw :: PrimMonad m => Unpacked a (R a) -> m (Mutable.Unpacked (PrimState m) a (R a))
uUnsafeThaw arr = case arr of
  Rational a b -> do
    a' <- G.basicUnsafeThaw a
    b' <- G.basicUnsafeThaw b
    return $ Mutable.Rational a' b'
  Tuple a b -> Mutable.Tuple <$> uUnsafeThaw a <*> uUnsafeThaw b
  Unboxed u -> Mutable.Unboxed <$> G.basicUnsafeThaw u
  Boxed u -> Mutable.Boxed <$> G.basicUnsafeThaw u

rep :: Vector a -> IO ()
rep (Vector xs) = putStrLn $ drawTree $ urep xs

uLength :: Unpacked a (R a) -> Int
uLength (Rational a _) = V.length a
uLength (Unboxed u)    = VU.length u
uLength (Boxed v)      = V.length v
uLength (Tuple a _)    = uLength a

urep :: Unpacked a (R a) -> Tree String
urep Rational{}  = Node "Rational" [Node "BOXED" [], Node "BOXED" []]
urep Unboxed{}   = Node "UNBOXED" []
urep Boxed{}     = Node "BOXED" []
urep (Tuple a b) = Node "Tuple" [urep a, urep b]
