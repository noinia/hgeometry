{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}
module Data.Vector.Unpacked where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Coerce             (coerce)
-- import           Data.Ratio
import           Data.Tree
import qualified Data.Vector         as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as VU
import           GHC.Real

import           Data.Vector.Unpacked.Mutable (NoPacking, Unpack)
import qualified Data.Vector.Unpacked.Mutable as Mutable

newtype Vector a = Vector (Unpacked (Mutable.S a))

instance (Unpack a, Show a) => Show (Vector a) where
  show = show . G.toList

type instance G.Mutable Vector = Mutable.MVector
type instance G.Mutable Unpacked = Mutable.Unpacked

data Unpacked a where
  Rational :: !(V.Vector Integer) -> !(V.Vector Integer) -> Unpacked Rational
  Tuple    :: !(Unpacked a) -> !(Unpacked b) -> Unpacked (a,b)
  Unboxed  :: VU.Unbox a => !(VU.Vector a) -> Unpacked a
  Boxed    :: !(V.Vector a) -> Unpacked (NoPacking a)

-- instance VU.Unbox (MVector s a)
instance (Unpack a) => G.Vector Vector a where
  basicUnsafeFreeze :: PrimMonad m => Mutable.MVector (PrimState m) a -> m (Vector a)
  basicUnsafeFreeze = fmap coerce . uUnsafeFreeze . coerce
  basicUnsafeThaw :: PrimMonad m => Vector a -> m (G.Mutable Vector (PrimState m) a)
  basicUnsafeThaw = fmap coerce . uUnsafeThaw . coerce
  basicLength :: Vector a -> Int
  basicLength = uLength . coerce
  basicUnsafeSlice :: Int -> Int -> Vector a -> Vector a
  basicUnsafeSlice i l = coerce . uUnsafeSlice i l . coerce

  basicUnsafeIndexM :: Monad m => Vector a -> Int -> m a
  basicUnsafeIndexM v i = coerce <$> uUnsafeIndexM (coerce v) i
  basicUnsafeCopy :: PrimMonad m => G.Mutable Vector (PrimState m) a -> Vector a -> m ()
  basicUnsafeCopy mut immut = uUnsafeCopy (coerce mut) (coerce immut)
  -- elemseq :: Vector a -> a -> b -> b

uUnsafeSlice :: Int -> Int -> Unpacked a -> Unpacked a
uUnsafeSlice i j (Rational a b) =
  Rational (G.basicUnsafeSlice i j a) (G.basicUnsafeSlice i j b)
uUnsafeSlice i j (Tuple a b) =
  Tuple (uUnsafeSlice i j a) (uUnsafeSlice i j b)
uUnsafeSlice i j (Unboxed u) =
  Unboxed (G.basicUnsafeSlice i j u)
uUnsafeSlice i j (Boxed u) =
  Boxed (G.basicUnsafeSlice i j u)

uUnsafeCopy :: PrimMonad m => Mutable.Unpacked (PrimState m) a -> Unpacked a -> m ()
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
uUnsafeCopy _ _ = error "Impossible"

uUnsafeIndexM :: Monad m => Unpacked a -> Int -> m a
uUnsafeIndexM arr i = case arr of
  Rational a b -> (:%) <$> G.basicUnsafeIndexM a i <*> G.basicUnsafeIndexM b i
  Tuple a b    -> (,) <$> uUnsafeIndexM a i <*> uUnsafeIndexM b i
  Unboxed u    -> G.basicUnsafeIndexM u i
  Boxed u      -> coerce <$> G.basicUnsafeIndexM u i

uUnsafeFreeze :: PrimMonad m => Mutable.Unpacked (PrimState m) a -> m (Unpacked a)
uUnsafeFreeze mut = case mut of
  Mutable.Rational a b -> do
    a' <- G.basicUnsafeFreeze a
    b' <- G.basicUnsafeFreeze b
    return $ Rational a' b'
  Mutable.Tuple a b -> Tuple <$> uUnsafeFreeze a <*> uUnsafeFreeze b
  Mutable.Unboxed u -> Unboxed <$> G.basicUnsafeFreeze u
  Mutable.Boxed u -> Boxed <$> G.basicUnsafeFreeze u

uUnsafeThaw :: PrimMonad m => Unpacked a -> m (Mutable.Unpacked (PrimState m) a)
uUnsafeThaw arr = case arr of
  Rational a b -> do
    a' <- G.basicUnsafeThaw a
    b' <- G.basicUnsafeThaw b
    return $ Mutable.Rational a' b'
  Tuple a b -> Mutable.Tuple <$> uUnsafeThaw a <*> uUnsafeThaw b
  Unboxed u -> Mutable.Unboxed <$> G.basicUnsafeThaw u
  Boxed u -> Mutable.Boxed <$> G.basicUnsafeThaw u

rep :: Vector a -> Tree String
rep (Vector xs) = urep xs

uLength :: Unpacked a -> Int
uLength (Rational a _) = V.length a
uLength (Unboxed u)    = VU.length u
uLength (Boxed v)      = V.length v
uLength (Tuple a _)    = uLength a

urep :: Unpacked a -> Tree String
urep Rational{}  = Node "Rational" [Node "UNBOXED" [], Node "UNBOXED" []]
urep Unboxed{}   = Node "UNBOXED" []
urep Boxed{}     = Node "BOXED" []
urep (Tuple a b) = Node "Tuple" [urep a, urep b]
