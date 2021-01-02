{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}
module Data.Vector.Unpacked where

import           Control.DeepSeq
import           Control.Lens
import           Data.Coerce
import           Data.Ratio
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import           GHC.Real
import           Linear.V2
import Data.Tree

newtype NoPacking a = NoPacking {unNoPacking :: a} deriving (Show)

newtype Vector a = Vector (Unpacked (S a))

instance (Show a, Unpack a) => Show (Vector a) where
  show = show . v_toList

-- test_extend :: Vector Double -> Vector (Double, Bool)
test_extend :: Unpack b => Vector b -> Vector (b, Bool)
test_extend = v_extend (const True)


data Unpacked a where
  Rational :: !(V.Vector Integer) -> !(V.Vector Integer) -> Unpacked Rational
  Point2 :: !(Unpacked a) -> !(Unpacked a) -> Unpacked (V2 a)
  Tuple :: !(Unpacked a) -> !(Unpacked b) -> Unpacked (a,b)
  Unboxed :: VU.Unbox a => !(VU.Vector a) -> Unpacked a
  Packed :: !(V.Vector a) -> Unpacked (NoPacking a)

v_length :: Vector a -> Int
v_length (Vector xs) = ulength xs

v_index :: Unpack a => Vector a -> Int -> a
v_index (Vector xs) i = coerce (uget xs i)

v_toList :: Unpack a => Vector a -> [a]
v_toList v = [ v_index v i | i <- [0..v_length v-1]]

rep :: Vector a -> Tree String
rep (Vector xs) = urep xs

ulength :: Unpacked a -> Int
ulength (Rational a _) = V.length a
ulength (Point2 a _)    = ulength a
ulength (Unboxed u)    = VU.length u
ulength (Packed v)    = V.length v
ulength (Tuple a _)    = ulength a

urep :: Unpacked a -> Tree String
urep (Rational a b) = Node "Rational" [Node "UNBOXED" [], Node "UNBOXED" []]
urep (Point2 a b)   = Node "Point2" [urep a, urep b]
urep (Unboxed u)    = Node "UNBOXED" []
urep (Packed v)     = Node "BOXED" []
urep (Tuple a b)    = Node "Tuple" [urep a, urep b]

uget :: Unpacked a -> Int -> a
uget (Rational a b) i = (a V.! i) :% (b V.! i)
uget (Tuple a b) i    = (uget a i, uget b i)
uget (Unboxed u) i    = u VU.! i
uget (Packed v) i     = NoPacking (v V.! i)
uget _ _              = undefined

umap :: Unpack b => (a -> b) -> Unpacked a -> Unpacked (S b)
umap fn packed = pack $ map fn $ toList packed

v_map :: (Unpack a, Unpack b) => (a -> b) -> Vector a -> Vector b
v_map fn = pack' . map fn . v_toList

toList :: Unpacked a -> [a]
toList packed = [ uget packed i | i <- [0..ulength packed-1] ]

extend :: Unpack p => Unpacked a -> (a -> p) -> Unpacked (a, S p)
extend arr f = Tuple arr (umap f arr)

v_extend :: (Unpack a, Unpack b) => (a -> b) -> Vector a -> Vector (a,b)
v_extend fn (Vector xs) = Vector $ Tuple xs (umap (fn . coerce) xs)

unextend :: Unpacked (a, p) -> Unpacked a
unextend (Tuple a _p) = a
unextend Unboxed{} = error "shouldn't happen"
-- unextend (Packed v) = Packed (V.map fst v)

instance Show (Unpacked a) where
  show (Rational a b) = "R " ++ show a ++ " " ++ show b
  show (Point2 a b)   = show a ++ " " ++ show b
  show (Tuple a b)    = show a ++ " " ++ show b
  show Unboxed{}      = "unboxed"
  show Packed{}       = "other"

class (Coercible (S a) a) => Unpack a where
  pack :: [a] -> Unpacked (S a)
  pack' :: [a] -> Vector a
  pack' = Vector . pack

instance Unpack () where
  pack xs = Unboxed (VU.fromList xs)

instance Unpack Rational where
  pack xs = Rational (V.fromList $!! map numerator xs) (V.fromList $!! map denominator xs)

instance Unpack Double where
  pack xs = Unboxed (VU.fromList xs)

instance (Unpack a) => Unpack (V2 a) where
  pack xs =
    Point2 (pack $ map (view _x) xs) (pack $ map (view _y) xs)
  
instance (Unpack a, Unpack b) => Unpack (a,b) where
  pack xs =
    let (ls,rs) = unzip xs
    in Tuple (pack ls) (pack rs)

instance {-# OVERLAPS #-} (S a ~ NoPacking a) => Unpack a where
  pack xs = Packed (V.fromListN (length xs) xs)

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
{-
boxedElementSize :: [a] -> IO Word
boxedElementSize lst =
  (-) <$> computeSize (V.fromListN 2 lst) <*> computeSize (V.fromListN 1 lst)

unboxedElementSize :: VU.Unbox a => [a] -> IO Word
unboxedElementSize lst =
  (-) <$> computeSize (VU.fromListN 2 lst) <*> computeSize (VU.fromListN 1 lst)

packedElementSize :: Unpack a => [a] -> IO Word
packedElementSize lst =
  (-) <$> computeSize (pack $ take 2 lst) <*> computeSize (pack $ take 1 lst)

computeSize :: a -> IO Word
computeSize v
    | v `seq` False = undefined
    | otherwise = do
      size <- recursiveSize v
      return (size `div` 8)
-}
