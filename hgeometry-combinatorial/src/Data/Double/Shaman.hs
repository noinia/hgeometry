-- https://tel.archives-ouvertes.fr/tel-03116750/document
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Double.Shaman
  ( Shaman
  , significativeBits
  , significativeDigits
  , SDouble
  ) where

import Numeric.MathFunctions.Comparison ( addUlps)
import Numeric.MathFunctions.Constants
import GHC.TypeLits
import Data.Double.Approximate

-- | Double-precision floating point numbers that throw exceptions if
--   the accumulated errors grow large enough to cause unstable branching.
--
--   If @SDouble n@ works without throwing any exceptions, it'll be safe to
--   use @DoubleRelAbs n 0@ instead for a sizable performance boost.
--
-- >>> sin pi == (0 :: SDouble 0)
-- *** Exception: Insufficient precision.
-- ...
--
-- @SDouble 0@ failed so @DoubleRelAbs 0 0@ will lead to an unstable branch. In
-- other words, it'll return @False@ when it should have returned @True@:
--
-- >>> sin pi == (0 :: DoubleRelAbs 0 0)
-- False
--
-- Comparing to within 1 ULP stabalizes the branch:
--
-- >>> sin pi == (0 :: SDouble 1)
-- True
--
-- >>> sin pi == (0 :: DoubleRelAbs 1 0)
-- True
--
newtype SDouble (n::Nat) = SDouble Shaman
  deriving (Num, Fractional, Floating, Real, RealFrac, RealFloat)

instance Show (SDouble n) where
  showsPrec d (SDouble s) = showsPrec d s

instance Read (SDouble n) where
  readsPrec d inp = [ (SDouble v, r) | (v, r) <- readsPrec d inp ]


-- shamanUlpDistance :: Shaman -> Word64
-- shamanUlpDistance (Shaman n e) = ulpDistance n (n+e)

compareSDouble :: KnownNat n => SDouble n -> SDouble n -> Ordering
compareSDouble a@(SDouble a') b@(SDouble b')
   | a == b    = EQ
   | otherwise = shamanValue a' `compare` shamanValue b'

toDoubleRelAbs :: SDouble n -> DoubleRelAbs n 0
toDoubleRelAbs (SDouble (Shaman v _e)) = DoubleRelAbs v

-- If (a :: DoubleRelAbs n 0) == (b :: DoubleRelAbs n 0)
instance KnownNat n => Eq (SDouble n) where
  a@(SDouble a') == b@(SDouble b')
    | blessedResult == approxResult = blessedResult
    | otherwise                     = error "Insufficient precision."
    where
      blessedResult = a' == b'
      approxResult = toDoubleRelAbs a == toDoubleRelAbs b

instance KnownNat n => Ord (SDouble n) where
  compare = compareSDouble





-- | Double-precision floating point numbers with error-bounds.
--
-- Some digits can be represented exactly and have essentially an infinitely number of significant digits:
--
-- >>> significativeDigits 1
-- Infinity
--
-- Some fractional numbers can also be represented exactly:
--
-- >>> significativeDigits 0.5
-- Infinity
--
-- Other numbers are merely approximations:
--
-- >>> significativeDigits 0.1
-- 16.255619765854984
--
-- Pi is an irrational number so we can't represent it with infinite precision:
--
-- >>> significativeDigits pi
-- 15.849679651557175
--
-- @sin pi@ should theoretically be zero but we cannot do better than saying it is near zero:
--
-- >>> sin pi :: Shaman
-- 1.2246467991473532e-16±4.440892098500626e-16
--
-- The error margins are greater than value itself so we have no significant digits:
--
-- >>> significativeDigits (sin pi)
-- 0.0
--
-- Since 'near zero' is not zero, the following fails when using Doubles:
--
-- >>> sin pi == (0 :: Double)
-- False
--
-- Equality testing for Shaman numbers tests whether the two intervals
-- overlap:
--
-- >>> sin pi == (0 :: Shaman)
-- True
data Shaman = Shaman
  { shamanValue :: {-# UNPACK #-} !Double
  , shamanError :: {-# UNPACK #-} !Double
  }

instance Show Shaman where
  showsPrec d Shaman{..} = showParen (d > 10) $
    shows shamanValue . showChar '±' . shows shamanError

instance Read Shaman where
  readsPrec d = readParen (d > app_prec) $ \r ->
      [ (Shaman{..}, t')
      | (shamanValue, '±':t) <- reads r
      , (shamanError, t') <- reads t]
    where app_prec = 10

-- | Number of significant bits (base 2).
significativeBits :: Shaman -> Double
significativeBits v = significativeValue v / log 2

-- | Number of significant digits (base 10).
significativeDigits :: Shaman -> Double
significativeDigits v = significativeValue v / log 10

significativeValue :: Shaman -> Double
significativeValue (Shaman v e)
  | e == 0    = m_pos_inf
  | isNaN e   = 0
  | v == 0    = max 0 (log (abs e - 1))
  | otherwise =
    let relError = abs (e / v) in
    if relError >= 1
      then 0
      else negate $ log relError

instance Num Shaman where
  Shaman x dx + Shaman y dy =
    case x+y of
      !xy -> Shaman xy (dx+dy+twoSum x y xy)

  Shaman x dx * Shaman y dy =
    case x*y of
      !z -> Shaman z (dx * y + dy * x + fma x y (-z))
  
  abs (Shaman x dx) = Shaman (abs x) dx

  signum (Shaman x dx) = Shaman (signum x) dx

  negate (Shaman x dx) = Shaman (negate x) dx

  fromInteger i =
    let d = fromInteger i
    in Shaman d (abs $ realToFrac $ i - round d)

instance Fractional Shaman where
  Shaman x dx / Shaman y dy =
    let z = x/y
        numerator = dx - fma y z (-x) - z * dy
        denominator = y + dy
    in Shaman z (numerator / denominator)
  fromRational r =
    let d = fromRational r
    in Shaman d (abs $ realToFrac $ r - realToFrac d)

instance Floating Shaman where
  pi = Shaman pi (addUlps 1 pi - pi)
  sqrt (Shaman x dx)
    | z == 0, dx == 0 =
      Shaman z 0
    | z == 0 =
      Shaman z (sqrt (abs dx)) -- FIXME: Use long double.
    | otherwise =
      Shaman z ((r + dx) / (z+z))
    where
      r = negate $ fma z z (negate x)
      z = sqrt x
  log = foreignOp erf_log log
  exp = foreignOp erf_exp exp
  (**) = foreignBinOp erf_pow (**)
  sin = foreignOp erf_sin sin
  cos = foreignOp erf_cos cos
  tan = foreignOp erf_tan tan
  asin = foreignOp erf_asin asin
  acos = foreignOp erf_acos acos
  atan = foreignOp erf_atan atan
  sinh = foreignOp erf_sinh sinh
  cosh = foreignOp erf_cosh cosh
  tanh = foreignOp erf_tanh tanh
  asinh = foreignOp erf_asinh asinh
  acosh = foreignOp erf_acosh acosh
  atanh = foreignOp erf_atanh atanh

instance Real Shaman where
  toRational (Shaman v _) = toRational v

instance RealFrac Shaman where
  properFraction (Shaman v e) =
    case properFraction v of
      (i, v') -> (i, Shaman v' e)
  truncate (Shaman v _e) = truncate v
  round (Shaman v _e) = round v
  ceiling (Shaman v e) = ceiling (v+e)
  floor (Shaman v e) = floor (v-e)

instance RealFloat Shaman where
  floatRadix = floatRadix . shamanValue
  floatDigits = floatDigits . shamanValue
  floatRange = floatRange . shamanValue
  decodeFloat = decodeFloat . shamanValue
  encodeFloat m e = Shaman (encodeFloat m e) 0 -- FIXME: 1 ULP error bound?
  exponent = exponent . shamanValue
  significand s = Shaman (significand $ shamanValue s) 0
  scaleFloat p (Shaman v e) = Shaman (scaleFloat p v) e
  isNaN = isNaN . shamanValue
  isInfinite = isInfinite . shamanValue
  isDenormalized = isDenormalized . shamanValue
  isNegativeZero = isNegativeZero . shamanValue
  isIEEE = isIEEE . shamanValue
  atan2 = foreignBinOp erf_atan2 atan2


instance Eq Shaman where
  x == y = compareShaman x y == EQ

instance Ord Shaman where
  compare = compareShaman

-- Returns EQ iff the two numbers overlap.
compareShaman :: Shaman -> Shaman -> Ordering
compareShaman (Shaman x dx) (Shaman y dy)
  | abs (x-y) < abs dx+ abs dy = EQ
  | x < y             = LT
  | otherwise         = GT

twoSum :: Double -> Double -> Double -> Double
twoSum x y xy = abs (x-x') + abs (y-y')
  where
    x' = xy-y
    y' = xy-x

foreignOp :: (Double -> Double -> Double -> Double) -> (Double -> Double) -> Shaman -> Shaman
foreignOp mkDelta fn (Shaman x dx) =
  let !z = fn x
  in Shaman z (abs $ mkDelta x dx z)

foreignBinOp ::
    (Double -> Double -> Double -> Double -> Double -> Double) ->
    (Double -> Double -> Double) ->
    Shaman -> Shaman -> Shaman
foreignBinOp mkDelta fn (Shaman x dx) (Shaman y dy) =
  let !z = fn x y
  in Shaman z (mkDelta x dx y dy z)

foreign import ccall unsafe "fma" fma :: Double -> Double -> Double -> Double

foreign import ccall unsafe "erf_log" erf_log :: Double -> Double -> Double -> Double
foreign import ccall unsafe "erf_exp" erf_exp :: Double -> Double -> Double -> Double
foreign import ccall unsafe "erf_pow" erf_pow :: Double -> Double -> Double -> Double -> Double -> Double

foreign import ccall unsafe "erf_sin" erf_sin :: Double -> Double -> Double -> Double
foreign import ccall unsafe "erf_cos" erf_cos :: Double -> Double -> Double -> Double
foreign import ccall unsafe "erf_tan" erf_tan :: Double -> Double -> Double -> Double

foreign import ccall unsafe "erf_asin" erf_asin :: Double -> Double -> Double -> Double
foreign import ccall unsafe "erf_acos" erf_acos :: Double -> Double -> Double -> Double
foreign import ccall unsafe "erf_atan" erf_atan :: Double -> Double -> Double -> Double

foreign import ccall unsafe "erf_sinh" erf_sinh :: Double -> Double -> Double -> Double
foreign import ccall unsafe "erf_cosh" erf_cosh :: Double -> Double -> Double -> Double
foreign import ccall unsafe "erf_tanh" erf_tanh :: Double -> Double -> Double -> Double

foreign import ccall unsafe "erf_asinh" erf_asinh :: Double -> Double -> Double -> Double
foreign import ccall unsafe "erf_acosh" erf_acosh :: Double -> Double -> Double -> Double
foreign import ccall unsafe "erf_atanh" erf_atanh :: Double -> Double -> Double -> Double

foreign import ccall unsafe "erf_atan2" erf_atan2 :: Double -> Double -> Double -> Double -> Double -> Double
