-- https://tel.archives-ouvertes.fr/tel-03116750/document
{-# LANGUAGE BangPatterns #-}
module Data.Double.Shaman where

import Numeric.MathFunctions.Comparison ( addUlps )
-- import Numeric.MathFunctions.Constants

data Shaman = Shaman
  { shamanValue :: {-# UNPACK #-} !Double
  , shamanError :: {-# UNPACK #-} !Double
  } deriving Show

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
  in Shaman z (mkDelta x dx z)

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
