{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
module Data.RealNumber.WithSqrt where

import Data.Kind (Constraint)
import Data.Functor.Identity
import Data.Proxy
import Data.Reflection

--------------------------------------------------------------------------------

type Root = Proxy

pattern Root :: forall root. Root root
pattern Root = Proxy

--------------------------------------------------------------------------------

-- | Extends the numeric type 'r' with a square root of a fixed 'r'
-- value that is tracked through the 'root' type.
--
-- pre: the root may not be zero
data WithSqrt root r = WithSqrt !r !r deriving (Functor,Foldable,Traversable)

--------------------------------------------------------------------------------

instance (Show r, Reifies root r) => Show (WithSqrt root r) where
  show (WithSqrt a b) = let r = reflect $ Root @root
                        in concat [show a, " ", show b, "sqrt(",show r,")"]

instance (Eq r, Num r, Reifies root r) => Eq (WithSqrt root r) where
  (WithSqrt a b) == (WithSqrt c d) = let r    = reflect $ Root @root
                                         sq x = x * x
                                     in (sq $ a - c) == (sq r)*(sq $ b + d)

instance (Ord r, Num r, Reifies root r) => Ord (WithSqrt root r) where
  (WithSqrt a b) `compare` (WithSqrt c d) = let r    = reflect $ Root @root
                                                sq x = x * x
                                            in (sq $ a - c) `compare` ((sq r)*(sq $ b + d))

instance (Ord r, Num r, Reifies root r) => Num (WithSqrt root r) where
  (WithSqrt a b) + (WithSqrt c d) = WithSqrt (a + c) (b + d)
  (WithSqrt a b) - (WithSqrt c d) = WithSqrt (a - c) (b - d)
  (WithSqrt a b) * (WithSqrt c d) = let r = reflect $ Root @root
                                    in WithSqrt (a*c + b*d*r) (a*d + b*c)

  fromInteger i = WithSqrt (fromInteger i) 0
  abs x | x < 0     = (-1) * x
        | otherwise = x
  signum x = case x `compare` 0 of
               LT -> -1
               EQ -> 0
               GT -> 1

  negate (WithSqrt a b) = WithSqrt (negate a) (negate b)


instance (Fractional r, Ord r, Reifies root r) => Fractional (WithSqrt root r) where
  x / (WithSqrt c d) = WithSqrt (a/e) (b/e)
    where
      r = reflect $ Root @root
      WithSqrt a b = x * WithSqrt c (-d)
      e = c*c - d*d*r
      -- similar to division with complex conjugate:
      -- we multiply both x and (c + d*sqrt(r)) with (c - d*sqrt(r))
      -- in the divisor we get e + cd*sqrt(r) - cdsqrt(r), hence
      -- the terms having a sqrt(r) cancel out.
  fromRational x = WithSqrt (fromRational x) 0

instance (Ord r, Fractional r, Real r, Reifies root r) => Real (WithSqrt root r) where
  toRational (WithSqrt a b) | b == 0    = toRational a
                            | otherwise = error "WithSqrt.toRational: trying to convert a number with a non-zero sqrt term into a rational!"

instance (Ord r, Fractional r, Reifies root r) => Floating (WithSqrt root r) where
  -- ^ basically the only supported operation is sqrt(root)
  sqrt (WithSqrt a b) | a == r && b == 0 = WithSqrt 0 1
                      | otherwise = error "WithSqrt. sqrt on anything else than the root itself is unsupported"
    where
      r = reflect $ Root @root

  pi  = error "WithSqrt: pi not implemented"
  exp = error "WithSqrt: exp not implemented"
  log = error "WithSqrt: log not implemented"
  sin = error "WithSqrt: sin not implemented"
  cos = error "WithSqrt: cos not implemented"
  asin = error "WithSqrt: asin not implemented"
  acos = error "WithSqrt: acos not implemented"
  atan = error "WithSqrt: atan not implemented"
  sinh = error "WithSqrt: sinh not implemented"
  cosh = error "WithSqrt: cosh not implemented"
  asinh = error "WithSqrt: asinh not implemented"
  acosh = error "WithSqrt: acosh not implemented"
  atanh = error "WithSqrt: atanh not implemented"

--------------------------------------------------------------------------------


-- | Run a computation using our WithRoot type.
--
-- pre: the computation that we run does not return a value still involving sqrt terms
runWithRoot          :: forall proxy (constr :: * -> Constraint) r computation.
                        ( Functor computation, Ord r, Real r, Fractional r
                        , (forall root. constr (WithSqrt root r))
                        )
                     => proxy constr
                     -> r
                     -- ^ a value r
                     -> (forall real. (Floating real, constr real) => computation real)
                     -- ^ a computation that involves computing sqrt(r) terms
                     -> computation r
runWithRoot _ r comp = reify r comp'
  where
    -- we run the computation using our 'WithSqrt root r' type, and then fmap
    -- things dropping into a real r computation.
    comp'   :: forall root. (Reifies root r, constr (WithSqrt root r))
            => Proxy root -> computation r
    comp' _ = fmap (realToFrac @(WithSqrt root r) @r) comp



--------------------------------------------------------------------------------


newtype ConstR a r = ConstR { runConstR :: a }
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

class None a
instance None a

test :: Floating r => r
test = sqrt 2 * sqrt 2

test2 :: forall r. (Floating r, Show r) => ConstR String r
test2 = ConstR $ show (test @r)

test'   :: Double -> Double
test' r = runIdentity $ runWithRoot (Proxy @None) r (Identity test)

test2'   :: Double -> String
test2' r = runConstR $ runWithRoot (Proxy @Show) r test2
