{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector.Induction(
    induction
  , induction'

  , inductionPeano
  , inductionPeano'

  , All, AllPeano
  ) where

import           Data.Geometry.Vector.VectorFamilyPeano (FromPeano, ImplicitPeano(..), SingPeano(..))
import           Data.Proxy
import           Data.Vector.Fixed.Cont (Peano, PeanoNum(..))
import           Data.Vinyl.Core
import           GHC.Exts
import           GHC.TypeLits

--------------------------------------------------------------------------------

-- | Induction principle on natural numbers.
induction                  :: forall proxy c p n.
                              ( All c n
                              , ImplicitPeano (Peano n), n ~ FromPeano (Peano n))
                           => proxy c
                           -> p 0
                           -- ^ the base case
                           -> (forall (m :: Nat). c (1+m) => p m -> p (1+m))
                           -- ^ the step case. Items in the step case should satisfy constraint c
                           -> p n
induction pc baseCase step = runP $ inductionPeano' (Proxy @(LiftC c)) (P baseCase) step' sn
  where
    -- lift 'step' to working on Peano numbers
    step' :: forall (m :: PeanoNum). LiftC c (S m) => (P p) m -> (P p) (S m)
    step' = case conjure pc (Proxy @(S m)) of
              DictOnly -> P . step . runP
    sn :: SingPeano (Peano n)
    sn = implicitPeano

-- | Induction principle for the case that the step does not require
-- any additional constraints.
induction'  :: forall p n. ( ImplicitPeano (Peano n)
                           , n ~ FromPeano (Peano n)
                           , All Unconstrained n
                           )
           => p 0
           -> (forall (m :: Nat). p m -> p (1+m))
           -> p n
induction' = induction (Proxy @Unconstrained)

--------------------------------------------------------------------------------

-- | Induction principle on Peano numbers
inductionPeano                  :: forall proxy c p n. (AllPeano c n,  ImplicitPeano n)
                                => proxy c
                                -> p Z
                                -> (forall (m :: PeanoNum). c (S m) => p m -> p (S m))
                                -> p n
inductionPeano pc baseCase step = inductionPeano' pc baseCase step (implicitPeano @n)

-- | The actual implementation of the induction principle
inductionPeano'                 :: forall proxy c p n. AllPeano c n
                                => proxy c
                                -> p Z
                                -> (forall (m :: PeanoNum). c (S m) => p m -> p (S m))
                                -> SingPeano n -> p n
inductionPeano' _ baseCase step = go
  where
    go :: AllPeano c k => SingPeano k -> p k
    go = \case
      SZ    -> baseCase
      SS sm -> step $ go sm

--------------------------------------------------------------------------------

-- | All numbers [1,n] satisfy the constraint c
type All (c :: Nat -> Constraint) n = AllPeano (LiftC c) (Peano n)

type family AllPeano (c :: PeanoNum -> Constraint) n :: Constraint where
  AllPeano c Z     = ()
  AllPeano c (S n) = (c (S n), AllPeano c n)

--------------------------------------------------------------------------------
-- * Connecting GHC's fancy natural numbers with Peano Numbers doing
--   the actual work.

-- | We essentially use LiftC as partially applied function lifting a 'Nat -> Constraint' into a
-- 'PeanoNum -> Constraint'
class LiftC (c :: Nat -> Constraint) (n :: PeanoNum) where
  -- | Helper function to lift the constraint from Nat to PeanoNum
  conjure :: proxy' c -> proxy n -> DictOnly c (FromPeano n)

instance (c (FromPeano n)) => LiftC c n where
  conjure _ _ = DictOnly

-- | Similary, we lift a 'Nat -> *' into a 'PeanoNum -> *'
newtype P (p :: Nat -> *) (n :: PeanoNum) = P { runP :: p (FromPeano n) }

--------------------------------------------------------------------------------

class Unconstrained (n :: k)
instance Unconstrained n
